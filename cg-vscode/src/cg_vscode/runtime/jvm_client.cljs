;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.runtime.jvm-client
  "HTTP client for the JVM REST backend, with no vscode dependency. session/load
   uses node:http, because the undici fetch stops at 300 s with no headers."
  (:require ["http" :as http]
            ["../../../../cg-app/shared/squint/src/cg_gui/run/js.mjs" :as run-js]))

(def default-port 3000)

(def default-row-limit 5000)

(defn- jvm-base-url [port]
  (str "http://localhost:" port))

(defn- post-json-no-timeout
  "POSTs JSON over node:http with no response timeout. Returns
   Promise<#js {:status :body}>, and rejects only on a transport error."
  [port path extra-headers payload]
  (js/Promise.
   (fn [resolve reject]
     (let [body (js/JSON.stringify payload)
           headers (js/Object.assign
                    #js {"Content-Type" "application/json"
                         "Content-Length" (js/Buffer.byteLength body)}
                    (or extra-headers #js {}))
           req (http/request
                #js {:host "localhost" :port port :path path
                     :method "POST" :headers headers}
                (fn [^js res]
                  (let [chunks #js []]
                    (.on res "data" (fn [c] (.push chunks c)))
                    (.on res "end"
                         (fn []
                           (let [txt (.toString (js/Buffer.concat chunks) "utf8")
                                 parsed (try (js/JSON.parse txt)
                                             (catch :default _ nil))]
                             (resolve #js {:status (.-statusCode res)
                                           :body parsed})))))))]
       (.on req "error" (fn [e] (reject e)))
       (.write req body)
       (.end req)))))

(defn create-session [port]
  (-> (js/fetch (str (jvm-base-url port) "/api/session/create")
                #js {:method "POST"})
      (.then (fn [^js res]
               (if (.-ok res)
                 (.json res)
                 (-> (.text res)
                     (.then (fn [t]
                              (throw (js/Error.
                                      (str "session/create failed: "
                                           (.-status res) " " t)))))))))
      (.then (fn [body]
               (or (aget body "session-id")
                   (throw (js/Error. "session/create returned no session-id")))))))

(defn- load-cg [port session-id code filename]
  (-> (post-json-no-timeout port "/api/session/load"
                            #js {"X-Session-Id" session-id}
                            #js {:code code :filename filename})
      (.then (fn [^js res]
               (let [status (aget res "status")
                     body (aget res "body")]
                 (if (and (>= status 200) (< status 300))
                   body
                   (let [err (or (when body (aget body "error")) status)]
                     (throw (js/Error.
                             (str "session/load failed: "
                                  status " " err))))))))))

(defn eval-cg
  "Evaluates `text` in a new session on the backend at `port`. Returns a
   Promise of {:result <body>} or {:error <message>}."
  [port text filename]
  (-> (create-session port)
      (.then (fn [sid] (load-cg port sid text filename)))
      (.then (fn [body]
               (if (:error body)
                 {:error (:error body)}
                 {:result body})))
      (.catch (fn [err] {:error (.-message err)}))))

;; The caller owns the session, because it must stay alive across load, data
;; fetches and new runs.

(defn ^:async load-session!
  "Runs the .cg in the live session. Returns {:ok <vars>}, {:expired true} on
   404, or {:error <msg>}."
  [port session-id code filename]
  (try
    (let [res (await (post-json-no-timeout port "/api/session/load"
                                           #js {"X-Session-Id" session-id}
                                           #js {:code code :filename filename}))
          status (aget res "status")
          b (aget res "body")]
      (cond
        (and (>= status 200) (< status 300)) {:ok (when b (aget b "vars"))}
        (= 404 status) {:expired true}
        :else {:error (or (when b (aget b "error"))
                          (str "session/load HTTP " status))}))
    (catch :default e
      {:error (str "session/load failed: " (.-message e))})))

(defn ^:async fetch-layer
  "The dataset of `var` as a GeoJSON layer. Returns {:layer}, {:expired true}
   on 404, or {:skip <var> :reason} on another error, such as a missing var."
  [port session-id var {:keys [bbox limit color]}]
  (let [payload #js {:limit (or limit default-row-limit)}
        _ (when bbox (aset payload "bbox" bbox))
        res (await (js/fetch (str (jvm-base-url port) "/api/data/" var)
                             #js {:method "POST"
                                  :headers #js {"Content-Type" "application/json"
                                                "X-Session-Id" session-id}
                                  :body (js/JSON.stringify payload)}))]
    (if (.-ok res)
      (let [fc (await (.json res))]
        {:layer #js {:var var
                     :color (or color run-js/run-layer-color)
                     :crs (or (aget fc "crs") "EPSG:4326")
                     :geojson fc
                     :total-rows (aget fc "total-rows")
                     :returned-rows (aget fc "returned-rows")
                     :visible? true}})
      (if (= 404 (.-status res))
        {:expired true}
        (let [b (await (.catch (.json res) (fn [_] #js {})))]
          {:skip var :reason (or (aget b "error") (str "HTTP " (.-status res)))})))))

(defn ^:async fetch-layers-for
  "Fetches all `vars` at the same time into `layers` and `warnings`, in the
   order of `vars`. Returns {:layers :warnings}, or {:expired true} on a 404."
  [port session-id vars opts layers warnings]
  (let [results (await (js/Promise.all
                        (.map vars (fn [v] (fetch-layer port session-id v opts)))))]
    (if (.some results (fn [r] (:expired r)))
      {:expired true}
      (do (.forEach results
                    (fn [r]
                      (if (:skip r)
                        (.push warnings (str (:skip r) ": " (:reason r)))
                        (.push layers (:layer r)))))
          {:layers layers :warnings warnings}))))

(defn ^:async dataset-var-names
  "The names of all dataset vars, with or without geometry. Returns {:names},
   {:expired true} or {:error <msg>}."
  [port session-id]
  (let [res (await (js/fetch (str (jvm-base-url port) "/api/session/vars")
                             #js {:method "GET"
                                  :headers #js {"X-Session-Id" session-id}}))]
    (if (.-ok res)
      (let [b (await (.json res))
            vars (or (aget b "vars") #js [])
            names #js []]
        (.forEach vars (fn [v] (when (= "dataset" (aget v "type"))
                                 (.push names (aget v "name")))))
        {:names names})
      (if (= 404 (.-status res))
        {:expired true}
        (let [b (await (.catch (.json res) (fn [_] #js {})))]
          {:error (or (aget b "error") (str "session/vars HTTP " (.-status res)))})))))

(defn ^:async run->layers
  "Loads `code` into the session and fetches the geometry vars, or `:vars`, as
   layers. Returns {:layers :warnings}, {:expired true} or {:error <msg>}."
  [port session-id code filename {:keys [vars bbox limit]}]
  (let [loaded (await (load-session! port session-id code filename))]
    (cond
      (:expired loaded) {:expired true}
      (:error loaded)   {:error (:error loaded)}
      :else
      (let [explicit? (and vars (pos? (count vars)))
            disc (when-not explicit? (await (dataset-var-names port session-id)))]
        (if (:expired disc)
          {:expired true}
          (let [names (if explicit? vars (or (:names disc) #js []))]
            (await (fetch-layers-for port session-id names
                                     {:bbox bbox :limit limit} #js [] #js []))))))))

(defn ^:async refetch-layers
  "Fetches the `vars` of a loaded session again for a new `bbox`, with no new
   run. Returns {:layers :warnings} or {:expired true}."
  [port session-id vars {:keys [bbox limit]}]
  (await (fetch-layers-for port session-id vars
                           {:bbox bbox :limit limit} #js [] #js [])))
