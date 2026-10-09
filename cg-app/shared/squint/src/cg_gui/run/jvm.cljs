;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run.jvm
  "The client of the JVM backend, for both hosts. Each call takes `send`, the
   transport of the host: (send method path headers body) gives a Promise of
   #js {:status :body}, with the body parsed from JSON or nil, and rejects
   only on a transport error. A session call gives its value, {:expired true}
   on a 404, because the session expired, or {:error <msg>}.")

(defn- session-headers [sid] #js {"X-Session-Id" sid})

(defn- outcome
  "(ok-fn body) of a 2xx response `res`, {:expired true} of a 404, else
   {:error}, with the message of the body when it has one."
  [^js res label ok-fn]
  (let [status (aget res "status")
        body (aget res "body")]
    (cond
      (and (>= status 200) (< status 300)) (ok-fn body)
      (= 404 status) {:expired true}
      :else {:error (or (some-> body (aget "error")) (str label " HTTP " status))})))

(defn ^:async health? [send]
  (try (= 200 (aget (await (send "GET" "/api/health" nil nil)) "status"))
       (catch :default _ false)))

(defn ^:async create-session
  "The id of a new session. Throws when the backend gives none."
  [send]
  (let [res (await (send "POST" "/api/session/create" nil nil))]
    (or (some-> (aget res "body") (aget "session-id"))
        (throw (js/Error. (str "session/create HTTP " (aget res "status")))))))

(defn ^:async load-session
  "Evaluates `code` in the session. {:ok vars}. `root`, when given, is
   the project dir that the relative module paths of `code` resolve against."
  [send sid code filename root]
  (outcome (await (send "POST" "/api/session/load" (session-headers sid)
                        #js {:code code :filename filename :root root}))
           "session/load"
           (fn [b] {:ok (some-> b (aget "vars"))})))

(defn- dataset-names [vars]
  (.map (.filter (or vars #js []) (fn [v] (= "dataset" (aget v "type"))))
        (fn [v] (aget v "name"))))

(defn ^:async dataset-var-names
  "{:names} of the dataset vars of the session, with or without geometry."
  [send sid]
  (outcome (await (send "GET" "/api/session/vars" (session-headers sid) nil))
           "session/vars"
           (fn [b] {:names (dataset-names (some-> b (aget "vars")))})))

(defn ^:async fetch-dataset
  "{:fc} of the dataset var `var-name` as GeoJSON in EPSG:4326. opts: :bbox
   (in EPSG:4326) and :limit (rows)."
  [send sid var-name {:keys [bbox limit]}]
  (let [payload #js {:limit limit}]
    (when bbox (aset payload "bbox" bbox))
    (outcome (await (send "POST" (str "/api/data/" var-name) (session-headers sid) payload))
             (str "data/" var-name)
             (fn [b] {:fc b}))))

(defn ^:async start-execution
  "{:id} of a run of `req` (#js {:code :filename :opts}) in the session. A run
   while another one streams gets {:error}, because trace events are global."
  [send sid req]
  (let [res (await (send "POST" "/api/execution/start" (session-headers sid) req))
        live (some-> (aget res "body") (aget "live-execution-id"))
        r (outcome res "execution/start" (fn [b] {:id (aget b "execution-id")}))]
    (if (and (:error r) live)
      {:error (str (:error r) " (live execution: " live ")")}
      r)))

(defn ^:async execution-result
  "The result of the execution `id`: #js {:count :name :results :layers}.
   Throws when the backend has none."
  [send id]
  (let [res (await (send "GET" (str "/api/execution/" id "/result") nil nil))]
    (when-not (= 200 (aget res "status"))
      (throw (js/Error. (str "execution/result HTTP " (aget res "status")))))
    (aget (aget res "body") "result")))

(defn ^:async start-repl
  "{:port} of the nREPL of the session, which the backend starts one time."
  [send sid]
  (outcome (await (send "POST" "/api/session/repl" (session-headers sid) nil))
           "session/repl"
           (fn [b] {:port (aget b "port")})))

(defn ^:async with-session
  "(f sid) with the session in the atom `!sid`, made when there is none, and
   again with a new session when `f` gives {:expired true}."
  [send !sid f]
  (when (nil? @!sid)
    (reset! !sid (await (create-session send))))
  (let [r (await (f @!sid))]
    (if (:expired r)
      (do (reset! !sid (await (create-session send)))
          (await (f @!sid)))
      r)))
