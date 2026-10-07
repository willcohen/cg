;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.runtime.jvm-client
  "The node transport of the JVM client (cg-gui.run.jvm), and the trace
   stream and the dataset layers of the extension, with no vscode dependency.
   It uses node:http, because the undici fetch stops a response that sends
   nothing for 300 s, and a first session load can take minutes."
  (:require ["http" :as http]
            ["string_decoder" :refer [StringDecoder]]
            ["../../../../cg-app/shared/squint/src/cg_gui/run/js.mjs" :as run-js]
            ["../../../../cg-app/shared/squint/src/cg_gui/run/jvm.mjs" :as jvm]))

(def default-port 3000)

(def default-row-limit 5000)

(defn- read-response [^js res resolve]
  (let [chunks #js []]
    (.on res "data" (fn [c] (.push chunks c)))
    (.on res "end"
         (fn []
           (resolve #js {:status (.-statusCode res)
                         :body (try (js/JSON.parse (.toString (js/Buffer.concat chunks) "utf8"))
                                    (catch :default _ nil))})))))

(defn send-to
  "The transport of the JVM client for the backend on `port`. `timeout-ms`,
   or nil for none, stops a request that takes longer."
  [port timeout-ms]
  (fn [method path headers payload]
    (js/Promise.
     (fn [resolve reject]
       (let [body (when payload (js/JSON.stringify payload))
             hs (js/Object.assign #js {"Content-Type" "application/json"} headers)
             _ (when body (aset hs "Content-Length" (js/Buffer.byteLength body)))
             ^js req (http/request #js {:host "localhost" :port port :path path
                                        :method method :headers hs}
                                   (fn [res] (read-response res resolve)))]
         (.on req "error" reject)
         (when timeout-ms
           (.setTimeout req timeout-ms (fn [] (.destroy req (js/Error. "timeout")))))
         (when body (.write req body))
         (.end req))))))

(defn- layer-of [var-name ^js fc]
  #js {:var var-name
       :color run-js/run-layer-color
       :crs (or (aget fc "crs") "EPSG:4326")
       :geojson fc
       :total-rows (aget fc "total-rows")
       :returned-rows (aget fc "returned-rows")
       :visible? true})

(defn ^:async fetch-layers
  "Gives {:layers}, in the order of `vars`, or {:expired true}. opts: :bbox
   and :limit."
  [send sid vars opts]
  (let [rs (await (js/Promise.all (.map vars (fn [v] (jvm/fetch-dataset send sid v opts)))))]
    (if (.some rs (fn [r] (:expired r)))
      {:expired true}
      {:layers (.filter (.map rs (fn [r i] (when-let [fc (:fc r)] (layer-of (aget vars i) fc))))
                        some?)})))

(defn ^:async load-layers
  "Loads `code` in the session, with its project dir `root`, and fetches its
   dataset vars, or `:vars`, as layers. Gives {:layers}, {:expired true} or
   {:error <msg>}."
  [send sid code filename {:keys [vars limit root]}]
  (let [loaded (await (jvm/load-session send sid code filename root))
        listed (when-not (or (:expired loaded) (:error loaded) (seq vars))
                 (await (jvm/dataset-var-names send sid)))]
    (cond
      (or (:expired loaded) (:error loaded)) loaded
      (or (:expired listed) (:error listed)) listed
      :else (await (fetch-layers send sid (or (:names listed) vars) {:limit limit})))))

(defn- frame-event [frame]
  (when (.startsWith frame "data: ")
    (try (js/JSON.parse (.slice frame 6)) (catch :default _ nil))))

(defn sse-events
  "[events tail]: the events of the complete `data:` frames of `buf`, and the
   rest of `buf`, which a later chunk completes. A frame that does not parse
   gives no event."
  [buf]
  (let [frames (.split buf "\n\n")
        tail (.pop frames)]
    #js [(.filter (.map frames frame-event) some?) tail]))

(defn stream-trace
  "Calls (on-event evt) for each trace event of the execution `id`. Resolves
   to the execution-ended event, and rejects when the stream ends before it."
  [port id on-event]
  (js/Promise.
   (fn [resolve reject]
     (let [!buf (atom "")
           !ended (atom false)
           ;; Not setEncoding: with network inspection on, node:inspector
           ;; throws on a chunk that is a string.
           ;; The decoder keeps a character that two chunks split.
           decoder (StringDecoder. "utf8")
           take! (fn [evt]
                   (if (= "execution-ended" (aget evt "kind"))
                     (do (reset! !ended true) (resolve evt))
                     (on-event evt)))
           req (http/get #js {:host "localhost" :port port
                              :path (str "/api/execution/" id "/trace")}
                         (fn [^js res]
                           (.on res "data"
                                (fn [chunk]
                                  (let [[events tail] (sse-events (str @!buf (.write decoder chunk)))]
                                    (reset! !buf tail)
                                    (.forEach events take!))))
                           (.on res "end"
                                (fn []
                                  (when-not @!ended
                                    (reject (js/Error. "the trace stream ended before the execution (backend gone?)")))))))]
       (.on req "error" reject)))))

(defn ^:async run-in-session
  "Runs `req` ({:code :filename :opts}) in the session `sid` of the backend at
   `port`, and calls (on-event evt) for each trace event. Gives {:result},
   {:expired true} or {:error <msg>}."
  [port sid req on-event]
  (let [send (send-to port nil)
        started (await (jvm/start-execution send sid req))]
    (if-let [id (:id started)]
      (let [end (await (stream-trace port id on-event))]
        (if (= "success" (aget end "outcome"))
          {:result (await (jvm/execution-result send id))}
          {:error (or (aget end "message") "the execution failed")}))
      started)))
