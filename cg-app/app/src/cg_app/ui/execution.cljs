;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.execution
  "Runs a flow on the JVM backend (an SSE trace) or the JS backend (in the
   browser). A jvm choice with no backend is refused, with no js fallback."
  (:require [cg-app.ui.state :as state]
            [cg-app.ui.runtime :as rt]
            [cg-app.ui.session :as session]
            ["../../../../shared/squint/src/cg_gui/run/jvm.mjs" :as jvm]
            [cg-app.ui.loader :as loader]
            ["../../../../shared/squint/src/cg_gui/run/js.mjs" :as run-js]
            ["../../../../shared/squint/src/cg_gui/run/lane.mjs" :as lane]))

(defn set-exec! [m]
  (swap! state/app-state assoc :exec m)
  m)

(defn bus-post!
  "Reads window from globalThis, because a bare js/window throws
   ReferenceError in the node test runner."
  [msg]
  (when-let [w (aget js/globalThis "window")]
    (.postMessage w msg "*")))

(defn- normalize-report
  "A report-shaped run result -> {:title :columns :rows}, or nil when it
   cannot normalize. A normalized table passes through."
  [mod r]
  (if (and (some? r) (not (js/Array.isArray r))
           (aget r "columns") (aget r "rows"))
    r
    (when-let [rt-fn (run-js/lookup2 mod "dataset" "report_table")]
      (try (rt-fn r nil) (catch :default _ nil)))))

(defn- add-map-layer!
  "run-js/run-result->layers converts each dataset of a JS run to GeoJSON. A
   JVM run gives GeoJSON already, and `mod` is nil."
  [mod r]
  (doseq [l (run-js/run-result->layers mod r)]
    (state/add-map-layer! (aget l "var") (aget l "color") (aget l "crs")
                          (aget l "geojson") (aget l "spec"))))

(defn route-result!
  "Sends a JS backend result to the map, the results panel or the badge, by
   its shape. A \"collection\" goes nowhere, because the app has no dataset
   grid."
  [mod r]
  (let [results (aget r "results")]
    (case (run-js/result-shape results)
      ("layer" "dataset" "geometry") (add-map-layer! mod r)
      "report" (when-let [t (normalize-report mod results)]
                 (state/set-report-result! t))
      "scalar" (state/set-scalar-result!
                #js {:text (run-js/scalar-text results)
                     :name (or (aget r "name") "result")})
      nil)))

(defn stream-trace!
  "Forwards each SSE trace frame to the canvas. Resolves #js {:count n} at the
   end sentinel, and closes the EventSource, which otherwise reconnects."
  [id]
  (js/Promise.
   (fn [resolve reject]
     (let [es (js/EventSource. (str (rt/backend-url) "/api/execution/" id "/trace"))
           done (atom false)
           finish! (fn [settle v]
                     (when-not @done
                       (reset! done true)
                       (.close es)
                       (settle v)))]
       (set! (.-onmessage es)
             (fn [^js e]
               (when-let [evt (try (js/JSON.parse (.-data e)) (catch :default _ nil))]
                 (if (= "execution-ended" (aget evt "kind"))
                   (if (= "success" (aget evt "outcome"))
                     (do (bus-post! #js {:type "cg/run-ended"
                                         :outcome "success"
                                         :count (aget evt "count")})
                         (finish! resolve #js {:count (aget evt "count")}))
                     (let [msg (or (aget evt "message") "execution failed")]
                       (bus-post! #js {:type "cg/run-error" :message msg})
                       (finish! reject (js/Error. msg))))
                   (bus-post! #js {:type "cg/trace-event" :event evt})))))
       (set! (.-onerror es)
             (fn [_]
               ;; Only a break in the middle of a run arrives here: the
               ;; sentinel handler closes the EventSource before the normal end
               ;; of the stream.
               (let [msg "trace stream interrupted before the execution ended (backend gone?)"]
                 (bus-post! #js {:type "cg/run-error" :message msg})
                 (finish! reject (js/Error. msg)))))))))

(defn- worker-urls
  "The files that the worker pool loads, or nil when the page does not map
   them."
  []
  (let [doc (aget js/globalThis "document")
        tag (when doc (.querySelector doc "script[src*='wasmts']"))]
    (when tag
      (try #js {"wasmts-js" (.-src tag)
                "proj-db" (loader/asset-url "gdal-wasm/proj.db")}
           (catch :default _ nil)))))

(defn ^:async execute-jvm!
  "Runs the flow in the session of the app on the JVM, so that its datasets
   stay there, streams the trace to the canvas and adds the layers of the
   result to the map. Returns the result, or throws on a 409 or a backend or
   stream error."
  [text flow model-name file-name]
  (let [mname (or model-name (run-js/model-binding-name flow))
        req #js {:code text}
        _ (when mname (aset req "opts" #js {:name mname :model mname}))
        _ (when file-name (aset req "filename" file-name))
        started (await (session/in-session (fn [sid] (jvm/start-execution session/send sid req))))]
    (when-let [e (:error started)]
      (throw (js/Error. e)))
    (let [id (:id started)]
      (bus-post! #js {:type "cg/run-started" :execution-id id})
      (await (stream-trace! id))
      (let [r (await (jvm/execution-result session/send id))]
        (add-map-layer! nil r)
        r))))

(defn ^:async execute!
  "Runs the loaded flow on the backend that lane/execute-route picks. A jvm
   choice with no backend sets status 'refused' and runs nothing."
  []
  (let [st @state/app-state
        text (:text (:loaded-file st))
        flow (:flow st)]
    (cond
      (not (and text flow))
      (set-exec! {:status "error" :runtime nil :message "no .cg file loaded"})

      (not= "loaded" (:module-status st))
      (set-exec! {:status "error" :runtime nil :message "cg module not loaded yet"})

      :else
      (let [route (lane/execute-route (rt/current-choice)
                                    (boolean (:backend-available st)))]
        (if (:refused route)
          (set-exec! {:status "refused" :runtime "jvm" :message (:reason route)})
          (let [lane (:runtime route)]
            (set-exec! {:status "running" :runtime lane :message nil})
            (state/clear-run-results!)
            (try
              (let [r (if (= "jvm" lane)
                        (await (execute-jvm! text flow (:run-model st) (:name (:loaded-file st))))
                        (let [mod (await (loader/load-cg-module!))
                              r (await (run-js/run-js! mod text flow
                                                       {:post bus-post!
                                                        :urls (worker-urls)
                                                        :read-module loader/fetch-module-text
                                                        :model (:run-model st)
                                                        :file (:name (:loaded-file st))}))]
                          (route-result! mod r)
                          (bus-post! #js {:type "cg/run-ended" :outcome "success"
                                          :count (aget r "count")})
                          r))]
                (set-exec! {:status "ok" :runtime lane :count (aget r "count")
                            :message (str (aget r "count") " result(s)")}))
              (catch :default e
                (set-exec! {:status "error" :runtime lane
                            :message (or (.-message e) (str e))})))))))))
