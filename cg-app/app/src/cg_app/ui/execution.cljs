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
            [cg-app.ui.loader :as loader]
            ["../../../../shared/squint/src/cg_gui/run/js.mjs" :as run-js]
            ["../../../../shared/squint/src/cg_gui/run/lane.mjs" :as lane]))

(defn set-exec! [m]
  (swap! state/app-state assoc :exec m)
  m)

(defn- bus-post!
  "Posts `msg` on the window bus of the canvas. Reads window from globalThis,
   because a bare js/window throws ReferenceError in the node test runner."
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
  "Adds the layers of a JS backend run to the map-layer table, keyed by layer
   name. run-js/run-result->layers converts each dataset to GeoJSON."
  [mod r]
  (doseq [l (run-js/run-result->layers mod r)]
    (state/add-map-layer! (aget l "var") (aget l "color") (aget l "crs")
                          (aget l "geojson") (aget l "spec"))))

(defn route-result!
  "Sends a JS backend result to the map, the results panel or the badge, by
   its shape. \"rows\" goes nowhere, because the app has no dataset grid."
  [mod r]
  (let [results (aget r "results")]
    (case (run-js/result-shape results)
      "layer"    (add-map-layer! mod r)
      "report"   (when-let [t (normalize-report mod results)]
                   (state/set-report-result! t))
      "dataset"  (add-map-layer! mod r)
      "geometry" (add-map-layer! mod r)
      "scalar"   (state/set-scalar-result!
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

(defn ^:async fetch-module-text
  "The text of the .cg module at `path`, from loader/module-url. The path
   resolves against the cg root, as the JVM does."
  [path]
  (let [resp (await (js/fetch (loader/module-url path)))]
    (when-not (.-ok resp)
      (throw (js/Error. (str "load-module " path ": HTTP " (.-status resp)))))
    (await (.text resp))))

(def max-workers
  "Each worker holds its own WasmTS, PROJ and GDAL heap. More workers
   cost memory that a browser tab does not have."
  4)

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

(defonce !workers (atom nil))

(defn- post-stage!
  "Posts a stage trace event on the bus, in the shape that cg emits."
  [label state]
  (bus-post! #js {:type "cg/trace-event"
                  :event #js {:kind "stage" :label label :state state
                              :ts (js/Date.now)}}))

(defn- ios?
  "True on an iPhone or an iPad. An iPadOS Safari tab names itself Macintosh."
  []
  (let [nav (aget js/globalThis "navigator")
        ua (or (aget nav "userAgent") "")]
    (or (.test #"iPhone|iPad|iPod" ua)
        (and (.includes ua "Macintosh") (> (or (aget nav "maxTouchPoints") 0) 1)))))

(defn ^:async ensure-workers!
  "Starts the worker pool on the first run. A failure is only a console
   warning, because the files that need no pool still run."
  [mod]
  (when (nil? @!workers)
    (let [urls (worker-urls)
          cores (or (aget (aget js/globalThis "navigator") "hardwareConcurrency") 2)]
      (when urls (post-stage! "start worker pool" "start"))
      (reset! !workers
              (if urls
                ;; Each worker holds its own wasm heaps (about 210 MB), and iOS Safari
                ;; reloads a tab above about 1.5 GB.
                (try (await (run-js/ensure-workers! mod urls (if (ios?) 1 (min max-workers cores))))
                     (catch :default e
                       (js/console.warn "cg worker pool did not start:" e)
                       false))
                false))
      (when urls (post-stage! "start worker pool" "done"))))
  @!workers)

(defn ^:async execute-jvm!
  "Runs the flow on the JVM and streams the trace to the canvas. Returns
   #js {:count n}, or throws on a 409 or a backend or stream error."
  [text flow model-name]
  (let [mname (or model-name (run-js/model-binding-name flow))
        req #js {:code text}
        _ (when mname (aset req "opts" #js {:name mname :model mname}))
        resp (await (js/fetch (str (rt/backend-url) "/api/execution/start")
                              #js {:method "POST"
                                   :headers #js {"Content-Type" "application/json"}
                                   :body (js/JSON.stringify req)}))
        body (try (await (.json resp)) (catch :default _ #js {}))]
    (when-not (.-ok resp)
      (throw (js/Error.
              (if (= 409 (.-status resp))
                (str (or (aget body "error") "another execution is already streaming")
                     " (live execution: " (aget body "live-execution-id") ")")
                (or (aget body "error") (str "HTTP " (.-status resp)))))))
    (let [id (aget body "execution-id")]
      (bus-post! #js {:type "cg/run-started" :execution-id id})
      (await (stream-trace! id)))))

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
                        (await (execute-jvm! text flow (:run-model st)))
                        (let [mod (await (loader/load-cg-module!))]
                          (await (run-js/ensure-wasmts! nil))
                          (await (run-js/ensure-arquero! mod))
                          (bus-post! #js {:type "cg/run-started"})
                          (try
                            (await (ensure-workers! mod))
                            (let [r (await (run-js/execute-js!
                                            mod text flow
                                            (fn [evt]
                                              (bus-post! #js {:type "cg/trace-event" :event evt}))
                                            fetch-module-text
                                            (:run-model st)
                                            (:name (:loaded-file st))))]
                              (route-result! mod r)
                              (bus-post! #js {:type "cg/run-ended"
                                              :outcome "success"
                                              :count (aget r "count")})
                              r)
                            (catch :default e
                              (bus-post! #js {:type "cg/run-error"
                                              :message (or (.-message e) (str e))})
                              (throw e)))))]
                (set-exec! {:status "ok" :runtime lane :count (aget r "count")
                            :message (or (aget r "message") (str (aget r "count") " result(s)"))}))
              (catch :default e
                (set-exec! {:status "error" :runtime lane
                            :message (or (.-message e) (str e))})))))))))
