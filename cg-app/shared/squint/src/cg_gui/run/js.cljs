;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run.js
  "The cg JS backend, shared by cg-app and the cg-vscode map webview. Results are
   plain JS objects, because a JS caller reads them with property access.")

(def wasmts-ready-timeout-ms 20000)

(defn- sleep [ms] (js/Promise. (fn [res _] (js/setTimeout res ms))))

(defn lookup2
  "mod.<fname>, else mod.<nsname>.<fname>: the export lookup order of the cg module."
  [mod nsname fname]
  (or (aget mod fname)
      (when-let [n (aget mod nsname)] (aget n fname))))

(defn- metadata-mod [mod] (when mod (aget mod "metadata")))

(defn ^:async ensure-wasmts!
  "Waits for the globalThis.wasmts global of a host script tag. Resolves true or
   throws after the timeout. Call it with nil; the recursion passes the deadline."
  [deadline]
  (let [d (or deadline (+ (js/Date.now) wasmts-ready-timeout-ms))
        w (aget js/globalThis "wasmts")]
    (if (and w (aget w "geom"))
      true
      (if (> (js/Date.now) d)
        (throw (js/Error. "wasmts global never became ready (script tag missing or blocked?)"))
        (do (await (sleep 100))
            (await (ensure-wasmts! d)))))))

(defn ^:async ensure-arquero!
  "Initializes Arquero in the module (dataset init-arquero!), which dataset ops
   need. Idempotent. Does nothing for a module without the export."
  [mod]
  (when-let [init (lookup2 mod "dataset" "init_arquero_BANG_")]
    (await (init))))

(defn ^:async ensure-workers!
  "Starts the worker pool of cg one time. Gives true, or false when the cg module
   has no pool. `urls` has wasmts-js and proj-db."
  [mod urls size]
  (if-let [init (lookup2 mod "run" "init_cg_workers_BANG_")]
    (let [resp (await (js/fetch (aget urls "proj-db")))
          _ (when-not (.-ok resp)
              (throw (js/Error. (str "proj.db: HTTP " (.-status resp)))))
          db (js/Uint8Array. (await (.arrayBuffer resp)))]
      (await (init #js {:size size
                        "wasmts-js-url" (aget urls "wasmts-js")
                        "gdal-init" #js {:dbBytes db}}))
      true)
    false))

(defn- ios?
  "True on an iPhone or an iPad. An iPadOS Safari tab names itself Macintosh."
  [nav]
  (let [ua (or (some-> nav (aget "userAgent")) "")]
    (or (.test #"iPhone|iPad|iPod" ua)
        (and (.includes ua "Macintosh") (> (or (some-> nav (aget "maxTouchPoints")) 0) 1)))))

(defn pool-size
  "The worker count for the navigator `nav`. Each worker holds its own WasmTS,
   PROJ and GDAL heap (about 210 MB), and iOS Safari reloads a tab above
   about 1.5 GB."
  [nav]
  (if (ios? nav)
    1
    (min 4 (or (some-> nav (aget "hardwareConcurrency")) 2))))

(defonce ^:private !pool (atom nil))

(defn- post-pool-stage! [post state]
  (post #js {:type "cg/trace-event"
             :event #js {:kind "stage" :label "start worker pool" :state state
                         :ts (js/Date.now)}}))

(defn ^:async ensure-pool!
  "Starts the worker pool on the first run, or never when `urls` is nil. A
   failure is only a console warning, because a file that needs no pool
   still runs."
  [mod urls post]
  (when (nil? @!pool)
    (when urls (post-pool-stage! post "start"))
    (reset! !pool
            (if urls
              (try (await (ensure-workers! mod urls (pool-size (aget js/globalThis "navigator"))))
                   (catch :default e
                     (js/console.warn "cg worker pool did not start:" e)
                     false))
              false))
    (when urls (post-pool-stage! post "done")))
  @!pool)

(defn model-names
  "Names of the threading-macro bindings of the parsed flow, in source order:
   the models that Run can run."
  [flow]
  (->> (or (and flow (aget flow "bindings")) #js [])
       (filter (fn [b] (= "threading_macro" (aget b "body-shape"))))
       (mapv (fn [b] (aget b "name")))))

(defn model-binding-name
  "The name of the last threading-macro binding in the parsed flow: the model
   that Run runs when there is no pick. nil when the file defines none."
  [flow]
  (last (model-names flow)))

(defn- result-count
  "The count of a run result: the length of an array, numRows (a number or a
   fn) of a dataset, 1 for any other non-nil value, 0 for nil."
  [results]
  (cond
    (js/Array.isArray results) (.-length results)
    (some? results) (let [rc (aget results "numRows")]
                      (cond
                        (number? rc) rc
                        (fn? rc) (try (.call rc results) (catch :default _ 1))
                        :else 1))
    :else 0))

(defn ^:async load-modules!
  "Loads each module that `code` names in (run/load-module ...) with `read-module`,
   before the eval, because the eval cannot await a load. No reader does nothing."
  [mod code read-module]
  (let [set-reader (lookup2 mod "run" "set_module_reader_BANG_")
        load-deps (lookup2 mod "run" "load_deps_BANG_")]
    (when (and read-module set-reader load-deps)
      (set-reader read-module)
      (await (load-deps code)))))

(defn ^:async execute-js!
  "Runs `model-name`, or the last threading-macro binding of `flow`, through
   cg.run/run-file. Returns #js {:count :results :name :layers}."
  [mod text flow on-trace read-module model-name file-name]
  (let [md (metadata-mod mod)
        split-fn (and md (aget md "split_code_and_metadata"))
        run-file (lookup2 mod "run" "run_file")
        add-listener (lookup2 mod "trace_js" "add_trace_listener")
        remove-listener (lookup2 mod "trace_js" "remove_trace_listener")
        mname (or model-name (model-binding-name flow))]
    (when-not split-fn (throw (js/Error. "metadata.split_code_and_metadata not found in cg module")))
    (when-not run-file (throw (js/Error. "run_file not found in cg module")))
    (when-not mname (throw (js/Error. "no threading-macro model binding to execute")))
    (let [code (aget (split-fn text) "code")
          _ (await (load-modules! mod code read-module))
          listener (when (and add-listener on-trace)
                     (add-listener (fn [evt] (on-trace evt))))]
      (try
        ;; A map host draws each map-layer model of the file, because a
        ;; file can keep the layers that explain a result in models of
        ;; their own.
        (let [opts #js {:model mname :map-layers true}
              _ (when file-name (aset opts "file" file-name))
              out (await (run-file code opts))
              results (aget out "result")]
          #js {:count (result-count results) :results results :name mname
               :layers (aget out "layers")})
        (finally
          (when (and remove-listener listener) (remove-listener listener)))))))

(defn ^:async run-js!
  "Runs `text` with execute-js! and gives its result. Posts cg/run-started,
   each cg/trace-event and, on a failure, cg/run-error with `post`. opts:
   :post, :urls (of ensure-workers!), :read-module, :model and :file."
  [mod text flow {:keys [post urls read-module model file]}]
  (await (ensure-wasmts! nil))
  (await (ensure-arquero! mod))
  (post #js {:type "cg/run-started"})
  (try
    (await (ensure-pool! mod urls post))
    (await (execute-js! mod text flow
                        (fn [evt] (post #js {:type "cg/trace-event" :event evt}))
                        read-module model file))
    (catch :default e
      (post #js {:type "cg/run-error" :message (or (.-message e) (str e))})
      (throw e))))

(defn- repl-text [x]
  (if (string? x) x (pr-str x)))

(defn- dataset-text [^js v]
  (let [n (aget v "numRows")
        cols (aget v "columnNames")]
    (str "#dataset {:rows " (if (fn? n) (.call n v) n)
         (when (fn? cols) (str " :columns " (pr-str (vec (.call cols v)))))
         "}")))

(defn repl-value-text
  "The REPL text of a value. A dataset gives its size, because its rows can be
   too many to print."
  [v]
  (if (and (some? v) (not (js/Array.isArray v)) (some? (aget v "numRows")))
    (dataset-text v)
    (try (pr-str v) (catch :default _ (str v)))))

(defn ^:async repl-eval!
  "Evaluates the REPL text `code` with cg.run/eval-cg, as a run evaluates a
   .cg, so it sees the defs and the aliases of the runs of the page. Gives #js
   {:value :out}, or #js {:err :out}, and waits for a Promise value. The
   console.log output of the eval goes to :out, and to the console too."
  [mod code read-module]
  (let [log js/console.log
        out #js []]
    (set! js/console.log
          (fn [& xs]
            (.push out (str (.join (.map (to-array xs) repl-text) " ") "\n"))
            (apply log xs)))
    (try
      (await (ensure-arquero! mod))
      (await (load-modules! mod code read-module))
      (let [v (await ((lookup2 mod "run" "eval_cg") code))]
        #js {:value (repl-value-text v) :out (.join out "")})
      (catch :default e
        #js {:err (or (.-message e) (str e)) :out (.join out "")})
      (finally (set! js/console.log log)))))

(defn report-shaped?
  "True when a run result looks like report data: a summarize tallies vector, a
   single tally map, a block-presence result, or a normalized report table."
  [r]
  (boolean
   (or (and (js/Array.isArray r) (pos? (.-length r))
            (some? (aget r 0)) (aget (aget r 0) "cells"))
       (and (some? r) (not (js/Array.isArray r))
            (or (aget r "cells")
                (and (aget r "counts") (aget r "rows"))
                (and (aget r "columns") (aget r "rows")))))))

(defn- numrows-dataset? [r]
  (and (some? r)
       (not (js/Array.isArray r))
       (some? (aget r "numRows"))
       (not (report-shaped? r))))

(defn- geometry-rows?
  "True when a run result is an array of row maps with a geometry column, the
   result shape of a table-> flow."
  [r]
  (and (js/Array.isArray r)
       (pos? (.-length r))
       (some? (aget r 0))
       (not (js/Array.isArray (aget r 0)))
       (some? (aget (aget r 0) "geometry"))))

(defn- primitive?
  "A value with nothing inside it to render as a table or a layer."
  [v]
  (or (string? v) (number? v) (boolean? v)))

(defn- layer-sink? [v]
  (boolean (and (some? v) (not (js/Array.isArray v))
                (aget v "layer") (aget v "geojson"))))

(defn layer-sink-result
  "The map-layer sink's output, out of the sink's collected vector. nil when
   the result is not one."
  [r]
  (cond
    (layer-sink? r) r
    (and (js/Array.isArray r) (= 1 (.-length r)) (layer-sink? (aget r 0))) (aget r 0)
    :else nil))

(defn result-shape
  "Classifies a JS backend result as layer, report, dataset, geometry, scalar,
   collection or empty. A one-element array of a primitive is \"scalar\"."
  [r]
  (cond
    (nil? r) "empty"
    (some? (layer-sink-result r)) "layer"
    (js/Array.isArray r) (cond
                           (zero? (.-length r)) "empty"
                           (report-shaped? r) "report"
                           (geometry-rows? r) "geometry"
                           (and (= 1 (.-length r)) (primitive? (aget r 0))) "scalar"
                           :else "collection")
    (report-shaped? r) "report"
    (numrows-dataset? r) "dataset"
    :else "scalar"))

(defn scalar-value
  "The value a \"scalar\" result carries, out of the sink's collected vector."
  [r]
  (if (js/Array.isArray r) (aget r 0) r))

(defn scalar-text
  "A scalar run result as display text. A primitive prints as itself. Any other
   value prints as JSON."
  [r]
  (let [v (scalar-value r)]
    (if (primitive? v)
      (str v)
      (try (js/JSON.stringify v) (catch :default _ (str v))))))

(defn- run-result->dataset
  "A JS backend result as a cg dataset, or nil when the result is not
   map-bound."
  [mod r]
  (case (result-shape r)
    "dataset"  r
    "geometry" (when-let [maps->ds (lookup2 mod "dataset" "maps__GT_dataset")]
                 (maps->ds r))
    nil))

(def run-layer-color
  "Fill color for a run-result map layer (distinct from the session-var palette)."
  "#e8590c")

(defn- layer-entry
  "A layer-table entry, or nil when the FeatureCollection has no features."
  [vname gj spec]
  (let [n (when gj (.-length (or (aget gj "features") #js [])))]
    (when (and gj (pos? n))
      (let [e #js {:var vname :color run-layer-color :crs "EPSG:4326"
                   :geojson gj :visible? true}]
        (when spec (aset e "spec" spec))
        e))))

(defn run-result->layer
  "A map-bound run result as #js {:var :color :crs :geojson :visible? :spec}, or
   nil. The CRS is EPSG:4326, because a map-bound flow ends in a transform to 4326."
  [mod r]
  (let [results (aget r "results")
        vname (or (aget r "name") "result")]
    (if-let [sink (layer-sink-result results)]
      (layer-entry (or (aget sink "var") vname)
                   (try (js/JSON.parse (aget sink "geojson")) (catch :default _ nil))
                   (aget sink "spec"))
      (when-let [ds (run-result->dataset mod results)]
        (when-let [to-geojson (lookup2 mod "dataset" "dataset__GT_geojson_string")]
          (layer-entry vname
                       (try (js/JSON.parse (to-geojson ds)) (catch :default _ nil))
                       nil))))))

(defn run-result->layers
  "The layers of a run: the layer of each other map-layer model of the file in
   source order, then the layer of the run's model. Each is a run-result->layer."
  [mod r]
  (let [others (.map (or (aget r "layers") #js [])
                     (fn [l] (run-result->layer mod #js {:results (aget l "result")
                                                         :name (aget l "model")})))]
    (.filter (.concat others #js [(run-result->layer mod r)]) some?)))
