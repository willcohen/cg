;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; The table-> path from a GeoJSON string to a reprojected dataset, on both
;; runtimes. The JVM expands transform-crs into three steps and cljs into one,
;; and the shared assertions hold on both.
(ns cg.dataset-exec-test
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [clojure.string :as str]
                    [cg.dataset :as ds]
                    [cg.feature :as feature]
                    [cg.geo :as geo]
                    [cg.run :as run]
                    [cg.macros :refer [table->]]
                    [cg.test-runner :refer [after error-message]])
     :cljs (:require [cljs.test :refer [deftest is]]
                     [clojure.string :as str]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["../../src/cg/run.mjs" :as run]
                     ["./pool_setup.mjs" :refer [init_cg_pool_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_ after error-message]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- wgs84-geojson
  "A FeatureCollection of points in EPSG:4326. A string, because the JS to
   CLJS boundary for a column map is a separate problem from this one."
  [points]
  (str "{\"type\":\"FeatureCollection\",\"crs\":\"EPSG:4326\",\"features\":["
       (str/join
        ","
        (map-indexed
         (fn [i p]
           (str "{\"type\":\"Feature\",\"properties\":{\"id\":" (inc i) "},"
                "\"geometry\":{\"type\":\"Point\",\"coordinates\":["
                (nth p 0) "," (nth p 1) "]}}"))
         points))
       "]}"))

(def ^:private three-points
  [[-72.0 42.0] [-74.0 40.5] [-77.0 39.0]])

;; Inside the area of EPSG:2249, the target CRS of the flow.
(def ^:private two-points
  [[-72.0 42.0] [-72.01 42.01]])

(defn- transform-crs-model []
  #?(:clj (table-> :parcels (ds/transform-crs "EPSG:4326" "EPSG:2249"))
     :cljs (run/eval-cg (str "(require '[cg.macros :refer [table->]])"
                             "(require '[cg.dataset :as ds])"
                             "(table-> :parcels"
                             " (ds/transform-crs \"EPSG:4326\" \"EPSG:2249\"))"))))

(defn- run-flow [parcels]
  (run/run-model (transform-crs-model) {:parcels parcels} {:timeout-ms 30000}))

(defn- field-strs
  "A JVM model carries keywords and symbols where a cljs model carries
   strings."
  [xs field]
  (mapv (fn [x] (str (get x field))) xs))

(defn- has-substring? [strings sub]
  (some? (first (filter (fn [s] (str/includes? s sub)) strings))))

(defn- result-dataset
  "The dataset a one-sink flow produces. Each runtime wraps it in its own
   sequence type."
  [result]
  #?(:clj (if (sequential? result) (first result) result)
     :cljs (if (array? result) (nth result 0) result)))

(defn- check-reprojected! [result]
  (let [d (result-dataset result)]
    (is (= 2 (ds/row-count d)) "the row count survives the reprojection")
    (let [geoms (ds/column d :geometry)
          g     (nth geoms 0)]
      ;; EPSG:2249 is in US feet: x and y are in the hundreds of thousands.
      (is (> (abs (geo/x g)) 1000.0) "x is in projected units")
      (is (> (abs (geo/y g)) 1000.0) "y is in projected units")
      ;; layer/get-dataset-crs reads it, so a second reproject-to does not
      ;; reproject again.
      (is (every? #(= "EPSG:2249" (feature/get-crs %)) geoms)))))

(deftest geojson-string-builds-a-dataset
  (let [d (ds/geojson->dataset (wgs84-geojson three-points))]
    (is (= 3 (ds/row-count d)) "the FeatureCollection gives 3 rows")
    (is (ds/has-column? d :geometry) "the dataset has a geometry column")))

(deftest table-model-names-the-transform-crs-group
  (is (has-substring? (field-strs (:nodes (transform-crs-model)) :fn) "transform-crs")
      "the model names the transform-crs group"))

(deftest expansion-produces-a-connected-chain
  (let [expanded (run/expand-model (transform-crs-model))
        types    (field-strs (:nodes expanded) :type)
        fns      (field-strs (:nodes expanded) :fn)
        ids      (field-strs (:nodes expanded) :id)
        froms    (field-strs (:edges expanded) :from)
        tos      (field-strs (:edges expanded) :to)]
    ;; A group node that survives expansion never reaches the executor.
    (is (not (has-substring? types "group")) "expansion removes every group node")
    ;; Each runtime names its own steps. Both must carry the transform.
    (is (or (has-substring? fns "transform-batch-step")
            (has-substring? fns "transform-coords"))
        "the chain carries the transform step")
    ;; An edge that points at a missing node silently drops the branch.
    (let [real (set ids)
          to-set (set tos)
          next-of (zipmap froms tos)
          path (take-while some? (iterate #(get next-of %)
                                          (first (remove #(contains? to-set %) ids))))]
      (is (every? #(contains? real %) (concat froms tos))
          "each edge points at a real node")
      (is (= real (set path)) "the edges make one path through each node"))))

;; The cljs reprojection runs on the joint pool: the proj handler and the
;; wasmts handler are on the same worker, so the coordinates do not cross a
;; worker boundary. The footer starts the pool before the tests run.
#?(:cljs
   (defn ^:async setup! []
     (await (init_cg_pool_BANG_ {}))
     (await (ds/init-arquero!))))

(deftest table-flow-reprojects-4326-to-2249
  (after (run-flow (ds/geojson->dataset (wgs84-geojson two-points))) check-reprojected!))

;; arquero gives a table with no columns 0 rows, so a dataset with only the
;; geometry column once failed in the step that puts the column back.
(defn- geometry-only-dataset []
  (ds/select-columns (ds/geojson->dataset (wgs84-geojson two-points)) [:geometry]))

(deftest table-flow-reprojects-a-geometry-only-dataset
  (after (run-flow (geometry-only-dataset)) check-reprojected!))

;; A geometry with a z takes the WasmTS path of the workers on the JS runtime.
(def ^:private three-d-geojson
  (str "{\"type\":\"FeatureCollection\",\"crs\":\"EPSG:4326\",\"features\":["
       "{\"type\":\"Feature\",\"properties\":{\"id\":1},"
       "\"geometry\":{\"type\":\"LineString\",\"coordinates\":"
       "[[-72.0,42.0,12.5],[-72.01,42.01,30.0]]}}]}"))

(defn- check-reprojected-z! [result]
  (let [g (nth (ds/column (result-dataset result) :geometry) 0)
        coords (feature/coordinates g)]
    (is (> (abs (first (first coords))) 1000.0) "x is in projected units")
    (is (= [12.5 30.0] (mapv (fn [c] (nth c 2)) coords)) "z passes through")
    (is (= "EPSG:2249" (feature/get-crs g)))))

(deftest table-flow-reprojects-a-geometry-with-z
  (after (run-flow (ds/geojson->dataset three-d-geojson)) check-reprojected-z!))

;; A transform-crs flow reprojects only the :geometry column, and it must
;; throw on a geometry column with another name, not pass it through.
(defn- misnamed-geometry-dataset []
  (let [d (ds/geojson->dataset (wgs84-geojson two-points))]
    #?(:clj (ds/rename-columns d {:geometry "geometry"})
       :cljs (ds/rename-columns d {"geometry" "shape"}))))

(defn- check-names-the-columns! [msg]
  (is (some? msg) "the flow throws")
  (when msg
    (is (str/includes? msg "no geometry column") msg)
    (is (str/includes? msg #?(:clj "\"geometry\"" :cljs "shape")) msg)))

(deftest transform-crs-throws-without-a-geometry-column
  (after (error-message #(run-flow (misnamed-geometry-dataset))) check-names-the-columns!))

;; A layer from a city server can hold a row with no geometry.
(defn- nil-geometry-dataset []
  (let [d (ds/geojson->dataset (wgs84-geojson two-points))]
    (ds/->dataset (conj (vec (ds/rows d :as-maps)) {:id 3 :geometry nil}))))

(defn- check-nil-geometry-stays! [result]
  (let [geoms (vec (ds/column (result-dataset result) :geometry))]
    (is (= 3 (count geoms)) "the row with no geometry stays")
    (is (nil? (nth geoms 2)))
    (is (> (abs (geo/x (nth geoms 0))) 1000.0) "the other rows are reprojected")))

(deftest transform-crs-passes-a-nil-geometry-through
  (after (run-flow (nil-geometry-dataset)) check-nil-geometry-stays!))

;; The JS hosts start PROJ with the joint pool of run/init-cg-workers!, not
;; with geo/init-proj!.
(deftest geo-reprojects-on-the-joint-pool
  (-> (after (geo/transform-geom (feature/point -71.06 42.36) "EPSG:4326" "EPSG:2249")
             (fn [g]
               (is (> (abs (geo/x g)) 1000.0) "x is in projected units")
               (geo/crs->projjson "EPSG:2249")))
      (after (fn [pj] (is (str/starts-with? pj "{"))))))

;; The parser accepts ds/concat and ds/filter-by-column, so each name must
;; find a fn at run time.
(def ^:private ds-prelude
  "(require '[cg.macros :refer [table->]])
   (require '[cg.dataset :as ds])
   ")

(defn- ids [d]
  (mapv (fn [r] (get r :id)) (ds/rows d :as-maps)))

(deftest ds-concat-runs-in-plain-code
  (is (= [1 2] (ids (run/eval-cg
                     (str ds-prelude
                          "(ds/concat (ds/maps->dataset [{:id 1}])"
                          " (ds/maps->dataset [{:id 2}]))"))))))

(deftest ds-filter-by-column-runs-in-plain-code
  (is (= [2] (ids (run/eval-cg
                   (str ds-prelude
                        "(ds/filter-by-column (ds/maps->dataset [{:id 1 :area 500}"
                        " {:id 2 :area 2000}]) :area :gt 1000)"))))))

(deftest ds-filter-by-column-runs-as-a-rung
  (after (run/run-model (run/eval-cg (str ds-prelude
                                          "(table-> :rows (ds/filter-by-column :area :gt 1000))"))
                        {:rows (ds/maps->dataset [{:id 1 :area 500} {:id 2 :area 2000}])}
                        {:timeout-ms 30000})
         (fn [result] (is (= [2] (ids (result-dataset result)))))))

#?(:cljs (-> (setup!)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.dataset-exec-test")))))
