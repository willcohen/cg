;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; The table-> path from a GeoJSON string to a reprojected dataset, on both
;; runtimes. The JVM expands transform-crs into three steps and cljs into one,
;; and the shared assertions hold on both.
(ns cg.dataset-exec-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.string :as str]
                    [cg.dataset :as ds]
                    [cg.feature :as feature]
                    [cg.geo :as geo]
                    [cg.run :as run]
                    [cg.macros :refer [table->]])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [clojure.string :as str]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["../../src/cg/run.mjs" :as run]
                     ["./pool_setup.mjs" :refer [init_cg_pool_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

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

(defn- node-field
  "One field of every node, as strings. A JVM model carries keywords and
   symbols where a cljs model carries strings."
  [expanded field]
  (mapv (fn [n] (str (get n field))) (:nodes expanded)))

(defn- edge-field [expanded field]
  (mapv (fn [e] (str (get e field))) (:edges expanded)))

(defn- has-substring? [strings sub]
  (some? (first (filter (fn [s] (str/includes? s sub)) strings))))

(defn- check-wgs84-dataset! [d]
  (is (= 3 (ds/row-count d)) "the FeatureCollection gives 3 rows")
  (is (ds/has-column? d :geometry) "the dataset has a geometry column"))

(defn- check-model-references-transform-crs! [model]
  (let [fns (node-field model :fn)]
    (is (has-substring? fns "transform-crs")
        "the model names the transform-crs group")))

(defn- check-expansion! [expanded]
  (let [types (node-field expanded :type)
        fns   (node-field expanded :fn)
        ids   (node-field expanded :id)
        froms (edge-field expanded :from)
        tos   (edge-field expanded :to)]
    ;; A group node that survives expansion never reaches the executor.
    (is (not (has-substring? types "group")) "expansion removes every group node")
    ;; Each runtime names its own steps. Both must carry the transform.
    (is (or (has-substring? fns "transform-batch-step")
            (has-substring? fns "transform-coords"))
        "the chain carries the transform step")
    ;; An edge that points at a missing node silently drops the branch.
    (doseq [f froms]
      (is (some? (first (filter (fn [i] (= i f)) ids)))
          (str "edge from " f " points at a real node")))
    (doseq [t tos]
      (is (some? (first (filter (fn [i] (= i t)) ids)))
          (str "edge to " t " points at a real node")))))

(defn- result-dataset
  "The dataset a one-sink flow produces. Each runtime wraps it in its own
   sequence type."
  [result]
  #?(:clj (if (sequential? result) (first result) result)
     :cljs (if (array? result) (nth result 0) result)))

(defn- absv [x]
  #?(:clj (Math/abs (double x)) :cljs (js/Math.abs x)))

(defn- check-reprojected! [result]
  (let [d (result-dataset result)]
    (is (= 2 (ds/row-count d)) "the row count survives the reprojection")
    (let [geoms (ds/column d :geometry)
          g     (nth geoms 0)]
      (is (= 2 (count geoms)) "both geometries survive")
      ;; EPSG:2249 is in US feet: x and y are in the hundreds of thousands.
      (is (> (absv (geo/x g)) 1000.0) "x is in projected units")
      (is (> (absv (geo/y g)) 1000.0) "y is in projected units"))))

(deftest geojson-string-builds-a-dataset
  (testing "geojson->dataset gives rows and a geometry column"
    (check-wgs84-dataset! (ds/geojson->dataset (wgs84-geojson three-points)))))

(deftest table-model-names-the-transform-crs-group
  (testing "the table-> model carries the transform-crs operation"
    (check-model-references-transform-crs! (transform-crs-model))))

(deftest expansion-produces-a-connected-chain
  (testing "expand-model leaves a connected chain with no group node"
    (check-expansion! (run/expand-model (transform-crs-model)))))

;; The cljs reprojection runs on the joint pool: the proj handler and the
;; wasmts handler are on the same worker, so the coordinates do not cross a
;; worker boundary. The footer starts the pool before the tests run.
#?(:cljs
   (defn ^:async setup!
     "Start the joint pool and arquero before any test body runs."
     []
     (await (init_cg_pool_BANG_ {}))
     (await (ds/init-arquero!))))

#?(:cljs
   (defn ^:async run-reproject! []
     (let [d (ds/geojson->dataset (wgs84-geojson two-points))
           result (await (run/run-model (transform-crs-model)
                                        {:parcels d}
                                        {:timeout-ms 30000}))]
       (check-reprojected! result))))

(deftest table-flow-reprojects-4326-to-2249
  (testing "a table-> transform-crs flow reprojects the geometry column"
    #?(:clj (let [d (ds/geojson->dataset (wgs84-geojson two-points))]
              (check-reprojected!
               (run/run-model (transform-crs-model) {:parcels d}
                              {:timeout-ms 30000})))
       :cljs (run-reproject!))))

;; arquero gives a table with no columns 0 rows, so a dataset with only the
;; geometry column once failed in the step that puts the column back.
(defn- geometry-only-dataset []
  (ds/select-columns (ds/geojson->dataset (wgs84-geojson two-points)) [:geometry]))

#?(:cljs
   (defn ^:async run-geometry-only-reproject! []
     (check-reprojected!
      (await (run/run-model (transform-crs-model)
                            {:parcels (geometry-only-dataset)}
                            {:timeout-ms 30000})))))

(deftest table-flow-reprojects-a-geometry-only-dataset
  (testing "a dataset whose only column is the geometry reprojects"
    #?(:clj (check-reprojected!
             (run/run-model (transform-crs-model) {:parcels (geometry-only-dataset)}
                            {:timeout-ms 30000}))
       :cljs (run-geometry-only-reproject!))))

;; A geometry with a z takes the WasmTS path of the workers on the JS runtime.
(def ^:private three-d-geojson
  (str "{\"type\":\"FeatureCollection\",\"crs\":\"EPSG:4326\",\"features\":["
       "{\"type\":\"Feature\",\"properties\":{\"id\":1},"
       "\"geometry\":{\"type\":\"LineString\",\"coordinates\":"
       "[[-72.0,42.0,12.5],[-72.01,42.01,30.0]]}}]}"))

(defn- check-reprojected-z! [result]
  (let [coords (feature/coordinates (nth (ds/column (result-dataset result) :geometry) 0))]
    (is (= 2 (count coords)) "both vertices survive")
    (is (> (absv (first (first coords))) 1000.0) "x is in projected units")
    (is (= [12.5 30.0] (mapv (fn [c] (nth c 2)) coords)) "z passes through")))

#?(:cljs
   (defn ^:async run-reproject-z! []
     (check-reprojected-z!
      (await (run/run-model (transform-crs-model)
                            {:parcels (ds/geojson->dataset three-d-geojson)}
                            {:timeout-ms 30000})))))

(deftest table-flow-reprojects-a-geometry-with-z
  (testing "a 3D line reprojects in x and y and keeps its z"
    #?(:clj (check-reprojected-z!
             (run/run-model (transform-crs-model)
                            {:parcels (ds/geojson->dataset three-d-geojson)}
                            {:timeout-ms 30000}))
       :cljs (run-reproject-z!))))

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

#?(:cljs
   (defn ^:async run-misnamed-reproject! []
     (let [msg (await (-> (run/run-model (transform-crs-model)
                                         {:parcels (misnamed-geometry-dataset)}
                                         {:timeout-ms 30000})
                          (.then (fn [_] nil))
                          (.catch (fn [e] (ex-message e)))))]
       (check-names-the-columns! msg))))

(deftest transform-crs-throws-without-a-geometry-column
  (testing "a dataset with rows and no geometry column throws and names its columns"
    #?(:clj (check-names-the-columns!
             (try
               (run/run-model (transform-crs-model)
                              {:parcels (misnamed-geometry-dataset)}
                              {:timeout-ms 30000})
               nil
               (catch Exception e (ex-message e))))
       :cljs (run-misnamed-reproject!))))

;; A layer from a city server can hold a row with no geometry.
#?(:clj
   (deftest transform-crs-passes-a-nil-geometry-through
     (let [d    (ds/geojson->dataset (wgs84-geojson two-points))
           rows (conj (vec (ds/rows d :as-maps)) {:id 3 :geometry nil})
           out  (result-dataset
                 (run/run-model (transform-crs-model) {:parcels (ds/->dataset rows)}
                                {:timeout-ms 30000}))
           geoms (vec (ds/column out :geometry))]
       (is (= 3 (count geoms)) "the row with no geometry stays")
       (is (nil? (nth geoms 2)))
       (is (> (absv (geo/x (nth geoms 0))) 1000.0) "the other rows are reprojected"))))

#?(:cljs (-> (setup!)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.dataset-exec-test")))))
