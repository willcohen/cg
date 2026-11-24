;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parcel-debug-layers-test
  "parcel/debug-layers on both runtimes. parcel.cg is a .cg module, so the
   tests call debug-layers through eval-cg, in the context that the backend
   session uses. A made-up parcel with known edge types makes a regression
   fail a check."
  #?(:clj (:require [clojure.test :refer [deftest is testing use-fixtures]]
                    [cg.run :as run]
                    [cg.dataset :as ds])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private parcel "src/cg/parcel.cg")

#?(:clj (use-fixtures :once (fn [t] (run/load-module parcel) (t))))

;; One parcel in EPSG:2249 with a front, two sides and a rear, in the shape
;; that analyze-parcel gives.
(def ^:private fixture-emit
  "(require '[cg.parcel :as parcel] '[cg.feature :as f])
   (parcel/debug-layers
     [{:parcel-id \"TEST-1\"
       :edges [{:edge (f/linestring [[0 0] [100 0]])   :edge-type :front :setback 15.0}
               {:edge (f/linestring [[100 0] [100 80]]) :edge-type :side  :setback 8.0}
               {:edge (f/linestring [[100 80] [0 80]])  :edge-type :rear  :setback 25.0}
               {:edge (f/linestring [[0 80] [0 0]])     :edge-type :side  :setback 8.0}]
       :envelope (f/polygon [[15 15] [85 15] [85 55] [15 55] [15 15]])
       :effective-boundary-geom (f/polygon [[0 0] [100 0] [100 80] [0 80] [0 0]])}]
     \"EPSG:2249\" {:length \"ft\"})")

(defn- emit [] (run/eval-cg fixture-emit))

(deftest debug-layers-shapes
  (let [layers (emit)]
    (testing "each is a materialized dataset"
      (is (ds/dataset? (:debug-edges layers)))
      (is (ds/dataset? (:debug-envelope layers)))
      (is (ds/dataset? (:debug-boundary layers)))
      (is (ds/dataset? (:debug-setbacks layers))))
    (testing "geometry stays EPSG:2249, tagged so /api/data reprojects to 4326"
      (is (= "EPSG:2249" (:crs (meta (:debug-edges layers)))))
      (is (= "EPSG:2249" (:crs (meta (:debug-envelope layers)))))
      (is (= "EPSG:2249" (:crs (meta (:debug-boundary layers)))))
      (is (= "EPSG:2249" (:crs (meta (:debug-setbacks layers))))))))

#?(:clj
   (deftest debug-layers-have-no-default-crs
     (testing "the caller names the CRS of its rows: no 1-arity form"
       (is (thrown? Exception
                    (run/eval-cg "(require '[cg.parcel :as parcel]) (parcel/debug-layers [])")))
       (is (thrown? Exception
                    (run/eval-cg "(require '[cg.parcel :as parcel]) (parcel/debug-layer-builders [])"))))))

#?(:clj
   (deftest debug-setback-labels-read-the-units
     (let [layers (run/eval-cg
                   "(require '[cg.parcel :as parcel] '[cg.feature :as f])
                 (parcel/debug-layers
                  [{:parcel-id \"S-2\"
                    :main-building {:geometry (f/polygon [[40 30] [60 30] [60 50] [40 50] [40 30]])}
                    :edges [{:edge (f/linestring [[0 0] [100 0]]) :edge-type :front :setback 30.0}]}]
                  \"EPSG:3857\" {:length \"m\"})")
           labels (vec (ds/column (:debug-setbacks layers) "label"))]
       (is (= ["30.0 m"] labels) "the label takes the length unit of the caller"))))

;; String column names, because the map styler keys on "category".
(deftest debug-edges-carries-category-and-identity
  (let [layers (emit)
        edges  (:debug-edges layers)
        cats   (vec (ds/column edges "category"))]
    (is (= 4 (ds/row-count edges)))
    (testing "category rides as a scalar column"
      (is (= #{"front" "side" "rear"} (set cats)))
      (is (= 2 (count (filter #{"side"} cats)))))
    (testing "each edge carries stable parcel identity"
      (is (= #{"TEST-1"} (set (ds/column edges "parcel-id")))))
    (testing "the setback rides along as a column"
      (is (= #{15.0 8.0 25.0} (set (ds/column edges "setback")))))))

;; Each edge scalar and parcel field becomes a string-keyed column, with nil
;; for a missing key.
(def ^:private diagnostic-emit
  "(require '[cg.parcel :as parcel] '[cg.feature :as f])
   (parcel/debug-layers
     [{:parcel-id \"D-1\" :lot-type :corner :edge-method :angle-based :rectangular? false
       :edges [{:edge (f/linestring [[0 0] [100 0]])   :edge-type :primary-front :setback 15.0
                :abutting :street :angle 90.5 :length 100.0 :right-of-way-width 40}
               {:edge (f/linestring [[100 0] [100 80]]) :edge-type :side :setback 8.0
                :abutting :lot :angle 12.0 :length 80.0}]
       :envelope (f/polygon [[15 15] [85 15] [85 55] [15 55] [15 15]])
       :effective-boundary-geom (f/polygon [[0 0] [100 0] [100 80] [0 80] [0 0]])}]
     \"EPSG:2249\" {:length \"ft\"})")

(deftest debug-edges-surface-diagnostic-scalars
  (let [edges (:debug-edges (run/eval-cg diagnostic-emit))
        cols  (set (map name (ds/column-names edges)))]
    (testing "edge decision scalars ride as columns"
      (is (contains? cols "angle"))
      (is (contains? cols "length"))
      (is (= [:street :lot] (mapv keyword (ds/column edges "abutting")))))
    (testing "parcel-level context rides prefixed parcel.*"
      (is (contains? cols "parcel.edge-method"))
      (is (= #{"corner"} (set (ds/column edges "parcel.lot-type")))))
    (testing "sparse keys nil-fill so the dataset stays rectangular"
      (is (= [40 nil] (vec (ds/column edges "right-of-way-width")))))
    (testing "category + identity still ride for the styler + selection"
      (is (= ["primary-front" "side"] (vec (ds/column edges "category"))))
      (is (= #{"D-1"} (set (ds/column edges "parcel-id")))))))

;; One setback segment for each edge with a setback, on a parcel with a main
;; building, labeled "X.X ft".
(def ^:private setback-emit
  "(require '[cg.parcel :as parcel] '[cg.feature :as f])
   (parcel/debug-layers
     [{:parcel-id \"S-1\"
       :main-building {:geometry (f/polygon [[40 30] [60 30] [60 50] [40 50] [40 30]])}
       :edges [{:edge (f/linestring [[0 0] [100 0]])   :edge-type :front :setback 30.0}
               {:edge (f/linestring [[100 0] [100 80]]) :edge-type :side  :setback 40.0}
               {:edge (f/linestring [[100 80] [0 80]])  :edge-type :rear}]
       :envelope (f/polygon [[15 15] [85 15] [85 55] [15 55] [15 15]])
       :effective-boundary-geom (f/polygon [[0 0] [100 0] [100 80] [0 80] [0 0]])}]
     \"EPSG:2249\" {:length \"ft\"})")

(deftest debug-setbacks-are-building-to-edge-segments-with-labels
  (let [sb   (:debug-setbacks (run/eval-cg setback-emit))
        cols (set (map name (ds/column-names sb)))]
    (testing "one indicator segment per edge WITH a numeric setback (rear has none)"
      (is (= 2 (ds/row-count sb))))
    (testing "carries the distance, a formatted label, category, and identity"
      (is (contains? cols "setback"))
      (is (contains? cols "parcel-id"))
      (is (contains? cols "label"))
      ;; JS prints 30.0 as "30".
      #?(:clj (is (= #{"30.0 ft" "40.0 ft"} (set (ds/column sb "label")))))
      (is (= #{"front" "side"} (set (ds/column sb "category")))))
    (is (every? some? (ds/column sb :geometry)))))

(deftest debug-polygon-layers-are-per-parcel
  (let [layers (emit)]
    (is (= 1 (ds/row-count (:debug-envelope layers))))
    (is (= 1 (ds/row-count (:debug-boundary layers))))
    (is (= #{"TEST-1"} (set (ds/column (:debug-envelope layers) "parcel-id"))))
    (is (= #{"TEST-1"} (set (ds/column (:debug-boundary layers) "parcel-id"))))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run/load-module parcel)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.parcel-debug-layers-test")))))
