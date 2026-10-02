;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parcel-debug-layers-test
  "Shape + content assertions for parcel/debug-layers. parcel.cg is a .cg
   module: it loads into CG's SCI context, not the JVM namespace registry, so
   debug-layers is exercised through eval-cg — the same context the backend
   session runs it in — and we assert on the host datasets it returns (SCI
   shares host data structures, so the crossing is transparent). A synthetic
   fixture parcel with known categorization makes a regression fail an
   assertion, not just the eye."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cg.run :as run]
            [cg.dataset :as ds]))

(set! *warn-on-reflection* true)

(use-fixtures :once
  (fn [t]
    (run/load-module "src/cg/parcel.cg")
    (t)))

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
    (testing "returns the three named geometry dataset vars"
      (is (contains? layers :debug-edges))
      (is (contains? layers :debug-envelope))
      (is (contains? layers :debug-boundary)))
    (testing "each is a materialized dataset (EmittedVarsAreMaterializedDatasets)"
      (is (ds/dataset? (:debug-edges layers)))
      (is (ds/dataset? (:debug-envelope layers)))
      (is (ds/dataset? (:debug-boundary layers))))
    (testing "geometry stays EPSG:2249, tagged so /api/data reprojects to 4326"
      (is (= "EPSG:2249" (:crs (meta (:debug-edges layers)))))
      (is (= "EPSG:2249" (:crs (meta (:debug-envelope layers)))))
      (is (= "EPSG:2249" (:crs (meta (:debug-boundary layers))))))))

;; String column names, because the map styler keys on "category".
(deftest debug-layers-have-no-default-crs
  (testing "the caller names the CRS of its rows: no 1-arity form"
    (is (thrown? Exception
                 (run/eval-cg "(require '[cg.parcel :as parcel]) (parcel/debug-layers [])")))
    (is (thrown? Exception
                 (run/eval-cg "(require '[cg.parcel :as parcel]) (parcel/debug-layer-builders [])")))))

(deftest debug-setback-labels-read-the-units
  (let [layers (run/eval-cg
                "(require '[cg.parcel :as parcel] '[cg.feature :as f])
                 (parcel/debug-layers
                  [{:parcel-id \"S-2\"
                    :main-building {:geometry (f/polygon [[40 30] [60 30] [60 50] [40 50] [40 30]])}
                    :edges [{:edge (f/linestring [[0 0] [100 0]]) :edge-type :front :setback 30.0}]}]
                  \"EPSG:3857\" {:length \"m\"})")
        labels (vec (ds/column (:debug-setbacks layers) "label"))]
    (is (= ["30.0 m"] labels) "the label takes the length unit of the caller")))

(deftest debug-edges-carries-category-and-identity
  (let [layers (emit)
        edges  (:debug-edges layers)
        cats   (vec (ds/column edges "category"))]
    (testing "one feature per categorized edge"
      (is (= 4 (ds/row-count edges))))
    (testing "category rides as a scalar column (CategoryRidesAsScalarColumn)"
      (is (= #{"front" "side" "rear"} (set cats)))
      (is (= 2 (count (filter #{"side"} cats)))))
    (testing "each edge carries stable parcel identity (CarriesStableFeatureIdentity)"
      (is (every? some? (ds/column edges "parcel-id")))
      (is (= #{"TEST-1"} (set (ds/column edges "parcel-id")))))
    (testing "setback rides alongside for future distance-arrow work"
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
    (testing "edge decision scalars ride as columns (surface-everything)"
      (is (contains? cols "abutting"))
      (is (contains? cols "angle"))
      (is (contains? cols "length"))
      (is (= [:street :lot] (mapv keyword (ds/column edges "abutting")))))
    (testing "parcel-level context rides prefixed parcel.*"
      (is (contains? cols "parcel.lot-type"))
      (is (contains? cols "parcel.edge-method"))
      (is (= #{"corner"} (set (ds/column edges "parcel.lot-type")))))
    (testing "sparse keys nil-fill so the dataset stays rectangular"
      (is (contains? cols "right-of-way-width"))
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
      (is (contains? cols "label"))
      (is (contains? cols "category"))
      (is (contains? cols "parcel-id"))
      (is (= #{"30.0 ft" "40.0 ft"} (set (ds/column sb "label"))))
      (is (= #{"front" "side"} (set (ds/column sb "category")))))
    (testing "each row has a materialized indicator geometry"
      (is (every? some? (ds/column sb :geometry))))))

(deftest debug-polygon-layers-are-per-parcel
  (let [layers (emit)]
    (testing "envelope + boundary are one feature per parcel"
      (is (= 1 (ds/row-count (:debug-envelope layers))))
      (is (= 1 (ds/row-count (:debug-boundary layers)))))
    (testing "and carry the parcel identity"
      (is (= #{"TEST-1"} (set (ds/column (:debug-envelope layers) "parcel-id"))))
      (is (= #{"TEST-1"} (set (ds/column (:debug-boundary layers) "parcel-id")))))))
