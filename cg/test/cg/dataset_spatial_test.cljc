;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dataset-spatial-test
  "The spatial helpers of cg.dataset that parcel.cg and zoning.cg call, on
   both runtimes: the zoning chain runs on the JS runtime too."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.dataset :as ds]
                    [cg.feature :as feature]
                    [cg.geo :as geo])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- square [x0 side]
  (feature/polygon [[x0 0] [(+ x0 side) 0] [(+ x0 side) side] [x0 side] [x0 0]]))

(defn- two-squares []
  (ds/maps->dataset [{:id 1 :geometry (square 0 10)}
                     {:id 2 :geometry (square 100 10)}]))

(defn- ids [d]
  (mapv (fn [r] (get r :id)) (ds/rows d :as-maps)))

(deftest select-intersects-keeps-the-rows-that-intersect
  (testing "a point in the first square selects only that row"
    (is (= [1] (ids (ds/select-intersects (two-squares) (feature/point 5 5)))))))

(defn- indexed-squares []
  (ds/add-spatial-index (two-squares)))

(deftest index-candidates-fn-gives-the-index-payloads-near-a-geometry
  (testing "the candidates of a point in the first square: one payload, as on the JVM"
    (let [candidates-of (ds/index-candidates-fn (indexed-squares) :geometry)
          hits (vec (candidates-of (feature/point 5 5)))]
      (is (= 1 (count hits)))
      (is (true? (geo/intersects? (:prepared-geometry (first hits)) (feature/point 5 5)))
          "a predicate on :prepared-geometry answers for the indexed geometry")
      (is (= 100.0 (geo/area (geo/prepared-geometry-get-geometry (:prepared-geometry (first hits)))))
          ":prepared-geometry is a prepared geometry, as on the JVM")
      (is (some? (:indexed-id (first hits))) "the payload names the row")))
  (testing "no index, no closure (the JVM gives nil and callers scan)"
    (is (nil? (ds/index-candidates-fn (two-squares) :geometry)))))

;; The JS runtime keeps the prepared geometries of the 2048 candidates used
;; last. A row of 2100 squares makes the first one leave that cache.
(defn- probe-square [candidates-of i]
  (let [p    (feature/point (+ 5 (* 20 i)) 5)
        hits (vec (candidates-of p))]
    (and (= 1 (count hits))
         (true? (geo/intersects? (:prepared-geometry (first hits)) p)))))

(deftest index-candidates-fn-answers-for-a-candidate-it-prepared-long-ago
  (let [n             2100
        d             (ds/add-spatial-index
                       (ds/maps->dataset (mapv (fn [i] {:id i :geometry (square (* 20 i) 10)})
                                               (range n))))
        candidates-of (ds/index-candidates-fn d :geometry)]
    (is (every? (fn [i] (probe-square candidates-of i)) (range n))
        "each square is the one candidate of a point in it")
    (is (probe-square candidates-of 0) "the first square, prepared again")
    (is (probe-square candidates-of (dec n)) "the last square")))

(deftest the-prepared-geometry-operations-that-parcel-cg-applies
  (let [pg (geo/prepared-geometry-factory-prepare (square 0 10))]
    (is (true? (geo/prepared-geometry-intersects pg (feature/point 5 5))))
    (is (false? (geo/prepared-geometry-intersects pg (feature/point 50 50))))
    (is (true? (geo/intersects? pg (feature/point 5 5)))
        "geo/intersects? with a prepared receiver, as the JVM reaches it by reflection")
    (is (true? (geo/contains? pg (feature/point 5 5))))
    (is (= 100.0 (geo/area (geo/prepared-geometry-get-geometry pg))))))

(deftest intersects-any?-with-and-without-an-index
  (is (true? (ds/intersects-any? (indexed-squares) (feature/point 5 5))))
  (is (false? (ds/intersects-any? (indexed-squares) (feature/point 50 50))))
  (is (true? (ds/intersects-any? (two-squares) (feature/point 105 5)))
      "with no index, a scan of the rows"))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.dataset-spatial-test")))))
