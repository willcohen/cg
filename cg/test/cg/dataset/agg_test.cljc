;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dataset.agg-test
  "Tests for the standard reducers in cg.dataset.agg and the
  cg.feature/reducer? predicate. JVM-only: cg.dataset.agg defines its
  reducers on the JVM only."
  (:require [clojure.test :refer [deftest is testing]]
            [cg.dataset :as ds]
            [cg.dataset.agg :as agg]
            [cg.feature :as f]))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (deftest count-returns-row-count
     (let [d (ds/->dataset {:area [10 20 30 40]})]
       (is (= 4 (agg/count d)))
       (is (= 0 (agg/count (ds/select-rows d [])))))))

#?(:clj
   (deftest sum-totals-the-over-column
     (let [d (ds/->dataset {:area [10 20 30 40]})]
       (is (= 100 (agg/sum d :area))))
     (is (= 0 (agg/sum (ds/select-rows (ds/->dataset {:area [1]}) []) :area)))))

#?(:clj
   (deftest mean-handles-empty
     (let [d (ds/->dataset {:area [10 20 30 40]})]
       (is (= 25.0 (agg/mean d :area))))
     (testing "empty sub-dataset returns nil, not NaN"
       (is (nil? (agg/mean (ds/select-rows (ds/->dataset {:area [10]}) [])
                           :area))))))

#?(:clj
   (deftest min-and-max-extremes
     (let [d (ds/->dataset {:area [30 10 20 40]})]
       (is (= 10 (agg/min d :area)))
       (is (= 40 (agg/max d :area))))
     (let [empty-d (ds/select-rows (ds/->dataset {:area [1]}) [])]
       (is (nil? (agg/min empty-d :area)))
       (is (nil? (agg/max empty-d :area))))))

#?(:clj
   (deftest reducer-predicate-tags-the-five-reducers
     (is (every? #(true? (f/reducer? (str "cg.dataset.agg/" %)))
                 ["count" "sum" "mean" "min" "max"]))
     (is (false? (f/reducer? "cg.feature/point")))
     (is (false? (f/reducer? "cg.dataset.agg/not-a-real-reducer")))))
