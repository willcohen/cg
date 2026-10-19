;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parcel-flatten-row-test
  "cg.parcel/flatten-row: the flat row of one analyzed parcel, which a
   cache and an export store.

   analyze-parcel puts the lot measurements under :metrics. The flat row
   gives seven of them their own columns, because the reports read
   :lot-area at the top level. It keeps the scalar values,
   gives a keyword as its name, and drops the nested values."
  #?(:clj (:require [clojure.test :refer [deftest is testing use-fixtures]]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private parcel "src/cg/parcel.cg")

#?(:clj (use-fixtures :once (fn [t] (run/load-module parcel) (t))))

(defn- flatten-row [row]
  ((run/eval-cg "(require '[cg.parcel :as parcel]) parcel/flatten-row") row))

(def ^:private analyzed-row
  {:LOT_ID "P1"
   :lot-type :corner
   :corner? true
   :frontage-count 2
   :zone-class :rs-2
   :in-flood-zone :unknown
   :edges [{:type :front}]
   :metrics {:lot-area 4900.0 :street-frontage 70.0
             :envelope-ratio 0.5 :building-coverage-ratio 0.25
             :building-coverage-area 1225.0 :envelope-area 2450.0
             :perimeter 280.0}
   :geometry :a-geometry})

(deftest flatten-row-gives-one-flat-row
  (let [flat (flatten-row analyzed-row)]
    (testing "six metrics get their own columns"
      (is (= {:lot-area 4900.0 :street-frontage 70.0
              :envelope-ratio 0.5 :building-coverage-ratio 0.25
              :building-coverage-area 1225.0 :envelope-area 2450.0}
             (select-keys flat [:lot-area :street-frontage :envelope-ratio
                                :building-coverage-ratio :building-coverage-area
                                :envelope-area]))))
    (testing "a keyword becomes its name; strings, numbers and booleans stay"
      (is (= ["corner" "rs-2" "unknown" "P1" 2 true]
             (mapv flat [:lot-type :zone-class :in-flood-zone
                         :LOT_ID :frontage-count :corner?]))))
    (testing "nested values drop, and the geometry stays"
      (is (not (contains? flat :metrics)))
      (is (not (contains? flat :edges)))
      (is (not (contains? flat :perimeter)))
      (is (= :a-geometry (:geometry flat))))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run/load-module parcel)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.parcel-flatten-row-test")))))
