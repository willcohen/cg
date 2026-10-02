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
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cg.run :as run]))

(set! *warn-on-reflection* true)

(use-fixtures :once
  (fn [t]
    (run/load-module "src/cg/parcel.cg")
    (t)))

(defn- flatten-row [row]
  ((deref (run/eval-cg "#'cg.parcel/flatten-row")) row))

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
