;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.boston-steps-test
  "examples/boston.cg loads with no network, and its steps resolve: a
   step that takes a table is an op, and the other steps are plain fns."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cg.feature :as feature]
            [cg.run :as run]))

(set! *warn-on-reflection* true)

(use-fixtures :once
  (fn [t]
    (run/load-module "examples/boston.cg")
    (t)))

(defn- cg-value [qualified-name]
  (deref (run/eval-cg (str "#'" qualified-name))))

(deftest the-boston-steps
  (testing "roll-up-parcels and analyze-parcels are table ops, because they take a table"
    (is (some? (feature/get-operation-info "boston/roll-up-parcels")))
    (is (some? (feature/get-operation-info "boston/analyze-parcels"))))
  (testing "the other steps are plain fns, because they take no table or row"
    (doseq [n ["boston/label-neighborhood" "boston/ensure-analysis!"]]
      (is (fn? (cg-value n)) n)
      (is (nil? (feature/get-operation-info n)) n))))

(deftest the-flat-row-keeps-the-cumulative-side-setback
  (let [rules  (cg-value "boston/boston-current-rules")
        row    {:edges [{:edge-type :side :setback 4.0} {:edge-type :side :setback 6.5}
                        {:edge-type :rear :setback 20.0}]
                :metrics {}}
        values ((cg-value "cg.zoning/measure-values")
                rules row (cg-value "boston/boston-parcel-schema"))]
    (is (= 10.5 (:actual-cumulative-side-setback
                  ((cg-value "cg.zoning/result-columns") rules values {}))))))
