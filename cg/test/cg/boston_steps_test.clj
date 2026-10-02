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

(defn- boston-fn [n]
  (deref (run/eval-cg (str "#'boston/" n))))

(deftest the-boston-steps
  (testing "roll-up-parcels is a table op, because it takes a table"
    (is (some? (feature/get-operation-info "boston/roll-up-parcels"))))
  (testing "analyze-parcels is a table op"
    (is (some? (feature/get-operation-info "boston/analyze-parcels"))))
  (testing "the other steps are plain fns, because they take no table or row"
    (doseq [n ["label-neighborhood" "ensure-analysis!"]]
      (is (fn? (boston-fn n)) n)
      (is (nil? (feature/get-operation-info (str "boston/" n))) n))))
