;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.boston-steps-test
  "examples/boston.cg loads with no network, and its steps resolve: a
   step that takes a table is an op, and the other steps are plain fns."
  #?(:clj (:require [clojure.test :refer [deftest is testing use-fixtures]]
                    [clojure.string :as str]
                    [cg.feature :as feature]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [clojure.string :as str]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/run.mjs" :as run]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private boston "examples/boston.cg")

#?(:clj (use-fixtures :once (fn [t] (run/load-module boston) (t))))

(defn- cg-value
  "The value of `qualified-name` in a loaded module. JS eval-cg does not
   resolve a var, so the value comes through an alias."
  [qualified-name]
  (let [[ns-name v] (str/split qualified-name #"/")
        alias (last (str/split ns-name #"\."))]
    (run/eval-cg (str "(require '[" ns-name " :as " alias "]) " alias "/" v))))

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

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run/load-module boston)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.boston-steps-test")))))
