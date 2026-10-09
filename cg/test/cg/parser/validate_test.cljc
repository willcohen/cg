;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.validate-test
  "Cross-cutting invariant checks driven by programmatically constructed
   CgFlow maps. These tests bypass extract entirely, exercising
   validate's uniqueness and reference-resolution rules in isolation."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.parser.validate :as validate])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../../src/cg/parser/validate.mjs" :as validate]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def empty-span
  {:start-line 1 :start-column 1 :end-line 1 :end-column 1})

(defn- function-binding [name head-sym]
  {:name name :body-shape :function_call
   :head-sym head-sym :args [] :location empty-span})

(defn- threading-binding
  ([name] (threading-binding name []))
  ([name ops] {:name name :body-shape :threading_macro :ops ops :location empty-span}))

(defn- diagnostic-codes [flow]
  (into #{} (map :code) (:diagnostics flow)))

(defn- code-counts [flow]
  (frequencies (map :code (:diagnostics flow))))

(deftest detects-duplicate-threading-binding-names
  (testing "two threading-macro bindings sharing a name yield :duplicate-binding-name"
    (let [flow {:bindings [(threading-binding "m")
                           (threading-binding "m")]
                :diagnostics []}
          out (validate/validate flow)]
      (is (= 1 (get (code-counts out) :duplicate-binding-name))
          "exactly one diagnostic per duplicate occurrence past the first"))))

(deftest detects-unknown-map-component
  (testing "a map component naming no declared op yields :unknown-component"
    (let [flow {:bindings [(threading-binding
                            "out"
                            [{:qualified-name "group-by" :args [] :location empty-span}
                             {:qualified-name "map" :component "nope"
                              :args [] :location empty-span}])]
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :unknown-component))))
  (testing "a map component naming a declared op is clean"
    (let [flow {:operations [{:name "f" :location empty-span}]
                :bindings [(threading-binding
                            "out"
                            [{:qualified-name "map" :component "f"
                              :args [] :location empty-span}])]
                :diagnostics []}
          out (validate/validate flow)]
      (is (not (contains? (diagnostic-codes out) :unknown-component))))))

(deftest detects-duplicate-parameter-names
  (testing "two parameters sharing a name yield :duplicate-parameter-name"
    (let [flow {:parameters [{:name "p" :value {:kind :number :raw "1"}
                              :config nil :location empty-span}
                             {:name "p" :value {:kind :number :raw "2"}
                              :config nil :location empty-span}
                             {:name "p" :value {:kind :number :raw "3"}
                              :config nil :location empty-span}]
                :diagnostics []}
          out (validate/validate flow)]
      (is (= 2 (get (code-counts out) :duplicate-parameter-name))
          "two duplicate occurrences past the first"))))

(deftest clean-flow-yields-no-extra-diagnostics
  (testing "validate is a no-op on a well-formed CgFlow"
    (let [flow {:parameters [{:name "p" :value {:kind :number :raw "1"}
                              :config nil :location empty-span}]
                :bindings [(function-binding "b" "ds/load-geojson")
                           (threading-binding "m")]
                :diagnostics []}
          out (validate/validate flow)]
      (is (= 0 (count (:diagnostics out)))))))

(deftest detects-name-collision-binding-vs-parameter
  (testing "a binding sharing a name with a parameter yields :name-collision"
    (let [flow {:parameters [{:name "x" :value {:kind :number :raw "1"}
                              :config nil :location empty-span}]
                :bindings [(function-binding "x" "ds/load-geojson")]
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :name-collision)))))

(deftest detects-duplicate-cross-shape-binding-names
  (testing "a function-call binding and a threading-macro binding sharing a name yield :duplicate-binding-name"
    (let [flow {:bindings [(function-binding "y" "ds/load-geojson")
                           (threading-binding "y")]
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :duplicate-binding-name)
          "the two body shapes share one namespace of binding names"))))

#?(:cljs (run_tests_and_exit_BANG_ "cg.parser.validate-test"))
