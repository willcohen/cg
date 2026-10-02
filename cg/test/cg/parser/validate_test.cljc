;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.validate-test
  "Cross-cutting invariant checks driven by programmatically constructed
   CgFlow maps. These tests bypass extract entirely, exercising
   validate's uniqueness and reference-resolution rules in isolation.

   A model is a Binding: threading-macro Bindings carry
   `:body-shape :threading_macro` plus the `:is-template` derived attribute.
   The fixtures below populate that shape.

   Run with: clojure -M:test-parser"
  (:require [cg.parser.validate :as validate]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

#?(:clj (set! *warn-on-reflection* true))

(def empty-span
  {:start-line 1 :start-column 1 :end-line 1 :end-column 1})

(defn- function-binding
  ([name head-sym] (function-binding name head-sym []))
  ([name head-sym args]
   {:name name :body-shape :function_call
    :head-sym head-sym :args args :location empty-span}))

(defn- threading-binding
  ([name mode input] (threading-binding name mode input []))
  ([name mode input ops]
   {:name name :body-shape :threading_macro
    :mode mode :input input :is-template true
    :ops ops :config nil :location empty-span}))

(defn- diagnostic-codes [flow]
  (into #{} (map :code) (:diagnostics flow)))

(defn- code-counts [flow]
  (frequencies (map :code (:diagnostics flow))))

(deftest detects-duplicate-threading-binding-names
  (testing "two threading-macro bindings sharing a name yield :duplicate-binding-name"
    (let [flow {:requires []
                :inputs []
                :parameters []
                :bindings [(threading-binding "m" :item "parcels")
                           (threading-binding "m" :coll "parcels")]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :duplicate-binding-name))
      (is (= 1 (get (code-counts out) :duplicate-binding-name))
          "exactly one diagnostic per duplicate occurrence past the first"))))

(deftest detects-unknown-map-component
  (testing "a map component naming no declared op yields :unknown-component"
    (let [flow {:requires [] :inputs [] :parameters []
                :operations []
                :bindings [(threading-binding
                            "out" :table "ps"
                            [{:qualified-name "group-by" :args [] :location empty-span}
                             {:qualified-name "map" :component "nope"
                              :args [] :location empty-span}])]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :unknown-component))))
  (testing "a map component naming a declared op is clean"
    (let [flow {:requires [] :inputs [] :parameters []
                :operations [{:name "f" :location empty-span}]
                :bindings [(threading-binding
                            "out" :table "ps"
                            [{:qualified-name "map" :component "f"
                              :args [] :location empty-span}])]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (not (contains? (diagnostic-codes out) :unknown-component))))))

(deftest detects-duplicate-parameter-names
  (testing "two parameters sharing a name yield :duplicate-parameter-name"
    (let [flow {:requires []
                :inputs []
                :parameters [{:name "p" :value {:kind :number :raw "1"}
                              :config nil :location empty-span}
                             {:name "p" :value {:kind :number :raw "2"}
                              :config nil :location empty-span}
                             {:name "p" :value {:kind :number :raw "3"}
                              :config nil :location empty-span}]
                :bindings []
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :duplicate-parameter-name))
      (is (= 2 (get (code-counts out) :duplicate-parameter-name))
          "two duplicate occurrences past the first"))))

(deftest detects-input-not-declared
  (testing "template binding source not in declared :inputs yields :input-not-declared"
    (let [flow {:requires []
                :inputs [{:name "parcels"}]
                :parameters []
                :bindings [(threading-binding "m" :item "buildings")]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :input-not-declared)))))

(deftest skips-input-check-when-no-inputs-block
  (testing "no :inputs block means any template-binding source is permitted"
    (let [flow {:requires []
                :inputs []
                :parameters []
                :bindings [(threading-binding "m" :item "buildings")]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (not (contains? (diagnostic-codes out) :input-not-declared))
          "with no declared :inputs, templates can use any source keyword"))))

(deftest clean-flow-yields-no-extra-diagnostics
  (testing "validate is a no-op on a well-formed CgFlow"
    (let [flow {:requires []
                :inputs []
                :parameters [{:name "p" :value {:kind :number :raw "1"}
                              :config nil :location empty-span}]
                :bindings [(function-binding "b" "ds/load-geojson")
                           (threading-binding "m" :item "parcels")]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (= 0 (count (:diagnostics out)))))))

(deftest detects-duplicate-binding-names
  (testing "two function-call bindings sharing a name yield :duplicate-binding-name"
    (let [flow {:requires []
                :inputs []
                :parameters []
                :bindings [(function-binding "ds" "ds/load-geojson")
                           (function-binding "ds" "ds/load-geojson")]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :duplicate-binding-name))
      (is (= 1 (get (code-counts out) :duplicate-binding-name))
          "exactly one diagnostic per duplicate occurrence past the first"))))

(deftest detects-name-collision-binding-vs-parameter
  (testing "a binding sharing a name with a parameter yields :name-collision"
    (let [flow {:requires []
                :inputs []
                :parameters [{:name "x" :value {:kind :number :raw "1"}
                              :config nil :location empty-span}]
                :bindings [(function-binding "x" "ds/load-geojson")]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :name-collision)))))

(deftest detects-duplicate-cross-shape-binding-names
  (testing "a function-call binding and a threading-macro binding sharing a name yield :duplicate-binding-name"
    (let [flow {:requires []
                :inputs []
                :parameters []
                :bindings [(function-binding "y" "ds/load-geojson")
                           (threading-binding "y" :item "parcels")]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (contains? (diagnostic-codes out) :duplicate-binding-name)
          "Stage D unified Bindings across both body shapes — name uniqueness is one-namespace"))))

(deftest no-name-collision-on-distinct-names
  (testing "distinct binding/parameter names yield no :name-collision"
    (let [flow {:requires []
                :inputs []
                :parameters [{:name "p" :value {:kind :number :raw "1"}
                              :config nil :location empty-span}]
                :bindings [(function-binding "b" "ds/load-geojson")
                           (threading-binding "m" :item "parcels")]
                :overlay nil
                :diagnostics []}
          out (validate/validate flow)]
      (is (not (contains? (diagnostic-codes out) :name-collision))))))
