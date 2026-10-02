;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; The JVM builds a model with the threading macros, and cljs sends the same
;; text through cg.run/eval-cg, because cljs has no macro expansion at run time.
(ns cg.codegen-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.string :as str]
                    [cg.codegen :as mg]
                    [cg.macros :refer [item-> coll-> table->]]
                    [cg.geo :as geo]
                    [cg.dataset :as ds :refer [summarize]])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [clojure.string :as str]
                     ["../../src/cg/codegen.mjs" :as mg]
                     ["../../src/cg/run.mjs" :as run]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- buffer-simplify-model []
  #?(:clj (item-> :parcels (geo/buffer 10) (geo/simplify 1.0))
     :cljs (run/eval-cg (str "(require '[cg.macros :refer [item->]])"
                             "(require '[cg.geo :as geo])"
                             "(item-> :parcels (geo/buffer 10) (geo/simplify 1.0))"))))

(defn- union-model []
  #?(:clj (coll-> :polygons (geo/union))
     :cljs (run/eval-cg (str "(require '[cg.macros :refer [coll->]])"
                             "(require '[cg.geo :as geo])"
                             "(coll-> :polygons (geo/union))"))))

(defn- buffer-model []
  #?(:clj (item-> :data (geo/buffer 100))
     :cljs (run/eval-cg (str "(require '[cg.macros :refer [item->]])"
                             "(require '[cg.geo :as geo])"
                             "(item-> :data (geo/buffer 100))"))))

(defn- filter-table-model []
  #?(:clj (table-> :buildings (ds/filter (fn [row] (> (:area row) 1000))))
     :cljs (run/eval-cg (str "(require '[cg.macros :refer [table->]])"
                             "(require '[cg.dataset :as ds])"
                             "(table-> :buildings (ds/filter (fn [row] (> (:area row) 1000))))"))))

(defn- with-visual
  "Put a visual position on the first node. The JVM model is an immutable map.
   The cljs model is a plain JS object, and codegen reads what is on it."
  [model]
  #?(:clj (assoc-in model [:nodes 0 :visual] {:pos [100 100] :collapsed false})
     :cljs (do (aset (nth (.-nodes model) 0) "visual"
                     {:pos [100 100] :collapsed false})
               model)))

(defn- check-buffer-simplify-code! [code]
  (is (str/includes? code "item->") "the item-> macro is in the code")
  (is (str/includes? code "geo/buffer") "the buffer operation is in the code")
  (is (str/includes? code "geo/simplify") "the simplify operation is in the code")
  (is (str/includes? code "parcels") "the data reference survives")
  ;; A parameter that does not reach the code makes the model unrunnable. The
  ;; tolerance is not checked here, because 1.0 prints as 1 on cljs.
  (is (str/includes? code "10") "the buffer distance survives"))

(defn- check-metadata-code! [code]
  (is (str/includes? code "^:cgproj") "the code carries the ^:cgproj marker")
  (is (str/includes? code ":nodes") "the metadata carries :nodes")
  (is (str/includes? code ":viewport") "the metadata carries :viewport"))

(deftest test-simple-pipeline
  (testing "Simple item-> pipeline round-trip"
    (let [generated (mg/flow-graph->cg-code (buffer-simplify-model))]
      (check-buffer-simplify-code! generated)
      #?(:clj
         (is (= (mg/normalize-code
                 "(item-> :parcels (geo/buffer 10) (geo/simplify 1.0))")
                (mg/normalize-code generated))
             "Generated code should match original after normalization")))))

(deftest test-collection-mode
  (testing "coll-> macro code generation"
    (let [generated (mg/flow-graph->cg-code (union-model))]
      (is (str/includes? generated "coll->") "the coll-> macro is in the code")
      (is (str/includes? generated "geo/union") "the union operation is in the code")
      #?(:clj
         (is (= (mg/normalize-code "(coll-> :polygons (geo/union))")
                (mg/normalize-code generated))
             "Collection mode should generate coll-> macro")))))

(deftest test-visual-metadata
  (testing "Visual metadata extraction and formatting"
    (check-metadata-code!
     (mg/flow-graph->cg-code-with-metadata (with-visual (buffer-simplify-model))))))

(deftest test-normalize-code
  (testing "Code normalization for comparison"
    (let [code1 "(item->  :data  (buffer  10)  (simplify  0.1))"
          code2 "(item-> :data (buffer 10) (simplify 0.1))"]
      (is (= (mg/normalize-code code1) (mg/normalize-code code2))
          "Whitespace variations should normalize to same string"))))

(deftest test-keyword-to-symbol-conversion
  (testing "Operation names convert from keywords to symbols"
    (let [generated (mg/flow-graph->cg-code (buffer-model))]
      (is (str/includes? generated "geo/buffer")
          "Should generate symbol not keyword")
      (is (not (str/includes? generated ":geo/buffer"))
          "Should not have keyword prefix"))))

(deftest test-table-macro-code-generation
  (testing "table-> code generation keeps the macro and the table reference"
    (let [generated (mg/flow-graph->cg-code (filter-table-model))]
      (is (str/includes? generated "table->") "the table-> macro is in the code")
      (is (str/includes? generated "buildings") "the table reference survives"))))

#?(:clj
   (deftest test-summarize-spec-round-trip
     (testing "a report op's serializable spec survives code -> model -> code"
       ;; The spec is pure data, so codegen must be able to regenerate the op
       ;; call with its inline map intact.
       (let [original-code (str "(table-> :rows"
                                " (summarize {:value :units, :as-int true,"
                                " :weight :rows, :scope :d, :scopes [:rs-2 :rs-3],"
                                " :buckets [{:label \"1\", :eq 1}"
                                " {:label \"4-6\", :range [4 6]}]}))")
             model (table-> :rows
                     (summarize {:value :units, :as-int true,
                                 :weight :rows, :scope :d, :scopes [:rs-2 :rs-3],
                                 :buckets [{:label "1", :eq 1}
                                           {:label "4-6", :range [4 6]}]}))
             generated-code (mg/flow-graph->cg-code model)]
         (is (= (mg/normalize-code original-code)
                (mg/normalize-code generated-code))
             "spec map must regenerate byte-comparably after normalization")))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.codegen-test")))))
