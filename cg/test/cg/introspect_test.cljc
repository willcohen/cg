;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; Model extraction from a string or a file needs SCI, which is JVM only. On
;; the JS runtime the node CLI gets each model by its name. The when-row explain
;; hook is on both runtimes, therefore its assertions run twice.
(ns cg.introspect-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.string :as str]
                    [cg.introspect :as introspect]
                    [cg.model :as model])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [clojure.string :as str]
                     ["../../src/cg/introspect.mjs" :as introspect]
                     ["../../src/cg/cli.mjs" :as cli]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (deftest test-extract-models-from-string
     (testing "Extract single model from code string"
       (let [{:keys [result models]} (introspect/extract-models-from-string
                                       "(require '[cg.macros :refer [item->]])
                                        (require '[cg.geo :as geo])
                                        (def buffer-model
                                          (item-> :data (geo/buffer 10)))
                                        :done")]
         (is (= :done result) "Should return last expression value")
         (is (= 1 (count models)) "Should find exactly one model")
         (is (contains? models 'buffer-model) "Should find buffer-model")
         (is (model/model? (get models 'buffer-model)) "buffer-model should be a valid model")))))

#?(:clj
   (deftest test-extract-multiple-models
     (testing "Extract multiple models from code string"
       (let [{:keys [models]} (introspect/extract-models-from-string
                                "(require '[cg.macros :refer [item-> coll->]])
                                 (require '[cg.geo :as geo])
                                 (def model1 (item-> :data (geo/buffer 10)))
                                 (def model2 (coll-> :data (geo/union)))
                                 (def not-a-model 42)
                                 :done")]
         (is (= 2 (count models)) "Should find exactly two models")
         (is (contains? models 'model1) "Should find model1")
         (is (contains? models 'model2) "Should find model2")
         (is (not (contains? models 'not-a-model)) "Should not include non-models")
         (is (model/model? (get models 'model1)))
         (is (model/model? (get models 'model2)))))))

#?(:clj
   (deftest test-extract-models-from-file
     (testing "Extract models from example file"
       (let [{:keys [models file]} (introspect/extract-models-from-file
                                     "test/fixtures/threading-macros.cg")]
         (is (= "test/fixtures/threading-macros.cg" file))
         (is (pos? (count models)) "Should find at least one model")
         (is (every? model/model? (vals models))
             "All extracted values should be valid models")))))

#?(:clj
   (deftest test-list-models
     (testing "List models with summary info"
       (let [{:keys [models]} (introspect/extract-models-from-string
                                "(require '[cg.macros :refer [item->]])
                                 (require '[cg.geo :as geo])
                                 (def m (item-> :data (geo/buffer 10) (geo/simplify 1.0)))
                                 :done")
             summary (introspect/list-models models)]
         (is (= 1 (count summary)))
         (let [info (first summary)]
           (is (= 'm (:name info)))
           (is (= 4 (:node-count info)) "Should have source + 2 ops + sink")
           (is (= 3 (:edge-count info)) "Should have 3 edges")
           (is (= [:geo/buffer :geo/simplify] (:operations info))))))))

#?(:clj
   (deftest test-get-model-by-name
     (testing "Get specific model by name"
       (let [result (introspect/extract-models-from-string
                      "(require '[cg.macros :refer [item->]])
                       (require '[cg.geo :as geo])
                       (def my-model (item-> :data (geo/buffer 10)))
                       :done")
             model (introspect/get-model-by-name result 'my-model)]
         (is (some? model) "Should find the model")
         (is (model/model? model) "Should be a valid model")
         (is (nil? (introspect/get-model-by-name result 'nonexistent))
             "Should return nil for non-existent model")))))

;; The node CLI gets each model of a file by its name. A command gives 0 or
;; true when it found the models and they are valid.
#?(:cljs
   (deftest the-node-cli-finds-the-models-of-a-file
     (is (= ["buffered"] (vec (introspect/extract-model-names-from-code
                               "(def shapes 1)\n(def buffered\n  (table-> :shapes (geo/buffer 1)))"))))
     (is (= 0 (cli/show-model "examples/simple.cg")))
     (is (= 0 (cli/generate "examples/simple.cg")))
     (is (= true (cli/validate "examples/simple.cg")))))

(defn- when-row-fixture-node
  "Builds a synthetic when-row op-node shaped exactly like the macro
   layer would emit. Centralised so the explain tests share one
   source of truth for the params shape."
  ([] (when-row-fixture-node {:on-miss "pass-through"}))
  ([options]
   {:type :operation
    :id :n1
    :fn "cg.run/when-row"
    :mode :table
    :params [{:predicate {:kind "has-column?" :arg :geometry}
              :options options
              :body {:fn "cg.geo/buffer" :params {:distance 100}}}]
    :config {}}))

(deftest when-row-node?-classifies-correctly
  (testing "when-row-node? recognises both string and symbol :fn forms"
    (is (introspect/when-row-node? (when-row-fixture-node)))
    (is (introspect/when-row-node?
         (assoc (when-row-fixture-node) :fn 'cg.run/when-row))))
  (testing "when-row-node? rejects non-when-row op-nodes"
    (is (not (introspect/when-row-node?
              (assoc (when-row-fixture-node) :fn "cg.geo/buffer"))))
    (is (not (introspect/when-row-node?
              (assoc (when-row-fixture-node) :type :sink))))
    (is (not (introspect/when-row-node? nil)))
    (is (not (introspect/when-row-node? "not a node")))))

(deftest explain-when-row-returns-nil-for-non-when-row
  (testing "explain-when-row returns nil when given a non-when-row node"
    (is (nil? (introspect/explain-when-row
               (assoc (when-row-fixture-node) :fn "cg.geo/buffer"))))
    (is (nil? (introspect/explain-when-row nil)))
    (is (nil? (introspect/explain-when-row {})))))

(deftest explain-when-row-pass-through-default
  (testing "explain-when-row returns three-stage expansion for the default on-miss"
    (let [exp (introspect/explain-when-row (when-row-fixture-node))]
      (is (= :when-row (:node-kind exp)))
      (is (= "has-column?" (-> exp :predicate :kind)))
      (is (= "pass-through" (:on-miss exp)))
      (is (= "cg.geo/buffer" (-> exp :body :fn)))
      (let [steps (mapv :step (:expansion exp))]
        (is (= [:predicate-split :body-apply :pass-through-merge] steps)
            "expansion lists predicate-split, body-apply, then the pass-through merge")))))

(deftest explain-when-row-drop-merge
  (testing "explain-when-row emits the drop-merge stage when :on-miss is drop"
    (let [exp (introspect/explain-when-row
               (when-row-fixture-node {"on-miss" "drop"}))]
      (is (= "drop" (:on-miss exp)))
      (let [merge-step (last (:expansion exp))]
        (is (= :drop-merge (:step merge-step))
            "the final stage swaps to :drop-merge under on-miss drop")
        (is (str/includes? (:description merge-step) "suppressed"))))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.introspect-test")))))
