;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; Model extraction from a string or a file needs SCI, which is JVM only. On
;; the JS runtime the node CLI gets each model by its name.
(ns cg.introspect-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.introspect :as introspect]
                    [cg.model :as model]
                    [cg.dataset :as ds]
                    [cg.macros :refer [table->]])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["../../src/cg/introspect.mjs" :as introspect]
                     ["../../src/cg/cli.mjs" :as cli]
                     ["../../src/cg/run.mjs" :as run]
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

;; transform-crs is an operation group: its node has :type :group.
(defn- transform-crs-model []
  #?(:clj (table-> :pts (ds/transform-crs "EPSG:4326" "EPSG:3857"))
     :cljs (run/eval-cg (str "(require '[cg.macros :refer [table->]])"
                             "(require '[cg.dataset :as ds])"
                             "(table-> :pts (ds/transform-crs \"EPSG:4326\" \"EPSG:3857\"))"))))

(deftest list-models-names-a-group-op
  (is (= [:dataset/transform-crs]
         (:operations (first (introspect/list-models {"m" (transform-crs-model)}))))))

;; The node CLI gets each model of a file by its name. A command gives 0 or
;; true when it found the models and they are valid.
#?(:cljs
   (deftest the-node-cli-finds-the-models-of-a-file
     (is (= ["buffered"] (vec (introspect/extract-model-names-from-code
                               "(def shapes 1)\n(def buffered\n  (table-> :shapes (geo/buffer 1)))"))))
     (is (= 0 (cli/show-model "examples/simple.cg")))
     (is (= 0 (cli/generate "examples/simple.cg")))
     (is (= true (cli/validate "examples/simple.cg")))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.introspect-test")))))
