;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.metadata-test
  "Tests for trailing EDN metadata support (cg.metadata). ONE .cljc suite runs
   under clojure.test (JVM, `bb test`) and under cljs.test (squint and node,
   `bb test:cljs`).

   Visual metadata is stored as trailing EDN with ^:cgproj marker:

   (def my-model
     (item-> :data (buffer 10)))

   ^:cgproj
   {:nodes {\"data-src\" {:pos [100 100]}
            \"buffer-1\" {:pos [250 100]}}
    :viewport {:zoom 1.0 :pan [0 0]}}

   This allows storing visual editor state without affecting code execution.

   The two runtimes read the trailing EDN differently. The JVM uses the EDN
   reader. cljs compiles the text with squint and evaluates it, which gives a
   plain JS object. Because of this, a count of entries needs one helper for
   each runtime."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.string :as str]
                    [cg.metadata :as metadata]
                    [cg.introspect :as introspect]
                    [cg.run :as run]
                    [cg.codegen :as codegen])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [clojure.string :as str]
                     ["../../src/cg/metadata.mjs" :as metadata]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/codegen.mjs" :as codegen]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- entry-count
  "The number of entries in a metadata map. The cljs form is a plain JS
   object."
  [m]
  #?(:clj (count m)
     :cljs (count (js/Object.keys m))))

(defn- buffer-simplify-model
  "The model that the metadata tests attach positions to. The JVM builds it
   through SCI and reads it back from the eval context. cljs gets the model
   from eval-cg directly."
  []
  #?(:clj (do (run/reset-context!)
              (run/eval-cg "(require '[cg.macros :refer [item->]])
                            (require '[cg.geo :as geo])
                            (def test-model (item-> :parcels
                                                    (geo/buffer 10)
                                                    (geo/simplify 1.0)))
                            :done")
              (get (introspect/extract-models-from-context (run/get-cg-context))
                   'test-model))
     :cljs (run/eval-cg (str "(require '[cg.macros :refer [item->]])"
                             "(require '[cg.geo :as geo])"
                             "(item-> :parcels (geo/buffer 10) (geo/simplify 1.0))"))))

(defn- buffer-model []
  #?(:clj (do (run/reset-context!)
              (run/eval-cg "(require '[cg.macros :refer [item->]])
                            (require '[cg.geo :as geo])
                            (def test-model (item-> :parcels (geo/buffer 10)))
                            :done")
              (get (introspect/extract-models-from-context (run/get-cg-context))
                   'test-model))
     :cljs (run/eval-cg (str "(require '[cg.macros :refer [item->]])"
                             "(require '[cg.geo :as geo])"
                             "(item-> :parcels (geo/buffer 10))"))))

(def ^:private content-with-metadata
  "(require '[cg.macros :refer [item->]])
(require '[cg.geo :as geo])

(def my-model
  (item-> :parcels
    (geo/buffer 10)
    (geo/simplify 1.0)))

^:cgproj
{:nodes {\"parcels\" {:pos [100 100]}
         \"buffer-1\" {:pos [250 100]}
         \"simplify-2\" {:pos [400 100]}}
 :viewport {:zoom 1.0 :pan [0 0]}}")

(deftest test-split-code-and-metadata
  (testing "Split code with trailing metadata"
    (let [result (metadata/split-code-and-metadata content-with-metadata)
          md     (:metadata result)]
      (is (string? (:code result)) "Should extract code")
      (is (> (count (:code result)) 0) "Code should not be empty")
      (is (= 3 (entry-count (:nodes md))) "Should have 3 node positions")
      ;; Field by field, because the JVM map and the JS object do not compare
      ;; as one value.
      (is (= 1.0 (:zoom (:viewport md))) "Should extract the viewport zoom")
      (is (= [0 0] (:pan (:viewport md))) "Should extract the viewport pan"))))

(deftest test-split-plain-code
  (testing "Split code without trailing metadata"
    (let [plain-code "(item-> :data (buffer 10))"
          result (metadata/split-code-and-metadata plain-code)]
      (is (= plain-code (:code result)) "Code should be unchanged")
      (is (= 0 (entry-count (:metadata result))) "Metadata should be empty"))))

(deftest test-merge-metadata-into-graph
  (testing "Merge visual metadata into Flow Graph"
    (let [graph (buffer-simplify-model)
          metadata-map {:nodes {"parcels" {:pos [100 100]}
                                "buffer-1" {:pos [250 100]}
                                "simplify-2" {:pos [400 100]}}
                        :viewport {:zoom 1.0 :pan [0 0]}}
          merged-graph (metadata/merge-metadata-into-graph graph metadata-map)]

      (is (= (count (:nodes graph)) (count (:nodes merged-graph)))
          "Node count should remain the same")

      (testing "Visual metadata attached to nodes"
        (let [source-node (first (:nodes merged-graph))]
          (is (some? (:visual source-node)) "Source node should have visual metadata")
          (is (vector? (:pos (:visual source-node))) "Should have position vector"))))))

(deftest test-auto-layout-nodes
  (testing "Auto-layout assigns positions to nodes without metadata"
    (let [graph-no-positions {:nodes [{:type :source :id :data-src :data-ref :data}
                                      {:type :operation :id :buffer-1 :fn :buffer}
                                      {:type :operation :id :simplify-2 :fn :simplify}
                                      {:type :sink :id :result}]
                              :edges []
                              :config {}}
          auto-layout-graph (metadata/auto-layout-nodes graph-no-positions)
          nodes (:nodes auto-layout-graph)
          nodes-with-positions (filter metadata/has-position? nodes)]

      (is (= 4 (count nodes-with-positions)) "All 4 nodes should have positions")
      (is (not= (:pos (:visual (nth nodes 0)))
                (:pos (:visual (nth nodes 1))))
          "Different nodes should have different positions"))))

(deftest test-round-trip-with-metadata
  (testing "Round-trip: graph → code+metadata → graph preserves positions"
    (let [original-graph (buffer-model)
          metadata-map {:nodes {"parcels" {:pos [100 100]}
                                "buffer-1" {:pos [250 100]}}
                        :viewport {:zoom 1.5 :pan [10 20]}}
          graph-with-metadata (metadata/merge-metadata-into-graph original-graph metadata-map)
          generated-code (codegen/flow-graph->cg-code-with-metadata graph-with-metadata)]

      (is (string? generated-code) "Should generate code")
      (is (str/includes? generated-code "^:cgproj") "Should contain ^:cgproj marker")
      (is (str/includes? generated-code ":viewport") "Should contain the viewport")

      (testing "Parse generated code back"
        (let [reparsed (metadata/split-code-and-metadata generated-code)]
          (is (= 2 (entry-count (:nodes (:metadata reparsed))))
              "Should preserve node positions"))))))

(deftest test-has-position?
  (testing "has-position? sees a node with a position and skips one without"
    (is (metadata/has-position? {:id :test :visual {:pos [100 200]}}))
    (is (not (metadata/has-position? {:id :test2})))))

#?(:clj
   (deftest test-load-cg-with-metadata
     (testing "Full pipeline: load code with metadata"
       (let [test-content "(require '[cg.macros :refer [item->]])
(require '[cg.geo :as geo])

(def my-model
  (item-> :parcels (geo/buffer 10)))

^:cgproj
{:nodes {\"parcels\" {:pos [100 100]}
         \"buffer-1\" {:pos [250 100]}}}"

             result (metadata/load-cg-with-metadata
                     test-content
                     (fn [code]
                       (run/reset-context!)
                       (run/eval-cg code)
                       {:models (introspect/extract-models-from-context (run/get-cg-context))}))]

         (is (map? result) "Should return result map")
         (is (pos? (count (:models result))) "Should extract models")

         (let [loaded-graph (first (vals (:models result)))]
           (is (every? metadata/has-position? (:nodes loaded-graph))
               "All nodes should have positions from metadata"))))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.metadata-test")))))
