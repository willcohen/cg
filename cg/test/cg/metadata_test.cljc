;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.metadata-test
  "Tests for trailing EDN metadata support (cg.metadata).

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
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [clojure.string :as str]
                    [cg.metadata :as metadata]
                    [cg.introspect :as introspect]
                    [cg.run :as run]
                    [cg.codegen :as codegen])
     :cljs (:require [cljs.test :refer [deftest is]]
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

(defn- with-positions
  "Put a :visual position on the first two nodes. The cljs model is a plain
   JS object."
  [model]
  #?(:clj (-> model
              (assoc-in [:nodes 0 :visual] {:pos [100 100]})
              (assoc-in [:nodes 1 :visual] {:pos [250 100]}))
     :cljs (do (aset (nth (.-nodes model) 0) "visual" {:pos [100 100]})
               (aset (nth (.-nodes model) 1) "visual" {:pos [250 100]})
               model)))

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
  (let [result (metadata/split-code-and-metadata content-with-metadata)
        md     (:metadata result)]
    (is (str/ends-with? (:code result) "(geo/simplify 1.0)))") "the code stops before the ^:cgproj tag")
    (is (= 3 (entry-count (:nodes md))))
    (is (= {:zoom 1.0 :pan [0 0]} (:viewport md)))))

(deftest test-split-plain-code
  (let [plain-code "(item-> :data (buffer 10))"
        result (metadata/split-code-and-metadata plain-code)]
    (is (= plain-code (:code result)))
    (is (= 0 (entry-count (:metadata result))))))

(deftest test-round-trip-with-metadata
  (let [generated-code (codegen/flow-graph->cg-code-with-metadata
                        (with-positions (buffer-model)))]

    (is (str/includes? generated-code ":viewport"))

    (let [reparsed (metadata/split-code-and-metadata generated-code)]
      (is (= 2 (entry-count (:nodes (:metadata reparsed))))
          "Should preserve node positions"))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.metadata-test")))))
