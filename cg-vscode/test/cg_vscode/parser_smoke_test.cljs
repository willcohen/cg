;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.parser-smoke-test
  "Imports the in-place squint parser build of cg core, and resolves the
   grammar wasm with createRequire."
  (:require [cljs.test :as t :refer [deftest is]]
            ["node:module" :refer [createRequire]]
            ["../../../cg/src/cg/parser/core.mjs" :as parser]))

(def ^:private wasm-path
  (.resolve (createRequire (js* "import.meta.url"))
            "@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm"))

(deftest ^:async extract-models-from-minimal-flow
  (let [cg-flow (await (parser/extract-models
                        "(require '[cg.macros :refer [item->]])\n(def m (item-> :data))"
                        {:grammar-wasm wasm-path}))]
    (is (= 1 (count (:requires cg-flow))))
    (is (= 1 (count (:bindings cg-flow))))
    (is (= :threading_macro
           (-> cg-flow :bindings first :body-shape)))
    (is (empty? (:diagnostics cg-flow)))))

(deftest ^:async extract-models-loader-binding-classifies-as-dataset-source
  (let [cg-flow (await (parser/extract-models
                        "(def parcels (ds/load-geojson \"parcels.geojson\"))"
                        {:grammar-wasm wasm-path}))]
    (is (= 1 (count (:bindings cg-flow))))
    (let [b (first (:bindings cg-flow))]
      (is (= :function_call (:body-shape b)))
      ;; A loader head has :kind :source in the built-in signatures.
      (is (true? (:is-dataset-source b))))))

(deftest ^:async extract-models-eager-threading-macro-binding-admits
  (let [cg-flow (await (parser/extract-models
                        "(def t (item-> parcels (geo/buffer 10)))"
                        {:grammar-wasm wasm-path}))
        codes (set (map :code (:diagnostics cg-flow)))
        b (first (:bindings cg-flow))]
    (is (not (contains? codes :non-keyword-data-ref)))
    (is (= 1 (count (:bindings cg-flow))))
    (is (= :threading_macro (:body-shape b)))
    (is (false? (:is-template b)))
    (is (= :parameter_ref (-> b :source :kind)))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
