;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.extract-test
  "extract-flow with the real tree-sitter parser of cg core."
  (:require [cljs.test :as t :refer [deftest is]]
            ["node:path" :as path]
            ["../../../src/cg_app/ui/extract.mjs" :as ex]))

(def grammar-wasm
  (path/resolve (js/process.cwd) "../../cg/node_modules/@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm"))

(def fixture
  "(require '[cg.geo :as geo])\n(require '[cg.macros :refer [table->]])\n\n(def parcels (geo/read-file \"parcels.geojson\"))\n\n(def buffered\n  (table-> parcels\n           (geo/buffer 10)\n           (geo/simplify 1.0)))\n")

(deftest ^:async extract-flow-parses-bindings
  (let [flow (await (ex/extract-flow fixture #js {:grammar-wasm grammar-wasm}))
        bindings (aget flow "bindings")
        by-name (fn [n] (.find bindings (fn [b] (= n (aget b "name")))))]
    (is (= "function_call" (aget (by-name "parcels") "body-shape")))
    (let [thr (by-name "buffered")]
      (is (= "threading_macro" (aget thr "body-shape")))
      (is (= 2 (.-length (aget thr "ops"))))
      (is (= "geo/buffer" (aget (aget (aget thr "ops") 0) "qualified-name"))))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
