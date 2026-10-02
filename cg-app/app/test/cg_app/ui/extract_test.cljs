;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.extract-test
  "extract-flow with the real tree-sitter parser of cg core, and
   merge-positions-into-flow, the host half of the overlay codec."
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
    (is (some? bindings))
    (is (= "function_call" (aget (by-name "parcels") "body-shape")))
    (let [thr (by-name "buffered")]
      (is (= "threading_macro" (aget thr "body-shape")))
      (is (= 2 (.-length (aget thr "ops"))))
      (is (= "geo/buffer" (aget (aget (aget thr "ops") 0) "qualified-name"))))))

(deftest merge-positions-full-rf-id-and-bare-id
  (let [flow #js {:bindings
                  #js [#js {:name "parcels" :body-shape "function_call"}
                       #js {:name "buffered" :body-shape "threading_macro"
                            :ops #js [#js {:qualified-name "geo/buffer"}
                                      #js {:qualified-name "geo/simplify"}]}]}
        ;; the keys mix bare ids and full RF ids
        merged (ex/merge-positions-into-flow
                flow #js {"buffer-1" #js [400 250]
                          "buffered__simplify-2" #js [10 20]
                          "binding-parcels" #js [7 8]})
        bs   (aget merged "bindings")
        ops  (aget (aget bs 1) "ops")]
    (is (= 7 (aget (aget (aget (aget bs 0) "visual") "pos") 0)))
    (is (= 400 (aget (aget (aget (aget ops 0) "visual") "pos") 0)))
    (is (= 20 (aget (aget (aget (aget ops 1) "visual") "pos") 1)))
    (is (nil? (aget (aget (aget flow "bindings") 0) "visual")))))

(deftest merge-positions-no-hit-passthrough
  (let [flow #js {:bindings #js [#js {:name "x" :body-shape "function_call"}]}
        merged (ex/merge-positions-into-flow flow #js {})]
    (is (nil? (aget (aget (aget merged "bindings") 0) "visual")))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
