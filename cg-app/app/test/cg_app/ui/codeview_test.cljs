;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.codeview-test
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/codeview.mjs" :as cv]))

(def text "(def buffered\n  (table-> parcels\n           (geo/buffer 10)))\n\n^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}}}")

(def flow
  #js {:overlay #js {:raw "^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}}}"
                     :location #js {:start-line 5 :start-column 1
                                    :end-line 6 :end-column 38}}
       :bindings #js [#js {:name "buffered" :body-shape "threading_macro"
                           :ops #js [#js {:qualified-name "geo/buffer"}]}]})

(deftest nil-flow-placeholder
  (is (= ";; Load a .cg file" (cv/content-for "dsl" nil nil #js {}))))

(deftest dsl-reflects-moved-position
  (let [out (cv/content-for "dsl" text flow {"buffer-1" #js [111 222]})]
    (is (.includes out "(geo/buffer 10)"))
    (is (.includes out "[111 222]"))))

(deftest flow-format-is-json-with-visual
  (let [out (cv/content-for "flow" text flow {"buffered__buffer-1" #js [5 6]})
        parsed (js/JSON.parse out)
        op (aget (aget (aget (aget parsed "bindings") 0) "ops") 0)]
    (is (= 5 (aget (aget (aget op "visual") "pos") 0)))))

(deftest unknown-format
  (is (.startsWith (cv/content-for "nope" text flow #js {}) ";; unknown")))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
