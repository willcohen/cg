;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.loader-test
  "overlay-positions, the decode half of the overlay codec, against the real cg
   metadata module. Node injects metadata.mjs: the browser bundle does not resolve here."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../../../cg/src/cg/metadata.mjs" :as md]
            ["../../../src/cg_app/ui/loader.mjs" :as loader]))

(def text-with-overlay
  "(def x 1)\n\n^:cgproj\n{:nodes {:buffer-1 {:pos [400 250]}\n         :parcels {:pos [7 8]}}}\n")

(deftest overlay-positions-decodes-bare-ids
  (let [ps (loader/overlay-positions md text-with-overlay)]
    (is (= 400 (aget (aget ps "buffer-1") 0)))
    (is (= 250 (aget (aget ps "buffer-1") 1)))
    (is (= 7 (aget (aget ps "parcels") 0)))))

(deftest asset-url-resolves-a-package-path
  (is (.endsWith (loader/asset-url "squint-cljs/core.js") "/node_modules/squint-cljs/core.js")))

(deftest overlay-positions-empty-when-absent
  (let [ps (loader/overlay-positions md "(def x 1)\n")]
    (is (zero? (.-length (js/Object.keys ps))))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
