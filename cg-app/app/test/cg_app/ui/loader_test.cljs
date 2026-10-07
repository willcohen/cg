;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.loader-test
  "The asset urls of the loader."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/loader.mjs" :as loader]))

(deftest asset-url-resolves-a-package-path
  (is (.endsWith (loader/asset-url "squint-cljs/core.js") "/node_modules/squint-cljs/core.js")))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
