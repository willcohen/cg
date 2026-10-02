;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.layers-test
  "The pure session-vars -> sidebar/map-layer mapping."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/layers.mjs" :as layers]))

(def vars
  #js [#js {:name "parcels" :type "dataset" :row-count 12345 :crs "EPSG:26986"}
       #js {:name "helper" :type "function"}
       #js {:name "n" :type "number"}
       #js {:name "roads" :type "dataset" :row-count 7
            :crs #js {:properties #js {:name "EPSG:4326"}}}])

(deftest group-vars-by-type
  (let [g (layers/group-vars vars)]
    (is (= 2 (.-length (aget g "datasets"))))
    (is (= 1 (.-length (aget g "functions"))))
    (is (= 1 (.-length (aget g "values"))))
    (is (= "parcels" (aget (aget (aget g "datasets") 0) "name")))))

(deftest palette-cycles
  (is (= (layers/next-color 0) (layers/next-color 8)))
  (is (not= (layers/next-color 0) (layers/next-color 1))))

(deftest crs-str-handles-map-and-string
  (is (= "EPSG:26986" (layers/crs-str "EPSG:26986")))
  (is (= "EPSG:4326" (layers/crs-str #js {:properties #js {:name "EPSG:4326"}})))
  (is (nil? (layers/crs-str nil))))

(deftest var-subtitle-combinations
  (is (.includes (layers/var-subtitle (aget vars 0)) "rows"))
  (is (.includes (layers/var-subtitle (aget vars 0)) "EPSG:26986"))
  (is (= "" (layers/var-subtitle #js {:name "helper" :type "function"})))
  (is (= "EPSG:4326" (layers/var-subtitle #js {:name "x" :type "dataset"
                                               :crs "EPSG:4326"}))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
