;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.map.sync-test
  "The pure half of the maplibre layer sync, and the CRS-picker filter."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/map/sync.mjs" :as ms]))

(def fc-a #js {:type "FeatureCollection"
               :features #js [#js {:type "Feature"
                                   :geometry #js {:type "Polygon"
                                                  :coordinates #js [#js [#js [-71 42] #js [-70 42]
                                                                         #js [-70 43] #js [-71 42]]]}}]})
(def fc-b #js {:type "FeatureCollection"
               :features #js [#js {:type "Feature"
                                   :geometry #js {:type "Point"
                                                  :coordinates #js [10 20]}}]})

(deftest plan-sync-add-remove-update
  (let [plan (ms/plan-sync #js ["cgdata-old" "cgdata-keep" "basemap-road"]
                           #js [#js {:var "keep" :visible? true :color "#111" :geojson fc-a}
                                #js {:var "new" :visible? true :color "#222" :geojson fc-b}
                                #js {:var "gone" :visible? false :geojson nil}])]
    (is (= 1 (.-length (aget plan "remove"))))
    (is (= "cgdata-old" (aget (aget plan "remove") 0)))
    (is (= 2 (.-length (aget plan "upsert"))))
    (is (= "keep" (aget (aget (aget plan "upsert") 0) "var")))))

(deftest plan-sync-removes-invisible-existing
  (let [plan (ms/plan-sync #js ["cgdata-gone"]
                           #js [#js {:var "gone" :visible? false :geojson nil}])]
    (is (= 1 (.-length (aget plan "remove"))))
    (is (= 0 (.-length (aget plan "upsert"))))))

(deftest bounds-of-geojsons
  (let [b (ms/bounds-of #js [fc-a fc-b])]
    (is (= -71 (aget (aget b 0) 0)))
    (is (= 20 (aget (aget b 0) 1)))
    (is (= 10 (aget (aget b 1) 0)))
    (is (= 43 (aget (aget b 1) 1)))))

(deftest bounds-nil-on-empty-or-invalid
  (is (nil? (ms/bounds-of #js [])))
  (is (nil? (ms/bounds-of #js [#js {:type "FeatureCollection"
                                    :features #js [#js {:type "Feature"
                                                        :geometry #js {:type "Point"
                                                                       :coordinates #js [999 999]}}]}]))))

(def crs-entries
  #js [#js {:name "NAD83 / Conus Albers" :authName "EPSG" :code "5070" :areaName "United States"}
       #js {:name "WGS 84 / UTM 19N" :authName "EPSG" :code "32619" :areaName "Atlantic"}
       #js {:name "Massachusetts Mainland" :authName "EPSG" :code "26986" :areaName "USA - Massachusetts"}])

(deftest filter-crs-by-name-code-area
  (is (= 1 (.-length (ms/filter-crs-list crs-entries "albers"))))
  (is (= 1 (.-length (ms/filter-crs-list crs-entries "26986"))))
  (is (= 1 (.-length (ms/filter-crs-list crs-entries "usa"))))
  (is (= 1 (.-length (ms/filter-crs-list crs-entries "atlantic"))))
  (is (= 3 (.-length (ms/filter-crs-list crs-entries "")))))

(deftest filter-crs-caps-results
  (let [many (.map (js/Array.from #js {:length 80})
                   (fn [_ i] #js {:name (str "crs-" i) :authName "EPSG"
                                  :code (str i) :areaName "x"}))]
    (is (= 50 (.-length (ms/filter-crs-list many ""))))
    (is (= 50 (.-length (ms/filter-crs-list many "crs"))))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
