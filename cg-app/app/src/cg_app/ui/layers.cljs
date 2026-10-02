;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.layers
  "Pure session-vars -> sidebar/map-layer mapping. JS-native
   grouping/formatting.")

(def color-palette
  #js ["#4589ff" "#ff832b" "#6fdc8c" "#d12771"
       "#f1c21b" "#08bdba" "#ba4e00" "#a56eff"])

(defn next-color [n]
  (aget color-palette (mod n (.-length color-palette))))

(defn crs-str
  "The CRS display string of a GeoJSON-style {:properties {:name ...}} object
   or a plain string. Returns nil for nil."
  [crs]
  (cond
    (nil? crs) nil
    (string? crs) crs
    :else (let [props (aget crs "properties")]
            (and props (aget props "name")))))

(defn group-vars
  "Session vars -> #js {:datasets [...] :functions [...] :values [...]}."
  [vars]
  (let [vs (or vars #js [])]
    #js {:datasets  (.filter vs (fn [v] (= "dataset" (aget v "type"))))
         :functions (.filter vs (fn [v] (= "function" (aget v "type"))))
         :values    (.filter vs (fn [v] (not (or (= "dataset" (aget v "type"))
                                                 (= "function" (aget v "type"))))))}))

(defn var-subtitle
  "\"12,345 rows  ·  EPSG:26986\". Each half is optional."
  [v]
  (let [rc (aget v "row-count")
        cs (crs-str (aget v "crs"))]
    (str (when rc (str (.toLocaleString rc) " rows"))
         (when (and rc cs) "  ·  ")
         (when cs cs))))
