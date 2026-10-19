;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.map.sync
  "Pure plans for the MapLibre data-layer sync, GeoJSON bounds and the CRS picker
   filter. The filter is here, because crs_picker.cljs stays out of node tests.")

(def ^:private data-prefix "cgdata-")

(defn plan-sync
  "Existing layer ids + desired layers -> #js {:remove [ids] :upsert [entries]}
   in layer-table order. Touches only cgdata-* layers."
  [existing-layer-ids layers]
  (let [upsert (.filter layers
                        (fn [l] (and (true? (aget l "visible?"))
                                     (aget l "geojson"))))
        visible-vars (js/Set. (.map upsert (fn [l] (aget l "var"))))
        remove-ids (.filter existing-layer-ids
                            (fn [id]
                              (and (.startsWith id data-prefix)
                                   (not (.has visible-vars
                                              (.substring id (.-length data-prefix)))))))]
    #js {:remove remove-ids :upsert upsert}))

(defn- valid-lnglat? [lng lat]
  (and (number? lng) (number? lat)
       (<= -180 lng 180) (<= -90 lat 90)))

(defn bounds-of
  "[[minLng minLat] [maxLng maxLat]] of the FeatureCollections for fitBounds, or
   nil when no coordinate is in range."
  [geojsons]
  (let [b #js [js/Infinity js/Infinity (- js/Infinity) (- js/Infinity)]
        extend! (fn [lng lat]
                  (when (valid-lnglat? lng lat)
                    (when (< lng (aget b 0)) (aset b 0 lng))
                    (when (< lat (aget b 1)) (aset b 1 lat))
                    (when (> lng (aget b 2)) (aset b 2 lng))
                    (when (> lat (aget b 3)) (aset b 3 lat))))
        walk (fn walk [coords]
               (when coords
                 (if (and (number? (aget coords 0)) (number? (aget coords 1)))
                   (extend! (aget coords 0) (aget coords 1))
                   (.forEach coords (fn [c] (walk c))))))]
    (.forEach geojsons
              (fn [fc]
                (.forEach (or (aget fc "features") #js [])
                          (fn [feat]
                            (let [geom (aget feat "geometry")]
                              (when geom (walk (aget geom "coordinates"))))))))
    (when (js/isFinite (aget b 0))
      #js [#js [(aget b 0) (aget b 1)] #js [(aget b 2) (aget b 3)]])))

(def max-crs-results 50)

(defn filter-crs-list
  "A case-insensitive CRS search over name, auth:code and area, with at most
   max-crs-results results."
  [entries query]
  (if (or (nil? query) (= "" query))
    (.slice entries 0 max-crs-results)
    (let [q (.toLowerCase query)
          results #js []]
      (.some entries
             (fn [entry]
               (when (or (.includes (.toLowerCase (or (aget entry "name") "")) q)
                         (.includes (.toLowerCase (str (aget entry "authName") ":"
                                                       (aget entry "code"))) q)
                         (.includes (.toLowerCase (or (aget entry "areaName") "")) q))
                 (.push results entry))
               (>= (.-length results) max-crs-results)))
      results)))
