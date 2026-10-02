;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.op-catalog
  "Static catalog of the built-in cg ops for the toolbar, by source module. User
   ops come from the :operations of the CgFlow, not from here.")

(def builtins
  #js [#js {:id "geo" :label "Geo"
            :ops #js [#js {:name "geo/buffer"}
                      #js {:name "geo/area"}
                      #js {:name "geo/centroid"}
                      #js {:name "geo/simplify"}
                      #js {:name "geo/envelope"}
                      #js {:name "geo/boundary"}
                      #js {:name "geo/union"}
                      #js {:name "geo/intersection"}
                      #js {:name "geo/difference"}
                      #js {:name "geo/sym-difference"}
                      #js {:name "geo/convex-hull"}
                      #js {:name "geo/distance"}
                      #js {:name "geo/length"}
                      #js {:name "geo/intersects?"}
                      #js {:name "geo/contains?"}
                      #js {:name "geo/within?"}
                      #js {:name "geo/touches?"}
                      #js {:name "geo/overlaps?"}
                      #js {:name "geo/crosses?"}
                      #js {:name "geo/covers?"}
                      #js {:name "geo/valid?"}]}
       #js {:id "ds" :label "Dataset"
            :ops #js [#js {:name "ds/transform-crs"}
                      #js {:name "ds/filter"}
                      #js {:name "ds/group-by"}
                      #js {:name "ds/max-by"}
                      #js {:name "ds/min-by"}
                      #js {:name "ds/reduce"}
                      #js {:name "ds/extract-coords-step"}
                      #js {:name "ds/apply-coords-step"}
                      #js {:name "ds/transform-coords-step"}]}
       #js {:id "feature" :label "Feature"
            :ops #js [#js {:name "f/point"}
                      #js {:name "f/linestring"}
                      #js {:name "f/polygon"}]}
       #js {:id "io" :label "I/O"
            :ops #js [#js {:name "ds/load-geojson"}
                      #js {:name "ds/write-geojson"}
                      #js {:name "ds/write-csv"}]}
       #js {:id "threading" :label "Threading"
            :ops #js [#js {:name "item->" :desc "per-item flow"}
                      #js {:name "coll->" :desc "collection-level flow"}
                      #js {:name "table->" :desc "dataset flow"}
                      #js {:name "layers->" :desc "scope iteration (not yet)"}]}])
