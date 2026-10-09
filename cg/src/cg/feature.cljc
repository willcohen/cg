;; Copyright (c) 2019, 2020, 2024, 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; cg.feature gives one API for geographic features (a geometry and its
;; properties) on the JVM (JTS) and on JavaScript (WasmTS). A geometry can
;; hold its CRS in UserData. User code calls this API and not JTS or WasmTS.

(ns cg.feature
  (:require [clojure.string :as str])
  #?(:clj (:import (org.locationtech.jts.geom Coordinate CoordinateXYZM Geometry
                                              GeometryFactory LinearRing LineString
                                              Point Polygon)
                   (tech.v3.datatype FastStruct))))

#?(:clj (set! *warn-on-reflection* true))

(defonce ^:private operation-registry (atom {}))

;; Registry keys are qualified-name strings ("cg.geo/buffer"), not symbols.
;; (str op-sym) makes one string key from a string or a symbol.
(defn register-operation!
  "Register the metadata map `info` for the operation `op-sym`, a qualified
  name as a string or a symbol. :type is required. The options are
  :fuseable, :cpu-bound and :params, a vector of {:name :required :type}
  for the explicit args. :name is the key that map-positional-to-named
  writes.

  An op of the authored .cg DSL also has the parser keys :dsl-name,
  :dsl-aliases, :kind, :threading-modes, :inputs and :outputs. The
  `bb gen:signatures` snapshot reads them. :inputs includes the threaded
  :role :row receiver, which :params does not."
  [op-sym info]
  (let [op-key (str op-sym)]
    (when-not (str/includes? op-key "/")
      (throw (ex-info "Operation key must be qualified (e.g., \"cg.geo/buffer\")"
                      {:op-sym op-sym})))
    (when-not (:type info)
      (throw (ex-info "Operation info must have :type" {:op-sym op-sym :info info})))
    (when-not (#{:source :transformer :extractor :predicate :selector :sink :step-factory} (:type info))
      (throw (ex-info "Operation :type must be :source, :transformer, :extractor, :predicate, :selector, :sink, or :step-factory"
                      {:op-sym op-sym :type (:type info)})))
    (let [derived-shape (or (:row-shape info)
                            (cond
                              (= :source (:type info)) :source
                              (= :sink   (:type info)) :terminal
                              (some #(= :row (:role %)) (:params info)) :per-row
                              :else :whole-dataset))
          enriched (assoc info
                          :row-shape derived-shape
                          :reads (or (:reads info) #{})
                          :writes (or (:writes info) #{}))]
      (swap! operation-registry assoc op-key enriched))))

(defn get-operation-info
  "Return the metadata of a registered operation, or nil."
  [op-sym]
  (get @operation-registry (str op-sym)))

(defn all-operations
  "Return the operation registry as a map of qualified name to info."
  []
  @operation-registry)

(defn reducer?
  "Return true when op-sym is a registered operation with :role :reducer,
  and false otherwise."
  [op-sym]
  (= :reducer (:role (get-operation-info op-sym))))

;; Maps the bare name of an op outside cg.geo to its host namespace, for
;; example "coalesce" to "cg.dataset". The map is here, a leaf namespace,
;; because cg.run requires cg.macros and both must read it.
;; cg.macros/parse-when-row-form turns `when` into `when-row`, and only
;; `when-row` is here.
(defonce ^:private bare-op-namespaces
  (atom {"coalesce"       "cg.dataset"
         "filter"         "cg.dataset"
         "group-by"       "cg.dataset"
         "partition-by"   "cg.dataset"
         "map"            "cg.dataset"
         "into"           "cg.dataset"
         "max-by"         "cg.dataset"
         "min-by"         "cg.dataset"
         "reduce"         "cg.dataset"
         "summarize"      "cg.dataset"
         "block-presence" "cg.dataset"
         "report-table"   "cg.dataset"
         "map-layer"      "cg.dataset"
         "aggregate"      "cg.run"
         "when-row"       "cg.run"}))

(defn register-bare-op-namespace!
  "Register ns-name as the host namespace of the bare op name op-name."
  [op-name ns-name]
  (swap! bare-op-namespaces assoc (str op-name) (str ns-name)))

(defn get-bare-op-namespace
  "Return the host namespace string of a bare op name, or nil."
  [op-name]
  (get @bare-op-namespaces (str op-name)))

;; Maps a qualified-name string ("ns/name") to a fn value.
;; cg.run/resolve-operation reads it before requiring-resolve, because
;; requiring-resolve cannot find a fn that a .cg file defines in SCI. The map
;; is here, a leaf namespace, because a cg.geo -> cg.run require makes the
;; cycle geo -> run -> dataset -> geo.
(defonce ^:private bare-op-fns (atom {}))

(defn register-bare-op-fn!
  "Register fn-value under \"<ns-name>/<op-name>\", leaving bare-op-namespaces
   unchanged."
  [ns-name op-name fn-value]
  (swap! bare-op-fns assoc (str ns-name "/" op-name) fn-value))

(defn get-bare-op-fn
  "Return the fn registered for a qualified \"ns/name\" string, or nil."
  [qualified-name]
  (get @bare-op-fns qualified-name))

(defonce ^:private group-registry (atom {}))

(defn register-group!
  "Register group-sym as one DSL operation that expands to several flow steps.
  info has :params and :expansion-fn, a fn of the params map that returns
  {:nodes [...] :edges [...] :entry <id> :exit <id>}."
  [group-sym info]
  (let [group-key (str group-sym)]
    (when-not (str/includes? group-key "/")
      (throw (ex-info "Group key must be qualified (e.g., \"cg.dataset/transform-crs\")"
                      {:group-sym group-sym})))
    (when-not (:expansion-fn info)
      (throw (ex-info "Group info must have :expansion-fn" {:group-sym group-sym :info info})))
    (when-not (fn? (:expansion-fn info))
      (throw (ex-info "Group :expansion-fn must be a function"
                      {:group-sym group-sym :expansion-fn (:expansion-fn info)})))
    (swap! group-registry assoc group-key info)))

(defn get-group-info
  "Return the metadata of a registered group, or nil."
  [group-sym]
  (get @group-registry (str group-sym)))

(defn all-groups
  "Return the group registry as a map of qualified name to info."
  []
  @group-registry)

;; A wasmts PreparedGeometry is a plain object that holds the prepared
;; handle and has no methods, and the map? of squint is true for it. Here it
;; is a raw value and not a feature map, as a JTS PreparedGeometry is on the
;; JVM.
#?(:cljs
   (defn prepared-geometry?
     "True for a wasmts PreparedGeometry."
     [x]
     (and (some? x) (some? (aget x "_jtsPreparedGeometry")))))

(defn is-geometry?
  "Return true if x is a JTS Geometry (JVM) or a WasmTS geometry (JS)."
  [x]
  #?(:clj (instance? Geometry x)
     ;; The own property of each WasmTS geometry object. A call of
     ;; getGeometryType goes into WASM and copies a string back, and to-jts
     ;; asks this for each argument of each geo call.
     :cljs (and (some? x) (some? (aget x "_jtsGeom")))))

;; A geometry holds its CRS in UserData: an EPSG code ("EPSG:4326"), a PROJ
;; string or PROJJSON.

(defn set-crs
  "Set the CRS of a geometry in its UserData. Return the geometry."
  [g crs]
  #?(:clj (.setUserData ^Geometry g crs)
     :cljs (set! (.-userData g) crs))
  g)

(defn get-crs
  "Return the CRS from the UserData of a geometry."
  [g]
  #?(:clj (.getUserData ^Geometry g)
     :cljs (.-userData g)))

(defn geometry-fields
  "Return geometry fields from a row (map or sequence)"
  [row]
  (cond (map? row)
        (filter #(is-geometry? (val %)) row)
        (seqable? row)
        (filter is-geometry? row)))

(defn get-geometry-field-meta [f]
  (:geometry-field (meta f)))

(defn normalized-map?
  "Check if map has the normalized structure {:geometry, :properties}"
  [m]
  (= (into #{} (keys m)) #{:geometry :properties}))

(defn get-map-structure
  "Return :normalized, :one-geom-with-properties-map, :one-geom-with-flat-properties,
  :meta-geometry-value-utilized-and-required (:geometry-field metadata) or :unknown."
  [m]
  (let [gf (geometry-fields m)
        gfm (get-geometry-field-meta m)]
    (cond
      (normalized-map? m)
      :normalized
      (= 1 (count gf))
      (let [remainder (dissoc m (key (first gf)))]
        (if (and (= 1 (count remainder))
                 (= 1 (count (vals remainder)))
                 (>= (count (first (vals remainder))) 1))
          :one-geom-with-properties-map
          :one-geom-with-flat-properties))
      (not (nil? gfm))
      :meta-geometry-value-utilized-and-required
      :else
      :unknown)))

(defn get-geometry-from-map [m]
  (case (get-map-structure m)
    :normalized
    (:geometry m)
    (:one-geom-with-properties-map :one-geom-with-flat-properties)
    (get m (key (first (geometry-fields m))))
    :meta-geometry-value-utilized-and-required
    (get m (get-geometry-field-meta m))))

(defn get-geometry-field-name-from-map [m]
  (case (get-map-structure m)
    :normalized
    :geometry
    (:one-geom-with-properties-map :one-geom-with-flat-properties)
    (key (first (geometry-fields m)))
    :meta-geometry-value-utilized-and-required
    (get-geometry-field-meta m)))

(defn get-properties-from-map [m]
  (case (get-map-structure m)
    :normalized
    (:properties m)
    :one-geom-with-properties-map
    (first (vals (dissoc m (key (first (geometry-fields m))))))
    :one-geom-with-flat-properties
    (dissoc m (key (first (geometry-fields m))))
    :meta-geometry-value-utilized-and-required
    (dissoc m (get-geometry-field-name-from-map m))))

(defn- map-assoc-properties [m p]
  (case (get-map-structure m)
    :normalized
    (assoc m :properties p)
    :one-geom-with-properties-map
    (assoc m (first (keys (dissoc m (key (first (geometry-fields m)))))) p)
    :one-geom-with-flat-properties
    (merge (apply dissoc m (keys (dissoc m (key (first (geometry-fields m)))))) p)
    :meta-geometry-value-utilized-and-required
    (merge (apply dissoc m (keys (dissoc m (get-geometry-field-name-from-map m)))) p)))

#?(:clj
   (defprotocol SimpleFeaturelike
     "The JVM dispatch of the feature fns. On JS, each fn tests the type of its
     argument."
     (-to-jts [this])
     (-to-feature [this] [this properties])
     (-geometry [this])
     (-properties [this])
     (-assoc-geometry [this s])
     (-assoc-properties [this p])))

#?(:clj
   (extend-protocol SimpleFeaturelike
     Geometry
     (-to-jts [this] this)
     (-to-feature
       ([this] (-to-feature this {}))
       ([this properties] {:geometry this :properties properties}))
     (-geometry [this] this)
     (-properties [_this] {})
     (-assoc-geometry [_this s] s)
     (-assoc-properties [this p] (-to-feature this p))))

;; Extend only these 3 classes. cg.dataset/ds-featurelike-columns finds the
;; feature columns with satisfies?, and a wider extension changes its result.
#?(:clj
   (let [map-impl {:-to-jts (fn [m] (-to-jts (get-geometry-from-map m)))
                   :-to-feature (fn
                                  ([m] m)
                                  ([m properties]
                                   {(get-geometry-field-name-from-map m) (get-geometry-from-map m)
                                    :properties properties}))
                   :-geometry get-geometry-from-map
                   :-properties get-properties-from-map
                   :-assoc-geometry (fn [m s] (assoc m (get-geometry-field-name-from-map m) s))
                   :-assoc-properties map-assoc-properties}]
     (doseq [c [clojure.lang.PersistentArrayMap clojure.lang.PersistentHashMap FastStruct]]
       (extend c SimpleFeaturelike map-impl))))

(defn to-jts
  "Return the geometry of a feature map. A geometry gives itself. On JS, a
  prepared geometry also gives itself."
  [this]
  #?(:clj (-to-jts this)
     :cljs (if (or (is-geometry? this) (prepared-geometry? this))
             this
             (get-geometry-from-map this))))

(defn to-feature
  "A geometry `g` gives {:geometry g :properties {}}. A feature map gives itself.
  With `properties`, the result holds them under :properties."
  ([this]
   #?(:clj (-to-feature this)
      :cljs (if (is-geometry? this)
              {:geometry this :properties {}}
              this)))
  ([this properties]
   #?(:clj (-to-feature this properties)
      :cljs (if (is-geometry? this)
              {:geometry this :properties properties}
              {(get-geometry-field-name-from-map this) (get-geometry-from-map this)
               :properties properties}))))

(defn geometry [this]
  #?(:clj (-geometry this)
     :cljs (if (is-geometry? this)
             this
             (get-geometry-from-map this))))

(defn properties
  "Return the properties of a SimpleFeaturelike. A geometry gives an empty
  map."
  [this]
  #?(:clj (-properties this)
     :cljs (if (is-geometry? this)
             {}
             (get-properties-from-map this))))

(defn assoc-geometry [this s]
  #?(:clj (-assoc-geometry this s)
     :cljs (if (is-geometry? this)
             s
             (assoc this (get-geometry-field-name-from-map this) s))))

(defn assoc-properties [this p]
  #?(:clj (-assoc-properties this p)
     :cljs (if (is-geometry? this)
             {:geometry this :properties p}
             (map-assoc-properties this p))))

#?(:clj
   (do
     (defn- ->coordinate [c]
       (case (count c)
         2 (Coordinate. (nth c 0) (nth c 1))
         3 (Coordinate. (nth c 0) (nth c 1) (nth c 2))
         4 (CoordinateXYZM. (nth c 0) (nth c 1) (nth c 2) (nth c 3))
         (throw (ex-info "Coordinate must have 2, 3, or 4 dimensions" {:coord c}))))

     (defn- ->coordinates ^"[Lorg.locationtech.jts.geom.Coordinate;" [coords]
       (into-array Coordinate (map ->coordinate coords)))

     (defn point
       "Create a Point geometry from x y, x y z, or x y z m."
       ([x y] (.createPoint (GeometryFactory.) (Coordinate. x y)))
       ([x y z] (.createPoint (GeometryFactory.) (Coordinate. x y z)))
       ([x y z m] (.createPoint (GeometryFactory.) (CoordinateXYZM. x y z m))))

     (defn linestring
       "Create a LineString geometry from a sequence of [x y], [x y z] or
       [x y z m] vectors."
       [coords]
       (.createLineString (GeometryFactory.) (->coordinates coords)))

     (defn polygon
       "Create a Polygon geometry from the exterior coordinates and optional
       holes. holes is a sequence of rings."
       ([exterior-coords]
        (polygon exterior-coords nil))
       ([exterior-coords holes]
        (let [gf (GeometryFactory.)
              shell (.createLinearRing gf (->coordinates exterior-coords))
              ^"[Lorg.locationtech.jts.geom.LinearRing;" hole-rings
              (when holes
                (into-array LinearRing
                            (map #(.createLinearRing gf (->coordinates %)) holes)))]
          (if hole-rings
            (.createPolygon gf shell hole-rings)
            (.createPolygon gf shell))))))
   :cljs
   (do
     ;; wasmts.geom.fromFlat builds a geometry from a flat Float64Array of
     ;; ordinates. `dim` selects how many ordinates carry meaning. ringOffsets
     ;; index the coordinates and partOffsets index the rings, and each buffer
     ;; holds one more entry than the count of the items it delimits.

     (defn- coord-dim
       "Return the ordinate count (2, 3 or 4) of the first coordinate."
       [coords]
       (let [c (first coords)
             n (if c (count c) 0)]
         (cond
           (>= n 4) 4
           (= n 3)  3
           :else    2)))

     (defn- push-flat!
       "Append the ordinates of each coordinate in `coords` to `out`. A missing
        ordinate becomes 0."
       [out coords dim]
       (doseq [c coords]
         (dotimes [i dim]
           (.push out (or (nth c i nil) 0)))))

     (defn- flat-geometry
       "Build a geometry of `type` from one group of coordinates."
       [type coords]
       (let [dim (coord-dim coords)
             out #js []]
         (push-flat! out coords dim)
         (.fromFlat (.. js/globalThis -wasmts -geom)
                    type (.from js/Float64Array out) dim nil nil)))

     (defn- flat-ring-geometry
       "Build a geometry of `type` from a sequence of coordinate groups. Each
        group is a ring of a polygon, or one line of a multilinestring."
       [type rings]
       (let [dim   (coord-dim (first rings))
             out   #js []
             offs  #js [0]]
         (doseq [r rings]
           (push-flat! out r dim)
           (.push offs (/ (.-length out) dim)))
         (.fromFlat (.. js/globalThis -wasmts -geom)
                    type (.from js/Float64Array out) dim
                    (.from js/Int32Array offs) nil)))

     (defn point
       "Create a Point geometry from x y, x y z, or x y z m."
       ([x y]     (flat-geometry "Point" [[x y]]))
       ([x y z]   (flat-geometry "Point" [[x y z]]))
       ;; The flat buffer is the only route that keeps the measure: wasmts
       ;; alpha6 has no CoordinateXYZM on the minted cg.geo surface.
       ([x y z m] (flat-geometry "Point" [[x y z m]])))

     (defn linestring
       "Create a LineString geometry from a sequence of coordinates."
       [coords]
       (flat-geometry "LineString" coords))

     (defn polygon
       "Create a Polygon geometry from the exterior ring and optional hole
       rings."
       ([exterior]
        (polygon exterior nil))
       ([exterior holes]
        (flat-ring-geometry "Polygon" (cons exterior (or holes [])))))))

#?(:clj
   (do
     (defn multi-point
       "Group Point geometries into a MultiPoint."
       [points]
       (.createMultiPoint (GeometryFactory.)
                          ^"[Lorg.locationtech.jts.geom.Point;"
                          (into-array Point points)))

     (defn multi-linestring
       "Group LineString geometries into a MultiLineString."
       [lines]
       (.createMultiLineString (GeometryFactory.)
                               ^"[Lorg.locationtech.jts.geom.LineString;"
                               (into-array LineString lines)))

     (defn multi-polygon
       "Group Polygon geometries into a MultiPolygon."
       [polygons]
       (.createMultiPolygon (GeometryFactory.)
                            ^"[Lorg.locationtech.jts.geom.Polygon;"
                            (into-array Polygon polygons))))

   :cljs
   (do
     ;; The generated wasmts factory accepts a JS array where JTS takes a Java
     ;; array. A conversion of each element is not necessary.
     (defn- create-multi
       [method geoms]
       (let [GF (.. js/globalThis -wasmts -geom -GeometryFactory)]
         ((aget GF method) (.create0 GF) (to-array geoms))))

     (defn multi-point
       "Group Point geometries into a MultiPoint."
       [points]
       (create-multi "createMultiPoint" points))

     (defn multi-linestring
       "Group LineString geometries into a MultiLineString."
       [lines]
       (create-multi "createMultiLineString" lines))

     (defn multi-polygon
       "Group Polygon geometries into a MultiPolygon."
       [polygons]
       (create-multi "createMultiPolygon" polygons))))

(register-operation! "cg.feature/point"
                     {:type :source :fuseable true :cpu-bound false})

(register-operation! "cg.feature/linestring"
                     {:type :source :fuseable true :cpu-bound false})

(register-operation! "cg.feature/polygon"
                     {:type :source :fuseable true :cpu-bound false})

(defn make-feature
  "Create a feature map from a geometry and optional properties."
  ([geom]
   (make-feature geom {}))
  ([geom props]
   {:geometry geom :properties props}))

#?(:clj
   (defn coordinates
     "Return the coordinates of a geometry or a feature as a vector of
     [x y], [x y z] or [x y z m] vectors."
     [geom]
     (mapv (fn [^Coordinate c]
             (let [x (.getX c)
                   y (.getY c)
                   z (.getZ c)
                   m (.getM c)]
               (cond
                 (not (Double/isNaN m)) [x y z m]
                 (not (Double/isNaN z)) [x y z]
                 :else [x y])))
           (.getCoordinates ^Geometry (geometry geom))))
   :cljs
   (defn coordinates
     "Return the coordinates of a geometry or a feature as a vector of
     [x y] or [x y z] vectors."
     [geom]
     ;; The flat array avoids a long-lived proxy for each Coordinate.
     (let [flat (js/globalThis.wasmts.geom.getCoordinatesFlat (geometry geom) 3)]
       (mapv (fn [i]
               (let [x (aget flat (* 3 i))
                     y (aget flat (+ 1 (* 3 i)))
                     z (aget flat (+ 2 (* 3 i)))]
                 (if (js/isNaN z)
                   [x y]
                   [x y z])))
             (range (quot (.-length flat) 3))))))

(defn point? [geom]
  #?(:clj (instance? org.locationtech.jts.geom.Point (geometry geom))
     :cljs (= (.getGeometryType (geometry geom)) "Point")))

(defn linestring? [geom]
  #?(:clj (instance? org.locationtech.jts.geom.LineString (geometry geom))
     :cljs (= (.getGeometryType (geometry geom)) "LineString")))

(defn polygon? [geom]
  #?(:clj (instance? org.locationtech.jts.geom.Polygon (geometry geom))
     :cljs (= (.getGeometryType (geometry geom)) "Polygon")))

(defn multi-point? [geom]
  #?(:clj (instance? org.locationtech.jts.geom.MultiPoint (geometry geom))
     :cljs (= (.getGeometryType (geometry geom)) "MultiPoint")))

(defn multi-linestring? [geom]
  #?(:clj (instance? org.locationtech.jts.geom.MultiLineString (geometry geom))
     :cljs (= (.getGeometryType (geometry geom)) "MultiLineString")))

(defn multi-polygon? [geom]
  #?(:clj (instance? org.locationtech.jts.geom.MultiPolygon (geometry geom))
     :cljs (= (.getGeometryType (geometry geom)) "MultiPolygon")))

(defn geometry-collection? [geom]
  #?(:clj (instance? org.locationtech.jts.geom.GeometryCollection (geometry geom))
     :cljs (= (.getGeometryType (geometry geom)) "GeometryCollection")))
