;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; cg.geo is the Clojure surface over JTS: org.locationtech.jts.* on the JVM
;; and wasmts (JTS in WebAssembly) on JS. The method names and the results
;; are the same on the two platforms. For a signature that this ns does not
;; show, read the JTS javadoc at
;; https://locationtech.github.io/jts/javadoc/
;;
;; (define-all-ts-fns) reads src/cg/ts_registry.cljc at compile time
;; and emits one defn for each in-scope entry. Each defn calls the dispatcher
;; for its shape: dispatch-receiver-call, dispatch-static-call or
;; dispatch-ctor.
;;
;; Do not require cg.dataset here. cg.dataset requires cg.geo, and a require
;; in this direction makes the cycle geo -> dataset -> geo.
(ns cg.geo
  ;; contains?, empty? and reverse are minted below and shadow clojure.core.
  (:refer-clojure :exclude [contains? empty? reverse])
  (:require [cg.feature :as feature]
            [cg.ts-macros :refer [define-all-ts-fns check-data
                                  geometry-subtypes]]
            [clojure.string :as str]
            #?(:clj [net.willcohen.proj.proj :as proj])
            #?(:cljs [proj-wasm :as proj]))
  #?(:clj (:import (org.locationtech.jts.geom Coordinate
                                              CoordinateSequence
                                              CoordinateSequenceFilter
                                              Geometry
                                              GeometryFactory
                                              PrecisionModel)
                   (org.locationtech.jts.operation.buffer
                    BufferOp
                    BufferParameters
                    OffsetCurveBuilder)
                   (org.locationtech.jts.operation.linemerge LineMerger)
                   (org.locationtech.jts.operation.union CascadedPolygonUnion))))

#?(:clj (set! *warn-on-reflection* true))

;; The minted `buffer` refers to buffer-params, which is defined after the
;; expansion because it calls the minted buffer-parameters-create.
(declare buffer-params)

(defn geometry-typed?
  "Return true when the type string names a class in the Geometry hierarchy."
  [type-str]
  (boolean (and type-str (clojure.core/contains? geometry-subtypes type-str))))

(defn ^:private coerce-args
  "Resolve each geometry-typed arg, a raw geometry or a feature map, to a raw
   geometry. The other args pass through."
  [param-types args]
  (mapv (fn [type-str a]
          (cond
            (geometry-typed? type-str) (feature/to-jts a)
            :else a))
        param-types
        args))

(defn coords->pairs
  "Convert a Coordinate array to a vector of [x y] pairs. On the JVM the
   elements are Coordinate. On JS they are {x, y, z?, m?} objects."
  [arr]
  #?(:clj  (mapv (fn [^Coordinate c] [(.getX c) (.getY c)]) arr)
     :cljs (mapv (fn [c] [(.-x c) (.-y c)]) arr)))

#?(:cljs
   (defn ^:private invoke-instance-js
     "Call a named method on a wasmts geometry. A prepared geometry goes as the
      first arg to the function of that name in wasmts.geom.prep.PreparedGeometry."
     [receiver ^string method-name args]
     (let [f (aget receiver method-name)
           call-args (let [arr #js []]
                       (doseq [a args] (.push arr a))
                       arr)]
       (cond
         f (.apply f receiver call-args)
         (feature/prepared-geometry? receiver)
         (let [pf (aget js/globalThis.wasmts.geom.prep.PreparedGeometry method-name)]
           (.unshift call-args receiver)
           (.apply pf nil call-args))
         :else (throw (js/Error. (str "No method " method-name " on " receiver)))))))

#?(:cljs
   (defn ^:private invoke-js-path
     "Call the function at a dotted path under globalThis with args."
     [^string path args]
     (let [parts  (.split path ".")
           n      (.-length parts)
           parent (loop [i 0 acc js/globalThis]
                    (if (>= i (dec n))
                      acc
                      (recur (inc i) (aget acc (aget parts i)))))
           fname  (aget parts (dec n))
           f      (aget parent fname)
           call-args (let [arr #js []]
                       (doseq [a args] (.push arr a))
                       arr)]
       (.apply f parent call-args))))

(defn dispatch-receiver-call
  "Dispatch a :receiver-call registry entry `meta`. A feature-map receiver gets
   its Geometry result put back into the feature map."
  [meta #?(:clj invoke) feat args]
  (let [class-fqn (:class meta)
        geom-receiver? (geometry-typed? class-fqn)
        g       (if (and geom-receiver? (map? feat))
                  (feature/to-jts feat)
                  feat)
        coerced (coerce-args (:params meta) args)
        result  #?(:clj  (invoke g coerced)
                   :cljs (invoke-instance-js g (:method meta) coerced))
        return-type (-> meta :returns :type)]
    (if (and geom-receiver? (geometry-typed? return-type) (map? feat))
      (feature/assoc-geometry feat result)
      result)))

(defn ^:private first-geom-feature
  "Return the first arg that has a Geometry parameter type and is a feature
   map."
  [param-types args]
  (some (fn [[type-str a]]
          (when (and (geometry-typed? type-str) (map? a)) a))
        (map vector param-types args)))

(defn dispatch-static-call
  "Dispatch a :static-call registry entry. A Geometry result goes into the first
   Geometry arg that is a feature map."
  [meta #?(:clj invoke) args]
  (let [coerced (coerce-args (:params meta) args)
        anchor  (first-geom-feature (:params meta) args)
        result  #?(:clj  (invoke coerced)
                   :cljs (invoke-js-path (:js-path meta) coerced))
        return-type (-> meta :returns :type)]
    (if (and (geometry-typed? return-type) anchor)
      (feature/assoc-geometry anchor result)
      result)))

(defn dispatch-ctor
  "Dispatch a :ctor registry entry and return the new instance, such as a
   BufferOp, outside any feature."
  [meta #?(:clj invoke) args]
  (let [coerced (coerce-args (:params meta) args)]
    #?(:clj  (invoke coerced)
       :cljs (invoke-js-path (:js-path meta) coerced))))

;; The dataset branch of GeoInput is in cg.dataset, which requires this ns.
;; The protocol is here, below each consumer, and the minted ops can call
;; -geo-map with no require cycle. Only the dataset branch uses `col-name`.

(defprotocol GeoInput
  "Lift a single-feature op `f` over a geo-op input `x`: apply directly to a
   lone feature/geometry, map over a collection, or (dataset branch, in
   cg.dataset) add a new `col-name` column of results."
  (-geo-map [x f col-name]))

#?(:clj
   (extend-protocol GeoInput
     clojure.lang.Sequential (-geo-map [x f _col-name] (mapv f x))
     Object                  (-geo-map [x f _col-name] (f x)))
   :cljs
   (do
     (extend-type js/Array GeoInput (-geo-map [x f _col-name] (mapv f x)))
     (extend-type js/Object GeoInput (-geo-map [x f _col-name] (f x)))))

;; The minted union and intersection call this on the result feature
;; (ts-macros :prop-strategy). It is public because squint resolves the minted
;; reference as a property of the namespace object.

(defn apply-prop-strategy
  "Set the properties of `result` from features a and b by `strategy`:
   :keep-first (default), :keep-last or :merge (a, then b)."
  [result a b strategy]
  (if (and (map? result) (map? a) (map? b))
    (cond
      (= strategy :keep-last) (feature/assoc-properties result (feature/properties b))
      (= strategy :merge)     (feature/assoc-properties
                               result (merge (feature/properties a) (feature/properties b)))
      :else                   result)
    result))

(defn offset-line
  "Return a line parallel to the input at distance, to the left for a positive
   distance and to the right for a negative one."
  [feat distance]
  #?(:clj
     (let [geom (feature/geometry feat)
           gf (GeometryFactory.)
           coords (.getCoordinates ^Geometry geom)
           ocb (OffsetCurveBuilder. (PrecisionModel.) (BufferParameters.))
           offset-coords (.getOffsetCurve ocb coords distance)]
       (.createLineString gf offset-coords))
     :cljs
     (let [geom (feature/geometry feat)]
       ;; wasmts has OffsetCurve.getCurve, not OffsetCurveBuilder.
       (js/globalThis.wasmts.operation.buffer.OffsetCurve.getCurve geom distance))))

#?(:clj
   (defn ^:private offset-polygon
     "A polygon from an offset curve (JVM only): the original line connected to
      its offset LineString in a closed ring."
     [feat distance]
     (let [gf (GeometryFactory.)
           coords (.getCoordinates ^Geometry (feature/geometry feat))
           ocb (OffsetCurveBuilder. (PrecisionModel.) (BufferParameters.))
           offset-coords (.getOffsetCurve ocb coords distance)
           ring-coords (into [] (concat coords
                                        (clojure.core/reverse offset-coords)
                                        [(first coords)]))
           ^"[Lorg.locationtech.jts.geom.Coordinate;" coord-array
           (into-array Coordinate ring-coords)
           ^CoordinateSequence coord-seq
           (.create (.getCoordinateSequenceFactory gf) coord-array)]
       (.createPolygon gf coord-seq))))

(defn offset-line-string
  "Offset the line by distance. geom-type :line returns the LineString; :polygon
   (JVM only) returns the Polygon between the line and its offset."
  [feat distance geom-type]
  #?(:clj (try (case geom-type
                 :polygon (offset-polygon feat distance)
                 :line (offset-line feat distance))
               (catch Exception _ nil))
     :cljs (case geom-type
             :polygon (throw (js/Error. "offset-polygon not yet implemented in CLJS"))
             :line (offset-line feat distance))))

(defn one-sided-buffer
  "Return a one-sided buffer of a feature. JVM only."
  #_{:clj-kondo/ignore [:unused-binding]}
  [feat dist]
  #?(:clj (let [geom (feature/geometry feat)
                params (doto (BufferParameters.) (.setSingleSided true))
                buffered (BufferOp/bufferOp ^Geometry geom (double dist) params)]
            (if (map? feat)
              (feature/assoc-geometry feat buffered)
              buffered))
     :cljs (throw (js/Error. "Single-sided buffer not yet implemented in CLJS"))))

(defn left-buffer
  "A left-side buffer (positive single-sided distance)."
  #_{:clj-kondo/ignore [:unused-binding]}
  [feat dist]
  #?(:clj (one-sided-buffer feat (Math/abs (double dist)))
     :cljs (throw (js/Error. "Left buffer not yet implemented in CLJS"))))

(defn right-buffer
  "A right-side buffer (negative single-sided distance)."
  #_{:clj-kondo/ignore [:unused-binding]}
  [feat dist]
  #?(:clj (one-sided-buffer feat (- (Math/abs (double dist))))
     :cljs (throw (js/Error. "Right buffer not yet implemented in CLJS"))))

(defn merge-lines
  "Merge multiple linestrings into connected lines, then union them.
   Assumes the input MultiLineStrings are all one segment and contiguous."
  [features]
  #?(:clj (let [lm (LineMerger.)
                geoms (map #(.getGeometryN ^Geometry (feature/geometry %) 0) features)]
            (doseq [g geoms] (.add lm ^Geometry g))
            (CascadedPolygonUnion/union
             (into [] (map feature/to-jts) (.getMergedLineStrings lm))))
     :cljs (let [merger (js/globalThis.wasmts.operation.linemerge.LineMerger.create)]
             (doseq [f features]
               (js/globalThis.wasmts.operation.linemerge.LineMerger.add
                merger (feature/geometry f)))
             ;; merged is a wasmts array, and the union takes it with no
             ;; conversion.
             (let [merged (js/globalThis.wasmts.operation.linemerge.LineMerger.getMergedLineStrings merger)]
               (js/globalThis.wasmts.operation.union.CascadedPolygonUnion.union merged)))))

;; transform-geom uses clj-proj (net.willcohen.proj.proj on the JVM, proj-wasm
;; on JS), not JTS.

#?(:clj
   (do
     (def ^:private proj-init-future
       (future (proj/init!) :proj-initialized))

     (defn init-proj!
       "Block until the background PROJ init completes."
       []
       @proj-init-future
       true)

     (defn proj-ready?
       "True when PROJ is initialized and ready."
       []
       (realized? proj-init-future))))

#?(:cljs
   (do
     (defonce ^:private proj-initialized? (atom false))
     (defonce ^:private proj-init-promise (atom nil))

     (defn init-proj!
       "Initialize PROJ (WASM). Returns a Promise that resolves when PROJ is
        ready. A second call does not initialize again."
       []
       (if @proj-initialized?
         (js/Promise.resolve true)
         (if-let [existing @proj-init-promise]
           existing
           (let [promise (-> (proj/init!)
                             (.then (fn [_] (reset! proj-initialized? true) true)))]
             (reset! proj-init-promise promise)
             promise))))

     (defn proj-ready?
       "True when PROJ is initialized and ready."
       []
       @proj-initialized?)

     (defn shutdown-proj!
       "Shut down the PROJ worker pool that init-proj! started. Returns a Promise
        of false if init-proj! did not run."
       []
       (if @proj-initialized?
         (-> (proj/shutdown!)
             (.then (fn [_]
                      (reset! proj-initialized? false)
                      (reset! proj-init-promise nil)
                      true)))
         (js/Promise.resolve false)))

     (defonce ^:private transformer-cache (atom {}))))

#?(:clj
   (do
     (def ^:private ^ThreadLocal thread-local-transformers
       (ThreadLocal/withInitial (reify java.util.function.Supplier
                                  (get [_] (atom {})))))

     (defn- normalize-crs
       "Normalize a CRS value to a PROJ-parseable string. Handles PROJJSON
        fragment maps produced by JTS GeoJsonWriter."
       [crs]
       (cond
         (string? crs) crs
         (and (map? crs) (= "name" (:type crs)))
         (get-in crs [:properties :name])
         (map? crs) ((requiring-resolve 'charred.api/write-json-str) crs)
         :else (str crs)))

     (defn get-or-create-transformer
       "Return {:ctx <context> :tx <transformer>} for the CRS pair, cached per
        thread, with lon/lat axis order. Blocks until the PROJ init completes."
       [source-crs target-crs]
       @proj-init-future
       (let [source-crs (normalize-crs source-crs)
             target-crs (normalize-crs target-crs)
             cache @(.get thread-local-transformers)
             cache-key [source-crs target-crs]]
         (or (get cache cache-key)
             (let [ctx (proj/context-create)
                   raw-tx (proj/proj-create-crs-to-crs {:context ctx
                                                        :source_crs source-crs
                                                        :target_crs target-crs})
                   ;; Keep the normalize: a raw transformer reads [lon lat] as
                   ;; [lat lon] for EPSG:4326, and GeoJSON and the map
                   ;; renderers use lon/lat.
                   tx (proj/proj-normalize-for-visualization {:context ctx :obj raw-tx})]
               (when (nil? tx)
                 (throw (ex-info "Failed to create CRS transformer"
                                 {:source-crs source-crs :target-crs target-crs})))
               (swap! (.get thread-local-transformers) assoc cache-key {:ctx ctx :tx tx})
               {:ctx ctx :tx tx}))))

     (defn extract-geometry-coords
       "Extract all coordinates from a JTS geometry as [[x y z m] ...].
        Returns {:coords [...] :count n}."
       [^Geometry geom]
       (let [jts-coords (.getCoordinates geom)
             n (alength jts-coords)]
         {:coords (mapv (fn [^Coordinate c]
                          [(.getX c) (.getY c)
                           (if (Double/isNaN (.getZ c)) 0.0 (.getZ c))
                           0.0])
                        jts-coords)
          :count n}))

     (defn read-coord-array
       "Read coordinates back from a PROJ coord array after transformation."
       [coord-array n]
       (mapv (fn [i]
               [(get-in coord-array [i 0])
                (get-in coord-array [i 1])
                (get-in coord-array [i 2])
                (get-in coord-array [i 3])])
             (range n)))

     (defn- apply-coord-item!
       "Apply one transformed coordinate to a CoordinateSequence item."
       [^CoordinateSequence cseq ^Integer i coord]
       (let [[x y z m] coord
             dim (.getDimension cseq)
             has-m (.hasM cseq)]
         (.setOrdinate cseq i 0 (double x))
         (.setOrdinate cseq i 1 (double y))
         (when (>= dim 3)
           (.setOrdinate cseq i 2 (double (or z 0.0))))
         (when (and has-m (>= dim 4))
           (.setOrdinate cseq i 3 (double (or m 0.0))))))

     (defn- apply-coords-filter
       "A CoordinateSequenceFilter that writes transformed coordinates back."
       ^CoordinateSequenceFilter [transformed-coords idx-atom]
       (reify CoordinateSequenceFilter
         (filter [_ cseq i]
           (apply-coord-item! cseq i (nth transformed-coords @idx-atom))
           (swap! idx-atom inc))
         (isDone [_] false)
         (isGeometryChanged [_] true)))

     (defn apply-transformed-coords!
       "Apply transformed coordinates to a geometry in-place (2D/3D/4D)."
       [^Geometry geom transformed-coords]
       (let [idx (atom 0)
             filt (apply-coords-filter transformed-coords idx)]
         (.apply geom filt)
         geom))))

;; crs->projjson works on both platforms. The fndefs of clj-proj
;; declare `proj_as_projjson` for FFI and for wasm. One difference remains: a
;; worker holds the surface of proj-wasm. The cljs arity is therefore ^:async,
;; and the caller must await it. Keep the two bodies the same.
#?(:clj
   (defn crs->projjson
     "Resolve a CRS string (PROJJSON, 'EPSG:2249', WKT, PROJ string or urn) to
      PROJJSON. Returns nil for a nil or blank crs."
     [crs]
     (when (string? crs)
       (let [s (str/trim crs)]
         (when (pos? (count s))
           (if (str/starts-with? s "{")
             s
             (do @proj-init-future
                 (let [ctx (proj/context-create)
                       pj  (proj/proj-create {:context ctx :definition s})]
                   (proj/proj-as-projjson {:context ctx :pj pj}))))))))

   :cljs
   (defn ^:async crs->projjson
     "Return a Promise of the CRS string as PROJJSON. Throws if the CRS is not
      PROJJSON and init-proj! did not run."
     [crs]
     (when (string? crs)
       (let [s (str/trim crs)]
         (when (pos? (count s))
           (if (str/starts-with? s "{")
             s
             (do
               (when-not @proj-initialized?
                 (throw (js/Error. "PROJ not initialized. Call init-proj! first.")))
               (let [ctx (await (proj/context_create))
                     pj  (await (proj/proj_create #js {:context ctx :definition s}))]
                 (await (proj/proj_as_projjson #js {:context ctx :pj pj}))))))))))

#?(:cljs
   (do
     (defn ^:async get-or-create-transformer
       "Return a Promise of {:ctx <context> :tx <transformer>} for the CRS pair.
        Throws if init-proj! did not run."
       [source-crs target-crs]
       (when-not @proj-initialized?
         (throw (js/Error. "PROJ not initialized. Call init-proj! first.")))
       (let [cache-key [source-crs target-crs]]
         (or (get @transformer-cache cache-key)
             (let [ctx (await (proj/context_create))
                   raw-tx (await (proj/proj_create_crs_to_crs #js {:context ctx
                                                                   :source_crs source-crs
                                                                   :target_crs target-crs}))
                   tx (await (proj/proj_normalize_for_visualization #js {:context ctx :obj raw-tx}))]
               (when (nil? tx)
                 (throw (js/Error. (str "Failed to create CRS transformer: "
                                        source-crs " -> " target-crs))))
               (swap! transformer-cache assoc cache-key {:ctx ctx :tx tx})
               {:ctx ctx :tx tx}))))

     (defn extract-geometry-coords
       "Extract all coordinates from a wasmts geometry as [[x y z m] ...]."
       [geom]
       (let [wasmts-coords (js/globalThis.wasmts.geom.getCoordinates geom)
             n (.-length wasmts-coords)]
         {:coords (mapv (fn [c]
                          [(.-x c) (.-y c)
                           (or (.-z c) 0.0)
                           (or (.-m c) 0.0)])
                        wasmts-coords)
          :count n}))

     (defn read-coord-array
       "Read n coordinates from a PROJ coord array as [[x y z t] ...]."
       [coord-array n]
       (mapv (fn [i] (proj/get_coords coord-array i)) (range n)))))

(defn ^:async transform-geom
  "Transform a geometry or feature from source-crs to target-crs, with lon/lat
   axis order. Returns the new geometry or feature, as a Promise on JS."
  [feat source-crs target-crs]
  #?(:clj
     (let [geom (feature/geometry feat)
           ^Geometry g (.copy ^Geometry geom)
           {:keys [tx]} (get-or-create-transformer source-crs target-crs)
           {:keys [coords count]} (extract-geometry-coords g)
           coord-array (proj/coord-array count)
           _ (proj/set-coords! coord-array coords)
           result (proj/proj-trans-array {:p tx :direction 1 :n count :coord coord-array})
           _ (when (and result (not= 0 result))
               (throw (ex-info "PROJ transformation failed"
                               {:result result :source-crs source-crs :target-crs target-crs})))
           transformed-coords (read-coord-array coord-array count)
           _ (apply-transformed-coords! g transformed-coords)
           _ (feature/set-crs g target-crs)]
       (if (map? feat) (feature/assoc-geometry feat g) g))

     :cljs
     ;; Only the transformer creation and the transform go to the proj-wasm
     ;; worker. proj-trans-array copies the result back into the same WASM
     ;; buffer, and read-coord-array sees it after the await.
     (let [geom (feature/geometry feat)
           {:keys [tx]} (await (get-or-create-transformer source-crs target-crs))
           {:keys [coords count]} (extract-geometry-coords geom)
           coord-array (proj/coord_array count)
           _ (await (proj/set_coords_BANG_ coord-array coords))
           result (await (proj/proj_trans_array #js {:p tx :direction 1 :n count :coord coord-array}))
           _ (when (and result (not= 0 result))
               (throw (js/Error. (str "PROJ transformation failed: " result))))
           transformed-coords (read-coord-array coord-array count)
           idx (atom 0)
           transformed-geom (.apply geom
                                    (fn [seq i]
                                      (let [coord-idx @idx
                                            [x y z _m] (nth transformed-coords coord-idx)]
                                        (.setOrdinate seq i 0 x)
                                        (.setOrdinate seq i 1 y)
                                        (when (.hasZ seq)
                                          (.setOrdinate seq i 2 z))
                                        (swap! idx inc))))
           _ (feature/set-crs transformed-geom target-crs)]
       (if (map? feat) (feature/assoc-geometry feat transformed-geom) transformed-geom))))

#?(:clj (define-all-ts-fns :jvm)
   :cljs (define-all-ts-fns))

;; The minted `buffer` (ts-macros :opts-arg) calls this helper for the final
;; BufferParameters argument of BufferOp.bufferOp.

(defn buffer-params
  "Build a BufferParameters from optional :quad-segs (default 8), :cap-style
   (:round, :flat or :square), :join-style (:round, :mitre or :bevel) and
   :mitre-limit (default 5.0)."
  [opts]
  (buffer-parameters-create
   {:quadrant-segments (get opts :quad-segs 8)
    :end-cap-style     (get {:round 1 :flat 2 :square 3} (get opts :cap-style :round) 1)
    :join-style        (get {:round 1 :mitre 2 :bevel 3} (get opts :join-style :round) 1)
    :mitre-limit       (get opts :mitre-limit 5.0)}))

(defn check-report
  "Return a snapshot of what the macro mints, for `bb ts:check`."
  []
  (let [{:keys [minted by-package total-entries]} (check-data)]
    {:minted minted
     :registered minted
     :total-entries total-entries
     :by-package by-package}))
