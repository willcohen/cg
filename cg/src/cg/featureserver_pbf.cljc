;; The GDAL driver reads JSON only, and a PBF fetch cannot go through it.
(ns cg.featureserver-pbf
  "Decoder for the FeatureServer `f=pbf` query response (FeatureCollectionPBuffer).
   Gives the {:rows :source-crs} that cg.io/walk-dataset-jvm! returns."
  #?(:clj (:require [cg.feature :as feature]
                    [cg.geo :as geo]))
  #?(:clj (:import (org.locationtech.jts.geom Coordinate GeometryFactory
                                              LinearRing Polygon)
                   (org.locationtech.jts.algorithm Orientation))))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (do
     (def ^:private ^GeometryFactory gf (GeometryFactory.))

     (defn- read-varint!
       "Read the varint at `i`. Writes the value to cur[0] and returns the next
        byte index. The caller owns `cur`, and the hot loop does not allocate."
       ^long [^bytes b ^long i ^longs cur]
       (loop [i i shift 0 acc 0]
         (let [x (bit-and (aget b i) 0xff)
               acc (bit-or (long acc)
                           (bit-shift-left (long (bit-and x 0x7f)) (long shift)))]
           (if (zero? (bit-and x 0x80))
             (do (aset cur 0 (long acc)) (inc i))
             (recur (inc i) (+ (long shift) 7) acc)))))

     (defn- zigzag ^long [^long n]
       (bit-xor (unsigned-bit-shift-right n 1) (- (bit-and n 1))))

     (defn- field-number ^long [^long key] (bit-shift-right key 3))
     (defn- wire-type ^long [^long key] (bit-and key 7))

     (defn- read-double ^double [^bytes b ^long i]
       (Double/longBitsToDouble
        (loop [k 7 acc 0]
          (if (neg? k)
            acc
            (recur (dec k) (bit-or (bit-shift-left (long acc) 8)
                                   (bit-and (aget b (+ i k)) 0xff)))))))

     (defn- skip-payload
       "Advance past one field's payload, given its wire type."
       ^long [^bytes b ^long i ^long wt ^longs cur]
       (case (int wt)
         0 (read-varint! b i cur)
         1 (+ i 8)
         2 (let [i (read-varint! b i cur)] (+ i (aget cur 0)))
         5 (+ i 4)))

     (defn- read-spatial-reference
       [^bytes b start end ^longs cur]
       (let [end (long end)]
         (loop [i (long start) acc {}]
           (if (>= i end)
             acc
             (let [i (read-varint! b i cur)
                   key (aget cur 0)
                   fnum (field-number key)
                   wt (wire-type key)]
               (case (int fnum)
                 1 (let [i (read-varint! b i cur)]
                     (recur i (assoc acc :wkid (aget cur 0))))
                 2 (let [i (read-varint! b i cur)]
                     (recur i (assoc acc :latest-wkid (aget cur 0))))
                 5 (let [i (read-varint! b i cur)
                         len (aget cur 0)]
                     (recur (+ i len)
                            (assoc acc :wkt (String. b (int i) (int len) "UTF-8"))))
                 (recur (skip-payload b i wt cur) acc)))))))

     (defn- read-transform
       "Scale and translate for the quantization grid, as [sx sy tx ty]."
       [^bytes b start end ^longs cur]
       (let [end (long end)
             out (double-array 4)]
         (loop [i (long start)]
           (when (< i end)
             (let [i (read-varint! b i cur)
                   key (aget cur 0)
                   fnum (field-number key)
                   wt (wire-type key)]
               (if (or (= fnum 2) (= fnum 3))
                 (let [i (read-varint! b i cur)
                       blk-end (+ i (aget cur 0))
                       base (if (= fnum 2) 0 2)]
                   (loop [j (long i)]
                     (when (< j blk-end)
                       (let [j (read-varint! b j cur)
                             k2 (aget cur 0)
                             f2 (field-number k2)
                             w2 (wire-type k2)]
                         (if (and (= w2 1) (<= 1 f2 2))
                           (do (aset out (+ base (dec f2)) (read-double b j))
                               (recur (+ j 8)))
                           (recur (skip-payload b j w2 cur))))))
                   (recur (long blk-end)))
                 (recur (skip-payload b i wt cur))))))
         out))

     ;; Because proto3 omits a field that equals its default, the small-integer
     ;; field type (enum value 0) is never on the wire. Start from it.
     (defn- read-field
       [^bytes b start end ^longs cur]
       (let [end (long end)]
         (loop [i (long start) acc {:field-type 0}]
           (if (>= i end)
             acc
             (let [i (read-varint! b i cur)
                   key (aget cur 0)
                   fnum (field-number key)
                   wt (wire-type key)]
               (case (int fnum)
                 1 (let [i (read-varint! b i cur)
                         len (aget cur 0)]
                     (recur (+ i len)
                            (assoc acc :name (String. b (int i) (int len) "UTF-8"))))
                 2 (let [i (read-varint! b i cur)]
                     (recur i (assoc acc :field-type (aget cur 0))))
                 (recur (skip-payload b i wt cur) acc)))))))

     ;; Value is a oneof. The index into the field list is positional: the nth
     ;; value of a feature belongs to the nth field of the result header.
     (defn- read-value
       [^bytes b start end ^longs cur]
       (let [end (long end)]
         (loop [i (long start) v nil]
           (if (>= i end)
             v
             (let [i (read-varint! b i cur)
                   key (aget cur 0)
                   fnum (field-number key)
                   wt (wire-type key)]
               (case (int fnum)
                 1 (let [i (read-varint! b i cur)
                         len (aget cur 0)]
                     (recur (+ i len) (String. b (int i) (int len) "UTF-8")))
                 2 (recur (+ i 4) (Float/intBitsToFloat
                                   (int (bit-or (bit-and (aget b i) 0xff)
                                                (bit-shift-left (bit-and (aget b (+ i 1)) 0xff) 8)
                                                (bit-shift-left (bit-and (aget b (+ i 2)) 0xff) 16)
                                                (bit-shift-left (bit-and (aget b (+ i 3)) 0xff) 24)))))
                 3 (recur (+ i 8) (read-double b i))
                 4 (let [i (read-varint! b i cur)] (recur i (int (zigzag (aget cur 0)))))
                 5 (let [i (read-varint! b i cur)] (recur i (aget cur 0)))
                 6 (let [i (read-varint! b i cur)] (recur i (aget cur 0)))
                 7 (let [i (read-varint! b i cur)] (recur i (aget cur 0)))
                 8 (let [i (read-varint! b i cur)] (recur i (zigzag (aget cur 0))))
                 9 (let [i (read-varint! b i cur)] (recur i (not (zero? (aget cur 0)))))
                 10 (let [i (read-varint! b i cur)] (recur i nil))
                 (recur (skip-payload b i wt cur) v)))))))

     (defn- read-lengths!
       [^bytes b start end ^longs cur ^java.util.ArrayList out]
       (let [end (long end)]
         (loop [i (long start)]
           (when (< i end)
             (let [i (read-varint! b i cur)]
               (.add out (aget cur 0))
               (recur i))))
         out))

     ;; The delta chain resets at every ring. A decoder that carries it across rings places
     ;; holes about 1.2e8 units away, and only a polygon with a hole shows it.
     (defn- read-ring!
       "Read one ring of `n` vertex pairs from `start` into `out`, de-quantized. Returns the next byte index."
       [^bytes b start n ^"[Lorg.locationtech.jts.geom.Coordinate;" out ^longs cur
        ^doubles xf]
       (let [sx (aget xf 0) sy (aget xf 1) tx (aget xf 2) ty (aget xf 3)]
         (loop [i (long start) v 0 x 0 y 0]
           (if (= (long v) (long n))
             i
             (let [i (read-varint! b i cur)
                   dx (zigzag (aget cur 0))
                   i (read-varint! b i cur)
                   dy (zigzag (aget cur 0))
                   x (+ (long x) dx)
                   y (+ (long y) dy)]
               (aset out (int v) (Coordinate. (+ (* (double x) sx) tx)
                                              (+ (* (double y) (- sy)) ty)))
               (recur (long i) (inc (long v)) x y))))))

     (defn- close-ring
       "JTS needs a closed coordinate array. The server omits the repeated last
        vertex on some rings."
       ^"[Lorg.locationtech.jts.geom.Coordinate;" [^"[Lorg.locationtech.jts.geom.Coordinate;" cs]
       (let [n (alength cs)]
         (if (and (pos? n) (.equals2D ^Coordinate (aget cs 0) ^Coordinate (aget cs (dec n))))
           cs
           (let [^objects out (make-array Coordinate (inc n))]
             (System/arraycopy cs 0 out 0 n)
             (aset out n (aget cs 0))
             out))))

     (defn- rings->polygonal
       "Assemble the wire-format rings into a JTS Polygon or MultiPolygon. A clockwise ring opens
        a polygon, a counter-clockwise ring is a hole of the preceding one (as GDAL organizePolygons)."
       [rings]
       (let [parts (reduce (fn [acc ^"[Lorg.locationtech.jts.geom.Coordinate;" cs]
                             (let [closed (close-ring cs)
                                   ring (.createLinearRing gf closed)]
                               (if (Orientation/isCCW closed)
                                 (if (seq acc)
                                   (update-in acc [(dec (count acc)) :holes] conj ring)
                                   (conj acc {:shell ring :holes []}))
                                 (conj acc {:shell ring :holes []}))))
                           []
                           rings)
             polys (mapv (fn [{:keys [shell holes]}]
                           (.createPolygon gf ^LinearRing shell
                                           (into-array LinearRing holes)))
                         parts)]
         (if (= 1 (count polys))
           (first polys)
           (.createMultiPolygon gf (into-array Polygon polys)))))

     (defn- parts->geometry
       "Build the JTS geometry for the geometryType of the layer from `lengths` and `coords`.
        A Point arrives as one part of one vertex."
       [geom-type parts]
       (case (int geom-type)
         0 (.createPoint gf ^Coordinate (aget ^objects (first parts) 0))
         1 (.createMultiPointFromCoords
            gf (into-array Coordinate (mapcat seq parts)))
         2 (let [lines (mapv (fn [^"[Lorg.locationtech.jts.geom.Coordinate;" cs]
                               (.createLineString gf cs))
                             parts)]
             (if (= 1 (count lines))
               (first lines)
               (.createMultiLineString gf (into-array org.locationtech.jts.geom.LineString lines))))
         3 (rings->polygonal parts)
         (throw (ex-info "cg.featureserver-pbf: unsupported geometry type"
                         {:geometry-type geom-type}))))

     (defn- read-geometry
       [^bytes b start end ^longs cur ^doubles xf geom-type]
       (let [end (long end)
             lengths (java.util.ArrayList.)
             rings (java.util.ArrayList.)]
         (loop [i (long start)]
           (when (< i end)
             (let [i (read-varint! b i cur)
                   key (aget cur 0)
                   fnum (field-number key)
                   wt (wire-type key)]
               (cond
                 (= fnum 2) (let [i (read-varint! b i cur)
                                  blk-end (+ i (aget cur 0))]
                              (read-lengths! b i blk-end cur lengths)
                              (recur (long blk-end)))
                 (= fnum 3) (let [i (read-varint! b i cur)
                                  blk-end (+ i (aget cur 0))]
                              (loop [p (long i) idx 0]
                                (when (< idx (.size lengths))
                                  (let [n (long (.get lengths idx))
                                        cs (make-array Coordinate n)
                                        p' (read-ring! b p n cs cur xf)]
                                    (.add rings cs)
                                    (recur (long p') (inc idx)))))
                              (recur (long blk-end)))
                 :else (recur (skip-payload b i wt cur))))))
         (when (pos? (.size rings))
           (parts->geometry geom-type (vec rings)))))

     ;; OGR_F_GetFieldAs* in the GDAL reader of cg.io gives 0 or "" for an
     ;; unset field, and downstream cg code expects that value, not the null.
     (def ^:private type-defaults
       {0 (int 0) 1 (int 0) 6 (int 0) 13 0
        2 0.0 3 0.0
        4 ""})

     ;; PBF carries a date as epoch milliseconds. OGR_F_GetFieldAsString gives
     ;; the same instant as "1964/05/25 00:00:00+00", and downstream cg code
     ;; reads that string.
     (def ^:private ^java.time.format.DateTimeFormatter ogr-date-format
       (.withZone (java.time.format.DateTimeFormatter/ofPattern "yyyy/MM/dd HH:mm:ss'+00'")
                  java.time.ZoneOffset/UTC))

     (defn- coerce-value
       [v field-type preserve-nulls? raw-dates?]
       (cond
         (nil? v) (when-not preserve-nulls? (get type-defaults field-type))
         (and (= 5 (long field-type)) (not raw-dates?) (number? v))
         (.format ogr-date-format (java.time.Instant/ofEpochMilli (long v)))
         :else v))

     (defn- read-feature
       "One feature into a row map. `order` is the field name list; a value's
        position in the feature selects its field."
       [^bytes b start end ^longs cur ^doubles xf geom-type order types oid-field
        crs-str opts idx]
       (let [end (long end)
             {:keys [preserve-nulls? raw-dates?]} opts
             vals (java.util.ArrayList.)]
         (loop [i (long start) geom nil]
           (if (>= i end)
             (let [fields (persistent!
                           (reduce (fn [acc n]
                                     (assoc! acc (nth order n)
                                             (coerce-value (.get vals n) (nth types n)
                                                           preserve-nulls? raw-dates?)))
                                   (transient {})
                                   (range (min (.size vals) (count order)))))]
               (when (and geom crs-str) (feature/set-crs geom crs-str))
               ;; The driver numbers the features in order when the query does
               ;; not return the object-id column. Do the same here. The
               ;; position restarts at 0 on each page, and a paging caller
               ;; must pass the running row count as :fid-offset.
               (assoc fields
                      :fid (or (get fields oid-field) (int idx))
                      :geometry geom))
             (let [i (read-varint! b i cur)
                   key (aget cur 0)
                   fnum (field-number key)
                   wt (wire-type key)]
               (case (int fnum)
                 1 (let [i (read-varint! b i cur)
                         len (aget cur 0)]
                     (.add vals (read-value b i (+ i len) cur))
                     (recur (+ i len) geom))
                 2 (let [i (read-varint! b i cur)
                         len (aget cur 0)]
                     (recur (+ i len) (read-geometry b i (+ i len) cur xf geom-type)))
                 ;; shapeBuffer is 3 and curveGeometry is 7. Both are separate
                 ;; branches of the geometry oneof that this decoder cannot read.
                 (3 7) (throw (ex-info "cg.featureserver-pbf: feature carries a curve or a shape buffer"
                                       {:field fnum}))
                 (recur (skip-payload b i wt cur) geom)))))))

     (defn- scan-feature-result
       [^bytes b start end ^longs cur]
       (let [end (long end)]
         (loop [i (long start) acc {:fields [] :features []}]
           (if (>= i end)
             (update acc :geometry-type #(or % 0))
             (let [i (read-varint! b i cur)
                   key (aget cur 0)
                   fnum (field-number key)
                   wt (wire-type key)]
               (case (int fnum)
                 1 (let [i (read-varint! b i cur)
                         len (aget cur 0)]
                     (recur (+ i len)
                            (assoc acc :oid-field (String. b (int i) (int len) "UTF-8"))))
                 ;; proto3 omits geometryType when it is the point type (enum
                 ;; value 0): absent means Point.
                 7 (let [i (read-varint! b i cur)]
                     (recur i (assoc acc :geometry-type (aget cur 0))))
                 8 (let [i (read-varint! b i cur)
                         len (aget cur 0)]
                     (recur (+ i len)
                            (assoc acc :spatial-reference
                                   (read-spatial-reference b i (+ i len) cur))))
                 ;; proto3 omits a false bool: absent means that the server
                 ;; returned the whole result set.
                 9 (let [i (read-varint! b i cur)]
                     (recur i (assoc acc :exceeded-transfer-limit?
                                     (not (zero? (aget cur 0))))))
                 10 (let [i (read-varint! b i cur)]
                      (recur i (assoc acc :has-z (not (zero? (aget cur 0))))))
                 11 (let [i (read-varint! b i cur)]
                      (recur i (assoc acc :has-m (not (zero? (aget cur 0))))))
                 12 (let [i (read-varint! b i cur)
                          len (aget cur 0)]
                      (recur (+ i len)
                             (assoc acc :transform (read-transform b i (+ i len) cur))))
                 13 (let [i (read-varint! b i cur)
                          len (aget cur 0)]
                      (recur (+ i len)
                             (update acc :fields conj (read-field b i (+ i len) cur))))
                 15 (let [i (read-varint! b i cur)
                          len (aget cur 0)]
                      (recur (+ i len) (update acc :features conj [i (+ i len)])))
                 (recur (skip-payload b i wt cur) acc)))))))

     (defn- source-crs
       "Return the CRS descriptor of cg from the PBF spatial reference. Each
        geometry gets the compact form."
       [{:keys [wkid latest-wkid wkt]}]
       (let [code (or latest-wkid wkid)]
         (cond
           code {:authority "EPSG"
                 :code (str code)
                 :projjson (geo/crs->projjson (str "EPSG:" code))}
           wkt  {:authority nil :code nil :projjson (geo/crs->projjson wkt)}
           :else nil)))

     (defn decode-page
       "Decode one `f=pbf` response to {:rows :source-crs :exceeded-transfer-limit?}. Geometry is quantized.
        Options: :preserve-nulls? keeps nil (default 0 or \"\"), :raw-dates? keeps epoch ms, :fid-offset adds to :fid.
        Throws on a curve or a shape buffer; use f=json for that layer."
       ([^bytes body] (decode-page body {}))
       ([^bytes body opts]
        (let [cur (long-array 1)
              top-end (alength body)
              ;; FeatureCollectionPBuffer.queryResult is field 2, and
              ;; QueryResult.featureResult is field 1.
              qr (loop [i 0]
                   (if (>= i top-end)
                     nil
                     (let [i (read-varint! body i cur)
                           key (aget cur 0)]
                       (if (= 2 (field-number key))
                         (let [i (read-varint! body i cur)] [i (+ i (aget cur 0))])
                         (recur (skip-payload body i (wire-type key) cur))))))
              [qs qe] qr
              fr (loop [i (long qs)]
                   (if (>= i (long qe))
                     nil
                     (let [i (read-varint! body i cur)
                           key (aget cur 0)]
                       (if (= 1 (field-number key))
                         (let [i (read-varint! body i cur)] [i (+ i (aget cur 0))])
                         (recur (skip-payload body i (wire-type key) cur))))))
              [fs fe] fr
              {:keys [fields features transform spatial-reference oid-field has-z has-m
                      geometry-type exceeded-transfer-limit?]}
              (scan-feature-result body fs fe cur)]
          (when (or has-z has-m)
            (throw (ex-info "cg.featureserver-pbf: hasZ and hasM change the coordinate stride"
                            {:has-z has-z :has-m has-m})))
          (let [order (mapv :name fields)
                types (mapv :field-type fields)
                crs (source-crs spatial-reference)
                crs-str (if (:code crs) (str (:authority crs) ":" (:code crs)) (:projjson crs))
                xf (or transform (double-array [1.0 1.0 0.0 0.0]))
                fid-offset (long (or (:fid-offset opts) 0))
                rows (into []
                           (map-indexed (fn [idx [s e]]
                                          (read-feature body s e cur xf geometry-type
                                                        order types oid-field crs-str
                                                        opts (+ fid-offset (long idx)))))
                           features)]
            {:rows rows
             :source-crs crs
             :exceeded-transfer-limit? (boolean exceeded-transfer-limit?)}))))

     ;; A server that ignores resultOffset answers each request with page 1,
     ;; and the transfer-limit flag stays set. The ceiling prevents an
     ;; unbounded loop against the server.
     (def ^:private default-max-pages 10000)

     ;; Stop on a cleared exceededTransferLimit or on an empty page: a server can truncate
     ;; a result and not set the flag.
     (defn load-pages
       "Page through a layer and return {:rows :source-crs :pages}. `fetch-page` takes a row offset and
        returns the f=pbf bytes of that page. Options: those of decode-page plus :max-pages.
        Keep the page size at or under the server cap: 4000 requested gives 2000 rows with the flag set."
       ([fetch-page] (load-pages fetch-page {}))
       ([fetch-page opts]
        (let [max-pages (long (or (:max-pages opts) default-max-pages))
              decode-opts (dissoc opts :max-pages)]
          (loop [offset 0 page 1 rows [] crs nil]
            (when (> (long page) max-pages)
              (throw (ex-info "cg.featureserver-pbf: page ceiling reached, the server may be ignoring resultOffset"
                              {:max-pages max-pages :rows (count rows)})))
            (let [{page-rows :rows page-crs :source-crs more? :exceeded-transfer-limit?}
                  (decode-page (fetch-page offset)
                               (assoc decode-opts :fid-offset offset))
                  rows (into rows page-rows)
                  crs (or crs page-crs)]
              (if (and more? (pos? (count page-rows)))
                (recur (+ (long offset) (count page-rows)) (inc (long page)) rows crs)
                {:rows rows :source-crs crs :pages page}))))))))
