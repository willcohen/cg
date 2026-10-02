;; Copyright (c) 2019, 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.io
  (:require #?(:clj [cg.feature :as feature])
            #?(:clj [cg.featureserver-pbf :as pbf])
            #?(:clj [clojure.java.io :as jio])
            #?(:clj [clojure.core.reducers :as r])
            #?(:clj [clojure.tools.logging :as log])
            [clojure.string :as str]
            #?(:clj  [net.willcohen.gdal.gdal :as gdal])
            #?(:clj  [net.willcohen.gdal.fndefs :as gdal-fndefs])
            #?(:clj  [net.willcohen.gdal.network :as gdal-network])
            #?(:cljs ["gdal-wasm" :as gdal])
            #?(:cljs ["gdal-wasm/fndefs" :as gdal-fndefs])
            #?(:cljs [cg.trace-js :as trace]))
  #?(:clj (:import (org.locationtech.jts.geom Geometry GeometryFactory LinearRing Polygon Polygonal)
                   (org.locationtech.jts.io WKTReader WKTWriter WKBReader WKBWriter)
                   (org.locationtech.jts.io.geojson GeoJsonReader GeoJsonWriter))))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (do
     (def ^GeoJsonReader gjr (GeoJsonReader.))

     (def ^GeoJsonWriter gjw-crs (GeoJsonWriter.))
     (def ^GeoJsonWriter gjw-nocrs (doto (GeoJsonWriter.)
                                     (.setEncodeCRS false)))

     (defn read-geojson
       "Read GeoJSON string to JTS Geometry"
       [^String s]
       (.read gjr s))

     (defn write-geojson
       "Write JTS Geometry to GeoJSON string"
       ([^Geometry g]
        (write-geojson g false))
       ([^Geometry g ^Boolean write-crs?]
        (case write-crs?
          true (.write gjw-crs g)
          false (.write gjw-nocrs g))))))

#?(:clj
   (do
     (def ^WKTReader wkt-reader (WKTReader.))
     (def ^WKTWriter wkt-writer (WKTWriter.))

     (defn read-wkt
       "Read Well-Known Text (WKT) string to JTS Geometry"
       [^String wkt-str]
       (.read wkt-reader wkt-str))

     (defn write-wkt
       "Write JTS Geometry to Well-Known Text (WKT) string"
       [^Geometry geom]
       (.write wkt-writer geom))))

#?(:cljs
   (do
     (defn read-wkt
       "Read Well-Known Text (WKT) string to WasmTS Geometry."
       [wkt-str]
       (let [R js/globalThis.wasmts.io.WKTReader]
         (.read R (.create0 R) wkt-str)))

     (defn write-wkt
       "Write WasmTS Geometry to Well-Known Text (WKT) string."
       [geom]
       (let [W js/globalThis.wasmts.io.WKTWriter]
         (.write W (.create0 W) geom)))

     ;; GeoJSON I/O converts through WKT, because WasmTS has no GeoJSON support.

     (defn- coords->wkt-coords
       "Convert GeoJSON coordinate array to WKT coordinate string"
       [coords]
       (if (number? (first coords))
         (str/join " " coords)
         (str/join ", " (map coords->wkt-coords coords))))

     (defn- ring->wkt
       "Convert a GeoJSON ring (array of coordinates) to WKT ring string"
       [ring]
       (clojure.core/str "(" (str/join ", " (map coords->wkt-coords ring)) ")"))

     (defn- geojson-geometry->wkt
       "Convert a parsed GeoJSON geometry object to WKT string"
       [geom]
       (let [geom-type (:type geom)
             coords (:coordinates geom)]
         (case geom-type
           "Point"
           (clojure.core/str "POINT (" (coords->wkt-coords coords) ")")

           "MultiPoint"
           (clojure.core/str "MULTIPOINT (" (str/join ", " (map #(clojure.core/str "(" (coords->wkt-coords %) ")") coords)) ")")

           "LineString"
           (clojure.core/str "LINESTRING (" (str/join ", " (map coords->wkt-coords coords)) ")")

           "MultiLineString"
           (clojure.core/str "MULTILINESTRING (" (str/join ", " (map ring->wkt coords)) ")")

           "Polygon"
           (clojure.core/str "POLYGON (" (str/join ", " (map ring->wkt coords)) ")")

           "MultiPolygon"
           (clojure.core/str "MULTIPOLYGON (" (str/join ", " (map #(clojure.core/str "(" (str/join ", " (map ring->wkt %)) ")") coords)) ")")

           "GeometryCollection"
           (clojure.core/str "GEOMETRYCOLLECTION (" (str/join ", " (map geojson-geometry->wkt (:geometries geom))) ")")

           (throw (js/Error. (clojure.core/str "Unknown GeoJSON geometry type: " geom-type))))))

     (defn read-geojson
       "Read a GeoJSON string to a WasmTS Geometry. The conversion goes
        through WKT."
       [geojson-str]
       ;; Under squint, the plain JS object from JSON.parse is a usable map.
       ;; No js->clj is necessary.
       (let [geom (.parse js/JSON geojson-str)
             wkt (geojson-geometry->wkt geom)]
         (read-wkt wkt)))

     (defn- parse-wkt-coords
       "Parse WKT coordinates to GeoJSON: \"x y\" gives [x y], and \"x y, x y\"
        gives [[x y] [x y]]."
       [coord-str]
       (let [trimmed (str/trim coord-str)]
         (if (str/includes? trimmed ",")
           (mapv parse-wkt-coords (str/split trimmed #","))
           (mapv js/parseFloat (str/split trimmed #"\s+")))))

     (defn- parse-wkt-ring
       "Parse WKT ring string (including parens) to coordinate array"
       [ring-str]
       (let [inner (-> ring-str str/trim (subs 1) (subs 0 (dec (count (str/trim ring-str)))))]
         (parse-wkt-coords inner)))

     (defn- wkt->geojson-geometry
       "Convert WKT string to GeoJSON geometry object (as Clojure map)"
       [wkt-str]
       (let [wkt (str/trim wkt-str)
             space-idx (str/index-of wkt " ")
             paren-idx (str/index-of wkt "(")
             type-end (if (and space-idx paren-idx) (min space-idx paren-idx) (or space-idx paren-idx))
             geom-type (subs wkt 0 type-end)
             content (str/trim (subs wkt paren-idx))]
         (case (str/upper-case geom-type)
           "POINT"
           (let [inner (subs content 1 (dec (count content)))]
             {:type "Point"
              :coordinates (parse-wkt-coords inner)})

           "MULTIPOINT"
           ;; MultiPoint is ((x y), (x y)) or (x y, x y)
           (let [inner (subs content 1 (dec (count content)))]
             {:type "MultiPoint"
              :coordinates (mapv #(parse-wkt-coords (str/replace % #"[()]" ""))
                                 (str/split inner #","))})

           "LINESTRING"
           (let [inner (subs content 1 (dec (count content)))]
             {:type "LineString"
              :coordinates (parse-wkt-coords inner)})

           "MULTILINESTRING"
           (let [rings (re-seq #"\([^()]+\)" content)]
             {:type "MultiLineString"
              :coordinates (mapv parse-wkt-ring rings)})

           "POLYGON"
           (let [rings (re-seq #"\([^()]+\)" content)]
             {:type "Polygon"
              :coordinates (mapv parse-wkt-ring rings)})

           "MULTIPOLYGON"
           ;; Each polygon is a parenthesized list of rings.
           (let [polygons (re-seq #"\(\([^)]+\)(?:,\s*\([^)]+\))*\)" content)]
             {:type "MultiPolygon"
              :coordinates (mapv (fn [poly]
                                   (mapv parse-wkt-ring (re-seq #"\([^()]+\)" poly)))
                                 polygons)})

           (throw (js/Error. (clojure.core/str "Unknown WKT geometry type: " geom-type))))))

     (defn write-geojson
       "Write a WasmTS Geometry to a GeoJSON string. The conversion goes
        through WKT."
       ([geom]
        (write-geojson geom false))
       ([geom _write-crs?]
        (let [wkt (write-wkt geom)
              geojson-obj (wkt->geojson-geometry wkt)]
          (.stringify js/JSON geojson-obj))))))

#?(:clj
   (do
     (def ^WKBReader wkb-reader (WKBReader.))
     (def ^WKBWriter wkb-writer (WKBWriter.))

     (defn read-wkb
       "Read Well-Known Binary (WKB) byte array to JTS Geometry"
       [^bytes wkb-bytes]
       (.read wkb-reader wkb-bytes))

     (defn write-wkb
       "Write JTS Geometry to Well-Known Binary (WKB) byte array"
       [^Geometry geom]
       (.write wkb-writer geom))))

#?(:cljs
   (do
     (defn read-wkb
       "Read Well-Known Binary (WKB) from a Uint8Array to a WasmTS Geometry."
       [wkb-bytes]
       (let [R js/globalThis.wasmts.io.WKBReader]
         (.read R (.create0 R) wkb-bytes)))

     (defn write-wkb
       "Write a WasmTS Geometry to Well-Known Binary (WKB) as a Uint8Array."
       [geom]
       ;; The default writer is 2D and drops each z. A writer of dimension 3
       ;; writes a 2D geometry as 2D.
       (let [W js/globalThis.wasmts.io.WKBWriter]
         (.write W (.create1 W 3) geom)))))

;; load-source opens a vector source, walks the first layer, and returns
;;   {:rows       [<row>+]
;;    :source-crs {:projjson <PROJJSON> :authority "EPSG" :code "4326"} or nil}
;; A row is a flat map of the OGR attributes (field name string to value)
;; plus :fid and :geometry.
;;
;; This namespace does not make the FlowDataset. cg.dataset requires cg.io,
;; and cg.dataset/vector-source->dataset wraps the result.
;; CLJS goes through the joint worker pool (cg.run/init-cg-workers!). The
;; JVM calls the FFI surface of clj-gdal.

#?(:clj
   (do
     ;; The singleton wkb-reader is not thread-safe (JTS docs). The parallel
     ;; decode uses one WKBReader for each worker thread.
     (def ^:private ^ThreadLocal wkb-reader-tl
       (ThreadLocal/withInitial
        (reify java.util.function.Supplier
          (get [_] (WKBReader.)))))

     (defn- read-wkb-parallel
       (^Geometry [^bytes wkb-bytes]
        (.read ^WKBReader (.get wkb-reader-tl) wkb-bytes)))

     (defn- read-fields-jvm-fast
       "Read the fields of feat. field-info is a vector of [name type i] tuples."
       [feat field-info]
       (loop [fi field-info out (transient {})]
         (if-let [info (first fi)]
           (let [[nm t i] info
                 v  (cond
                      (= 1 (gdal/ogr-f-is-field-null feat i)) nil
                      (= gdal-fndefs/OFTInteger    t) (gdal/ogr-f-get-field-as-integer    feat i)
                      (= gdal-fndefs/OFTReal       t) (gdal/ogr-f-get-field-as-double     feat i)
                      (= gdal-fndefs/OFTString     t) (gdal/ogr-f-get-field-as-string     feat i)
                      (= gdal-fndefs/OFTInteger64  t) (gdal/ogr-f-get-field-as-integer64  feat i)
                      :else                           (gdal/ogr-f-get-field-as-string     feat i))]
             (recur (rest fi) (assoc! out nm v)))
           (persistent! out))))

     (defn- layer-crs-jvm
       "Return {:projjson :authority :code} for the layer SRS, or nil with no SRS.
        :authority and :code are nil for a CRS with no registry code."
       [layer]
       (when-let [srs (gdal/ogr-l-get-spatial-ref layer)]
         (let [code   (gdal/osr-get-authority-code srs nil)
               code-s (when code
                        (let [s (if (string? code) code (str code))]
                          (when (pos? (count s)) s)))]
           {:projjson  (gdal/srs-export-to-projjson srs)
            :authority (when code-s (gdal/osr-get-authority-name srs nil))
            :code      code-s})))

     (defn- walk-dataset-jvm!
       "Walk the first layer of ds into {:rows :source-crs}. The caller closes ds."
       [ds]
       (let [layer      (gdal/gdal-dataset-get-layer ds 0)
             lyr-defn   (gdal/ogr-l-get-layer-defn layer)
             crs        (layer-crs-jvm layer)
             ;; OSRGetAuthorityCode gives a code only for an exact registry
             ;; match, and the short form then loses nothing.
             crs-str    (or (when (:code crs)
                              (str (:authority crs) ":" (:code crs)))
                            (:projjson crs))
             n-fields   (gdal/ogr-fd-get-field-count lyr-defn)
             field-info (mapv (fn [i]
                                (let [fd (gdal/ogr-fd-get-field-defn lyr-defn i)]
                                  [(gdal/ogr-fld-get-name-ref fd)
                                   (gdal/ogr-fld-get-type fd)
                                   i]))
                              (range n-fields))]
         (gdal/ogr-l-reset-reading layer)
         (let [partials
               (loop [out (transient [])]
                 (let [feat (gdal/ogr-l-get-next-feature layer)]
                   (if (nil? feat)
                     (persistent! out)
                     (let [fid    (gdal/ogr-f-get-fid feat)
                           fields (read-fields-jvm-fast feat field-info)
                           wkb    (gdal/feature->wkb feat)]
                       (gdal/ogr-f-destroy feat)
                       (recur (conj! out [fid fields wkb]))))))
               ;; r/fold splits at 512 items. A small dataset stays serial.
               rows
               (r/fold (fn ([] []) ([a b] (into a b)))
                       (fn [acc [fid fields wkb]]
                         (let [geom (read-wkb-parallel wkb)]
                           (when crs-str (feature/set-crs geom crs-str))
                           (conj acc (assoc fields :fid fid :geometry geom))))
                       partials)]
           {:rows rows :source-crs crs})))))

#?(:clj
   (defn load-source
     "Read the first layer of the vector file at path into
      {:rows [...] :source-crs {...}}."
     [path]
     (gdal/init!)
     (gdal/gdal-all-register)
     (let [ds (gdal/gdal-open-ex path gdal-fndefs/GDAL_OF_VECTOR nil nil nil)]
       (when (nil? ds)
         (throw (ex-info "cg.io/load-source: GDALOpen returned NULL"
                         {:path path})))
       (try
         (walk-dataset-jvm! ds)
         (finally
           (gdal/gdal-close ds))))))

#?(:clj
   (defn load-source-from-bytes
     "Load a vector source from files-map, file name to byte[], opening open-name
      (the .shp for a shapefile). Returns what load-source returns."
     [files-map open-name]
     (when-not (contains? files-map open-name)
       (throw (ex-info "cg.io/load-source-from-bytes: open-name not in files-map"
                       {:open-name open-name :keys (vec (keys files-map))})))
     (let [tmpdir (java.nio.file.Files/createTempDirectory
                   "cg-io-load"
                   (into-array java.nio.file.attribute.FileAttribute []))]
       (try
         (doseq [[name bytes] files-map]
           (java.nio.file.Files/write
            (.resolve tmpdir ^String name)
            ^bytes bytes
            ^"[Ljava.nio.file.OpenOption;" (into-array java.nio.file.OpenOption [])))
         (load-source (.toString (.resolve tmpdir ^String open-name)))
         (finally
           (doseq [name (keys files-map)]
             (java.nio.file.Files/deleteIfExists (.resolve tmpdir ^String name)))
           (java.nio.file.Files/deleteIfExists tmpdir))))))

;; OGR vector write. The values arrive typed and in the order of `fields`.
;; cg.dataset/write-gdal! selects the OGR type of a column from its data.

#?(:clj
   (def ogr-field-types
     "Field types for write-ogr-layer!, each with its OGR type and subtype."
     {:string  {:type gdal-fndefs/OFTString    :subtype nil}
      :integer {:type gdal-fndefs/OFTInteger64 :subtype nil}
      :real    {:type gdal-fndefs/OFTReal      :subtype nil}
      :boolean {:type gdal-fndefs/OFTInteger   :subtype gdal-fndefs/OFSTBoolean}}))

#?(:clj
   (def ogr-geometry-types
     "Geometry types for write-ogr-layer!, mapped to the OGR wkb constants.
      The keys are the JTS type names in lower case."
     {:point              gdal-fndefs/wkbPoint
      :linestring         gdal-fndefs/wkbLineString
      :polygon            gdal-fndefs/wkbPolygon
      :multipoint         gdal-fndefs/wkbMultiPoint
      :multilinestring    gdal-fndefs/wkbMultiLineString
      :multipolygon       gdal-fndefs/wkbMultiPolygon
      :geometrycollection gdal-fndefs/wkbGeometryCollection
      :unknown            gdal-fndefs/wkbUnknown}))

#?(:clj
   (defn- set-ogr-field!
     "Set field i of feat from v by the type of the field. A nil value stays
      unset, and OGR writes NULL."
     [feat i field-type v]
     (when (some? v)
       (case field-type
         :boolean (gdal/ogr-f-set-field-integer feat i (int (if v 1 0)))
         :integer (gdal/ogr-f-set-field-integer64 feat i (long v))
         :real    (gdal/ogr-f-set-field-double feat i (double v))
         (gdal/ogr-f-set-field-string feat i (str v))))))

#?(:clj
   (defn write-ogr-layer!
     "Write features, a seq of {:wkb <byte[]> :values [...]} with :values in :fields
      order, to a new one-layer file at path. opts: :driver :layer :epsg :geometry-type
      :fields [{:name :type}]. Returns {:path :layer :features <count>}."
     [path {:keys [driver layer epsg geometry-type fields]} features]
     (gdal/init!)
     (gdal/gdal-all-register)
     (let [drv (gdal/gdal-get-driver-by-name driver)]
       (when (nil? drv)
         (throw (ex-info "cg.io/write-ogr-layer!: no such GDAL driver"
                         {:driver driver})))
       ;; The GPKG driver creates the file and does not overwrite. Delete the
       ;; output of an earlier run first.
       (.delete (jio/file path))
       (let [ds (gdal/gdal-create drv path 0 0 0 gdal-fndefs/GDT_Unknown nil)]
         (when (nil? ds)
           (throw (ex-info "cg.io/write-ogr-layer!: GDALCreate returned NULL"
                           {:path path :driver driver})))
         (try
           (let [srs (when epsg
                       (let [s (gdal/osr-new-spatial-reference "")]
                         (when-not (zero? (long (gdal/osr-import-from-epsg s epsg)))
                           (throw (ex-info "cg.io/write-ogr-layer!: bad EPSG"
                                           {:epsg epsg})))
                         s))
                 lyr (gdal/gdal-dataset-create-layer
                      ds layer srs
                      (get ogr-geometry-types (or geometry-type :unknown)
                           gdal-fndefs/wkbUnknown)
                      nil)]
             (when (nil? lyr)
               (throw (ex-info "cg.io/write-ogr-layer!: CreateLayer returned NULL"
                               {:path path :layer layer})))
             (doseq [{:keys [name type]} fields]
               (let [ogr (or (get ogr-field-types type)
                             (throw (ex-info "cg.io/write-ogr-layer!: unknown field type"
                                             {:field name :type type})))
                     subtype (:subtype ogr)
                     fld (gdal/ogr-fld-create name (:type ogr))]
                 (when subtype (gdal/ogr-fld-set-sub-type fld subtype))
                 (when-not (zero? (long (gdal/ogr-l-create-field lyr fld 1)))
                   (gdal/ogr-fld-destroy fld)
                   (throw (ex-info "cg.io/write-ogr-layer!: CreateField failed"
                                   {:field name})))
                 (gdal/ogr-fld-destroy fld)))
             (let [lyr-defn (gdal/ogr-l-get-layer-defn lyr)
                   types    (mapv :type fields)]
               (gdal/gdal-dataset-start-transaction ds 1)
               (let [n (loop [fs (seq features) n 0]
                         (if-let [f (first fs)]
                           (let [feat (gdal/ogr-f-create lyr-defn)]
                             (dorun (map-indexed
                                     (fn [i v] (set-ogr-field! feat i (nth types i) v))
                                     (:values f)))
                             (when-let [^bytes wkb (:wkb f)]
                               (let [geom (gdal/geometry-from-wkb wkb nil)]
                                 (when-not (zero? (long (gdal/ogr-f-set-geometry-directly
                                                         feat geom)))
                                   (throw (ex-info "cg.io/write-ogr-layer!: SetGeometry failed"
                                                   {:feature n})))))
                             (when-not (zero? (long (gdal/ogr-l-create-feature lyr feat)))
                               (throw (ex-info "cg.io/write-ogr-layer!: CreateFeature failed"
                                               {:feature n})))
                             (gdal/ogr-f-destroy feat)
                             (recur (next fs) (inc n)))
                           n))]
                 (gdal/gdal-dataset-commit-transaction ds)
                 (when srs (gdal/osr-destroy-spatial-reference srs))
                 {:path path :layer layer :features n})))
           (finally
             (gdal/gdal-close ds)))))))

#?(:cljs
   (do
     (defn- ^:async read-fields-cljs
       "Read the fields of feat into a JS object of field name to value. Each
        accessor is a worker RPC."
       [feat lyr-defn]
       (let [n   (await (gdal/ogr-fd-get-field-count lyr-defn))
             out #js {}]
         (loop [i 0]
           (if (>= i n)
             out
             (let [fd      (await (gdal/ogr-fd-get-field-defn lyr-defn i))
                   nm      (await (gdal/ogr-fld-get-name-ref fd))
                   t       (await (gdal/ogr-fld-get-type fd))
                   is-null (await (gdal/ogr-f-is-field-null feat i))
                   v       (cond
                             (= 1 is-null) nil
                             (= 0 t)  (await (gdal/ogr-f-get-field-as-integer    feat i))
                             (= 2 t)  (await (gdal/ogr-f-get-field-as-double     feat i))
                             (= 4 t)  (await (gdal/ogr-f-get-field-as-string     feat i))
                             (= 12 t) (await (gdal/ogr-f-get-field-as-integer64  feat i))
                             :else    (await (gdal/ogr-f-get-field-as-string     feat i)))]
               (aset out nm v)
               (recur (inc i)))))))

     (defn- ^:async layer-crs-cljs
       "Return #js {:projjson :authority :code} from the SRS of the layer, or
        nil with no SRS."
       [layer]
       (let [srs (await (gdal/ogr-l-get-spatial-ref layer))]
         (if (or (nil? srs) (zero? srs))
           nil
           (let [code   (await (gdal/osr-get-authority-code srs nil))
                 code-s (when (and code (not= "" code)) (str code))]
             #js {:projjson  (await (gdal/srs-export-to-projjson srs))
                  :authority (when code-s
                               (await (gdal/osr-get-authority-name srs nil)))
                  :code      code-s}))))

     (defn source-crs->string
       "Return authority:code for a CRS descriptor (#js {:projjson :authority :code})
        that has a code, else the PROJJSON."
       [descriptor]
       (when descriptor
         (let [code (.-code descriptor)]
           (if (and code (not= "" code))
             (str (.-authority descriptor) ":" code)
             (.-projjson descriptor)))))

     (defn- ^:async walk-dataset-cljs!
       "Walk each feature of the first layer of ds. Resolves to
        #js {:rows <Array> :sourceCrs ...}. The caller closes ds."
       [ds]
       (let [layer    (await (gdal/gdal-dataset-get-layer ds 0))
             lyr-defn (await (gdal/ogr-l-get-layer-defn layer))
             crs      (await (layer-crs-cljs layer))
             out      #js []]
         (await (gdal/ogr-l-reset-reading layer))
         (loop []
           (let [feat (await (gdal/ogr-l-get-next-feature layer))]
             (if (or (nil? feat) (zero? feat))
               #js {:rows out :sourceCrs crs}
               (let [fid    (await (gdal/ogr-f-get-fid feat))
                     fields (await (read-fields-cljs feat lyr-defn))
                     wkb    (await (gdal/feature->wkb feat nil))
                     geom   (read-wkb wkb)]
                 (aset fields "fid" fid)
                 (aset fields "geometry" geom)
                 (await (gdal/ogr-f-destroy feat))
                 (.push out fields)
                 (recur)))))))

     (defn ^:async load-source
       "Load the vector file at path through the joint worker pool, after gdal/init!
        adopts it. Node only. Resolves to #js {:rows <Array> :sourceCrs <obj or nil>}."
       [path]
       (await (gdal/gdal-all-register))
       ;; open-from-disk! throws when GDAL cannot open the copy.
       (let [ds (await (gdal/open-from-disk! path gdal-fndefs/GDAL_OF_VECTOR))]
         (try
           (await (walk-dataset-cljs! ds))
           (finally
             (await (gdal/gdal-close ds))))))

     (defn ^:async load-source-from-bytes
       "Load a vector source from a JS object of file name to Uint8Array, opening
        open-name (the .shp for a shapefile). Resolves to #js {:rows :sourceCrs}."
       [files-map open-name]
       (await (gdal/gdal-all-register))
       (let [staged    (await (gdal/stage-files! files-map "/work"))
             open-path (aget staged open-name)]
         (when (or (nil? open-path) (= "" open-path))
           (throw (js/Error.
                   (str "cg.io/load-source-from-bytes: open-name '"
                        open-name "' not in files-map"))))
         (let [ds (await (gdal/gdal-open-ex open-path
                                       gdal-fndefs/GDAL_OF_VECTOR
                                       nil nil nil))]
           (when (or (nil? ds) (= ds 0))
             (throw (js/Error.
                     (str "cg.io/load-source-from-bytes: open-ex returned null for "
                          open-path))))
           (try
             (await (walk-dataset-cljs! ds))
             (finally
               (await (gdal/gdal-close ds)))))))))

#?(:clj
   (defn- url-encode [s]
     (java.net.URLEncoder/encode (str s) "UTF-8")))

#?(:cljs
   (defn- url-encode-cljs [s]
     (js/encodeURIComponent (str s))))

;; A URL source uses the ESRIJSON driver of GDAL, which pages when the server
;; reports exceededTransferLimit. GDAL is built with no curl. The fetch goes
;; through the CPLHTTPFetch host callback: net.willcohen.gdal.network on the
;; JVM, registered on the first URL load, and __gdal_http_fetch on cljs,
;; which the worker handler registers at init. orderByFields defaults to
;; "OBJECTID ASC", because paging needs a stable order (GDAL driver docs).

(defn source-label
  "The layer name in a GeoServices URL, or the last segment of a path."
  [source]
  (let [s (str source)]
    (or (second (re-find #"/services/(.+?)/(?:FeatureServer|MapServer)" s))
        (re-find #"[^/\\]+$" s)
        s)))

(defn- geoservices-layer-url?
  "True when `s` looks like a GeoServices REST layer endpoint
   (…/FeatureServer/<n> or …/MapServer/<n>)."
  [s]
  (boolean (re-find #"/(FeatureServer|MapServer)/\d+/?$" s)))

#?(:clj
   (do
     (defn- build-geoservices-query-url
       "Build the query URL for a GeoServices REST layer from opts. :wire-format is
        \"json\" or \"pbf\". :result-offset is the paging cursor."
       [layer-url {:keys [where out-fields order-by result-record-count extra-params
                          wire-format result-offset]
                   :or {where               "1=1"
                        out-fields          "*"
                        order-by            "OBJECTID ASC"
                        result-record-count 1000
                        wire-format         "json"}}]
       (let [base   (str (str/replace layer-url #"/$" "") "/query"
                         "?f=" wire-format
                         "&where=" (url-encode where)
                         "&outFields=" (url-encode out-fields)
                         "&orderByFields=" (url-encode order-by)
                         "&resultRecordCount=" result-record-count)
             offset (if result-offset (str "&resultOffset=" result-offset) "")
             extras (apply str (for [[k v] extra-params]
                                 (str "&" (name k) "=" (url-encode v))))]
         (str base offset extras)))

     ;; CPLHTTPSetFetchCallback changes native state: register one time. The
     ;; callback reads gdal-network/*transport* dynamically, and an auth wrap
     ;; binds the var again with no new registration.
     (defonce ^:private gdal-http-callback
       (delay (gdal-network/setup-http-callback!)))

     ;; The GDAL driver cannot read PBF, and its JSON parse is the main cost
     ;; of a layer load. This path owns the paging, the retries and the fetch.

     ;; Servers cap a page and set the transfer-limit flag. The loop reads what
     ;; it gets and asks again, and a lower cap needs no special case.
     (def ^:private pbf-page-size 2000)
     (def ^:private pbf-retries 3)
     (def ^:private pbf-retry-delay-ms 250)

     (defn- geoservices-error-body?
       "True when a 200 response is a JSON error body, not PBF."
       [content-type ^bytes body]
       (boolean
        (or (and content-type (str/includes? (str/lower-case content-type) "json"))
            (and (pos? (alength body)) (= 0x7b (bit-and (aget body 0) 0xff))))))

     (defn- body-snippet [^bytes body]
       (String. body 0 (min (alength body) 300) "UTF-8"))

     (defn- fetch-geoservices-once
       "Do one GET through the shared transport. Returns {:body bytes}, or
        {:retry reason} on a failure that a retry can fix. Throws otherwise."
       [url]
       (let [resp (try (gdal-network/*transport* {:url url})
                       (catch Exception e {::transport-error (or (.getMessage e) (str e))}))
             {:keys [status content-type body-bytes]} resp]
         (cond
           (::transport-error resp) {:retry (::transport-error resp)}
           (or (nil? status) (>= (long status) 500) (= 429 (long status)))
           {:retry (str "status " status)}
           (not= 200 (long status))
           (throw (ex-info "cg.io: GeoServices request failed"
                           {:url url :status status}))
           (geoservices-error-body? content-type body-bytes)
           (throw (ex-info "cg.io: GeoServices answered with an error body, not PBF"
                           {:url url :body (body-snippet body-bytes)}))
           :else {:body body-bytes})))

     (defn- fetch-geoservices-page
       "Fetch one page. The delay doubles after each failed attempt."
       ^bytes [url {:keys [retries retry-delay-ms]
                    :or {retries pbf-retries retry-delay-ms pbf-retry-delay-ms}}]
       (loop [attempt 1 delay (long retry-delay-ms)]
         (let [{:keys [body retry]} (fetch-geoservices-once url)]
           (cond
             body body
             (< (long attempt) (long retries))
             (do (Thread/sleep delay)
                 (recur (inc (long attempt)) (* 2 (long delay))))
             :else (throw (ex-info "cg.io: GeoServices request failed after retries"
                                   {:url url :attempts attempt :reason retry}))))))

     (defn- load-geoservices-pbf
       "Page a GeoServices layer over f=pbf into the load-source shape."
       [layer-url opts]
       (let [page-size (or (:result-record-count opts) pbf-page-size)
             fetch-page (fn fetch-page [offset]
                          (fetch-geoservices-page
                           (build-geoservices-query-url
                            layer-url (assoc opts
                                             :wire-format "pbf"
                                             :result-record-count page-size
                                             :result-offset offset))
                           opts))]
         (pbf/load-pages fetch-page
                         (select-keys opts [:preserve-nulls? :raw-dates? :max-pages]))))

     (defn- load-through-driver
       "Open an ESRIJSON: URL with GDAL, which owns its own paging loop."
       [driver-url]
       (gdal/init!)
       @gdal-http-callback
       (load-source driver-url))

     (defn- with-geoservices-auth
       "Run f with the bearer token on each request, paging included."
       [auth f]
       (if-let [token (:token auth)]
         (let [base gdal-network/*transport*]
           (binding [gdal-network/*transport*
                     (fn [request]
                       (base (update request :headers assoc
                                     "X-Esri-Authorization" (str "Bearer " token))))]
             (f)))
         (f)))

     ;; The rule of the JS clip-flat, so that both runtimes keep the same rings.

     (defn- ring-meets-box?
       [^LinearRing ring [x0 y0 x1 y1]]
       (let [e (.getEnvelopeInternal ring)]
         (and (>= (.getMaxX e) (double x0)) (<= (.getMinX e) (double x1))
              (>= (.getMaxY e) (double y0)) (<= (.getMinY e) (double y1)))))

     (defn- clip-polygon
       "The part `p` with the holes that meet `box`, or nil when its shell
        misses the box."
       ^Polygon [^Polygon p box ^GeometryFactory fac]
       (when (ring-meets-box? (.getExteriorRing p) box)
         (let [holes (filterv #(ring-meets-box? % box)
                              (map #(.getInteriorRingN p (int %)) (range (.getNumInteriorRing p))))]
           (if (= (count holes) (.getNumInteriorRing p))
             p
             (.createPolygon fac (.getExteriorRing p) ^"[Lorg.locationtech.jts.geom.LinearRing;"
                             (into-array LinearRing holes))))))

     (defn- clip-polygonal
       "`g` with only the polygon parts and holes that meet `box`. `g` comes
        back as it is when it is not polygonal, when the box keeps each ring
        and when it keeps no part."
       ^Geometry [^Geometry g box]
       (if-not (instance? Polygonal g)
         g
         (let [fac   (.getFactory g)
               parts (mapv #(.getGeometryN g (int %)) (range (.getNumGeometries g)))
               kept  (into [] (keep #(clip-polygon % box fac)) parts)]
           (if (or (empty? kept)
                   (and (= (count kept) (count parts)) (every? true? (map identical? kept parts))))
             g
             (let [^Geometry out (if (= 1 (count kept))
                                   (first kept)
                                   (.createMultiPolygon fac ^"[Lorg.locationtech.jts.geom.Polygon;"
                                                        (into-array Polygon kept)))]
               (.setUserData out (.getUserData g))
               out)))))

     (defn- clip-rows
       [result box]
       (if box
         (update result :rows
                 (fn [rows] (mapv #(update % :geometry (fn [g] (some-> g (clip-polygonal box)))) rows)))
         result))

     (defn load-gdal-source
       "Open source with GDAL and walk the first layer into
        {:rows [...] :source-crs {...}}, as load-source does. source is a
        local path, an \"ESRIJSON:\" query URL, or a GeoServices REST
        (FeatureServer/MapServer) layer URL.

        For a layer URL, these opts make the query: :where (default \"1=1\"),
        :out-fields (default \"*\"), :order-by (default \"OBJECTID ASC\",
        because paging needs a stable order), :result-record-count (default
        1000) and :extra-params, a map of more query params. :auth
        {:token t} adds X-Esri-Authorization: Bearer <t> to each request.

        :clip-extent [min-x min-y max-x max-y], in the CRS of the response,
        drops each polygon part and hole of a layer URL that misses it.

        :fetch-format :pbf (default) reads f=pbf through cg.featureserver-pbf
        and falls back to the driver on a failure. :json uses the driver.
        :pbf-strict does not fall back. The PBF path also takes :retries
        (default 3), :retry-delay-ms (default 250, doubled for each attempt)
        and :max-pages, a limit for a server that ignores resultOffset.

        The PBF decoder throws on a curve geometry, a shape buffer, hasZ and
        hasM. Network loads go through the CPLHTTPFetch host callback, not
        libcurl."
       [source & {:keys [auth fetch-format clip-extent] :or {fetch-format :pbf} :as opts}]
       (let [layer? (geoservices-layer-url? source)
             target (cond
                      (str/starts-with? source "ESRIJSON:") source
                      layer? (str "ESRIJSON:"
                                  (build-geoservices-query-url source
                                                               (assoc opts :wire-format "json")))
                      :else source)]
         (if (str/starts-with? target "ESRIJSON:")
           (with-geoservices-auth
             auth
             (fn run-geoservices []
               (if (or (not layer?) (= fetch-format :json))
                 (cond-> (load-through-driver target)
                   layer? (clip-rows clip-extent))
                 (clip-rows
                  (try
                    (load-geoservices-pbf source opts)
                    (catch Exception e
                      (when (= fetch-format :pbf-strict) (throw e))
                      (log/warn e (str "cg.io: f=pbf failed for " source
                                       ", falling back to the GDAL driver"))
                      (load-through-driver target)))
                  clip-extent))))
           (load-source target))))))

#?(:cljs
   (do
     (defn- build-geoservices-query-url-cljs
       "Build the query URL for a layer endpoint. opts is a JS object with
        the keys where, outFields, orderBy, resultRecordCount and extraParams."
       [layer-url opts]
       (let [where        (or (aget opts "where") "1=1")
             out-fields   (or (aget opts "outFields") "*")
             order-by     (or (aget opts "orderBy") "OBJECTID ASC")
             result-count (or (aget opts "resultRecordCount") 1000)
             extra-params (aget opts "extraParams")
             base         (str (.replace layer-url (js/RegExp. "/$") "") "/query"
                               "?f=json"
                               "&where=" (url-encode-cljs where)
                               "&outFields=" (url-encode-cljs out-fields)
                               "&orderByFields=" (url-encode-cljs order-by)
                               "&resultRecordCount=" result-count)
             extras       (if extra-params
                            (apply str (for [k (js-keys extra-params)]
                                         (str "&" k "="
                                              (url-encode-cljs
                                               (aget extra-params k)))))
                            "")]
         (str base extras)))

     (defn- ^:async load-gdal-url
       "Open an ESRIJSON: query URL through the worker and walk it."
       [target]
       (await (gdal/gdal-all-register))
       (let [ds (await (gdal/gdal-open-ex target gdal-fndefs/GDAL_OF_VECTOR nil nil nil))]
         (when (or (nil? ds) (= ds 0))
           (throw (js/Error.
                   (str "cg.io/load-gdal-source: open-ex returned null for "
                        target))))
         (try
           (await (walk-dataset-cljs! ds))
           (finally
             (await (gdal/gdal-close ds))))))

     ;; A layer URL pages here and not in the GDAL driver, whose walk costs
     ;; one worker message for each field of each feature and gives no
     ;; progress. The page URLs, the fids and the field values are those of
     ;; the driver.

     (defn- more-pages?
       "True when the ESRI JSON page `bytes` has exceededTransferLimit: true
        at its top level, before or after the features."
       [bytes]
       (let [dec  (js/TextDecoder.)
             n    (.-length bytes)
             head (.decode dec (.subarray bytes 0 (min n 65536)))
             cut  (.indexOf head "\"features\"")
             head (if (neg? cut) head (.slice head 0 cut))
             tail (.decode dec (.subarray bytes (max 0 (- n 1000))))
             flag (js/RegExp. "\"exceededTransferLimit\"\\s*:\\s*true")]
         (or (.test flag head) (.test flag tail))))

     (defn- ^:async fetch-page-once
       "#js {:bytes}, #js {:status} for a response that is not ok, or
        #js {:error} for a network failure."
       [url]
       (try
         (let [resp (await (js/fetch url))]
           (if (.-ok resp)
             #js {:bytes (js/Uint8Array. (await (.arrayBuffer resp)))}
             #js {:status (.-status resp)}))
         (catch :default e
           #js {:error e})))

     (defn- ^:async fetch-page-bytes
       "GET one page. A network failure, a 5xx and a 429 get three attempts,
        with 250 ms and then 500 ms between them, as the JVM page fetch."
       [url]
       (loop [attempt 1 delay 250]
         (let [r      (await (fetch-page-once url))
               status (.-status r)]
           (cond
             (.-bytes r) (.-bytes r)

             (and (< attempt 3)
                  (or (.-error r) (>= status 500) (= status 429)))
             (do (await (js/Promise. (fn [resolve] (js/setTimeout resolve delay))))
                 (recur (inc attempt) (* 2 delay)))

             (.-error r) (throw (.-error r))

             :else
             (throw (js/Error. (str "cg.io/load-gdal-source: HTTP " status
                                    " for " url)))))))

     (defn- ^:async field-types-cljs
       "#js {<field name> <OGR field type>} of a layer defn."
       [lyr-defn]
       (let [n   (await (gdal/ogr-fd-get-field-count lyr-defn))
             out #js {}]
         (loop [i 0]
           (if (>= i n)
             out
             (let [fd (await (gdal/ogr-fd-get-field-defn lyr-defn i))
                   nm (await (gdal/ogr-fld-get-name-ref fd))]
               (aset out nm (await (gdal/ogr-fld-get-type fd)))
               (recur (inc i)))))))

     (defn- ^:async read-page!
       "Parse one ESRI JSON page with GDAL: #js {:features :types :crs}, the
        last two only for `first?`. nil when a later page has no layer."
       [bytes first?]
       (let [staged (await (gdal/stage-files! (js-obj "page.json" bytes) "/work/cg-fetch"))
             path   (aget staged "page.json")
             ds     (await (gdal/gdal-open-ex path gdal-fndefs/GDAL_OF_VECTOR nil nil nil))]
         (if (or (nil? ds) (= ds 0))
           (do (await (gdal/vsi-unlink path))
               (when first?
                 (throw (js/Error. "cg.io/load-gdal-source: GDAL cannot read the response as ESRI JSON"))))
           (try
             (let [layer (await (gdal/gdal-dataset-get-layer ds 0))]
               #js {:features (await (gdal/read-vector-features! layer))
                    :types    (when first?
                                (await (field-types-cljs (await (gdal/ogr-l-get-layer-defn layer)))))
                    :crs      (when first? (await (layer-crs-cljs layer)))})
             (finally
               (await (gdal/gdal-close ds))
               (await (gdal/vsi-unlink path)))))))

     ;; WKBReader takes a byte array one element for each call into WASM.
     ;; fromFlat takes all coordinates of a geometry as one array, so a 2D
     ;; WKB of GDAL is read here, with a DataView.

     (def ^:private flat-types
       #js [nil "Point" "LineString" "Polygon" "MultiPoint" "MultiLineString" "MultiPolygon"])

     (defn- wkb->flat
       "The fromFlat arguments of a little-endian 2D WKB. nil for a Z, an M,
        a collection, an empty part or a ring of under four points."
       [bytes]
       (let [len    (.-byteLength bytes)
             dv     (js/DataView. (.-buffer bytes) (.-byteOffset bytes) len)
             coords (js/Float64Array. (* 2 (quot len 16)))
             rings  #js [0]
             parts  #js [0]
             st     #js {:pos 0 :n 0 :ok true}
             u32!   (fn []
                      (let [v (.getUint32 dv (.-pos st) true)]
                        (set! (.-pos st) (+ 4 (.-pos st)))
                        v))
             type!  (fn []
                      (if (= 1 (.getUint8 dv (.-pos st)))
                        (do (set! (.-pos st) (inc (.-pos st)))
                            (u32!))
                        0))
             points! (fn [k]
                       (let [n   (.-n st)
                             pos (.-pos st)]
                         (dotimes [i k]
                           (aset coords (* 2 (+ n i)) (.getFloat64 dv (+ pos (* 16 i)) true))
                           (aset coords (inc (* 2 (+ n i))) (.getFloat64 dv (+ pos 8 (* 16 i)) true)))
                         (set! (.-n st) (+ n k))
                         (set! (.-pos st) (+ pos (* 16 k)))))
             line!  (fn [min-points]
                      (let [k (u32!)]
                        (when (< k min-points) (set! (.-ok st) false))
                        (points! k)
                        (.push rings (.-n st))))
             polygon! (fn []
                        (let [k (u32!)]
                          (when (zero? k) (set! (.-ok st) false))
                          (dotimes [_ k] (line! 4))
                          (.push parts (dec (.-length rings)))))
             many!  (fn [part-type part!]
                      (let [k (u32!)]
                        (when (zero? k) (set! (.-ok st) false))
                        (dotimes [_ k]
                          (if (and (.-ok st) (= part-type (type!)))
                            (part!)
                            (set! (.-ok st) false)))))
             t      (type!)]
         (cond
           (= t 1) (points! 1)
           (= t 2) (line! 2)
           (= t 3) (polygon!)
           (= t 4) (many! 1 (fn [] (points! 1)))
           (= t 5) (many! 2 (fn [] (line! 2)))
           (= t 6) (many! 3 polygon!)
           :else (set! (.-ok st) false))
         (when (and (.-ok st)
                    (= len (.-pos st))
                    (not (js/Number.isNaN (aget coords 0))))
           #js {:type   (aget flat-types t)
                :coords (.subarray coords 0 (* 2 (.-n st)))
                :rings  (when (or (= t 3) (= t 5) (= t 6)) rings)
                :parts  (when (= t 6) parts)})))

     ;; A clip box keeps a large polygon of a layer from holding each hole
     ;; and part far from the features of the analysis. No ring is cut, so
     ;; a geometry in the box stays the same. A box in another CRS meets no
     ;; ring, and then nothing is dropped.

     (defn- ring-meets-box?
       "True when the bounding box of the points `a` to `b` of `coords` meets
        `box` [min-x min-y max-x max-y]."
       [coords a b box]
       (loop [p a x0 js/Infinity y0 js/Infinity x1 (- js/Infinity) y1 (- js/Infinity)]
         (if (< p b)
           (let [x (aget coords (* 2 p))
                 y (aget coords (inc (* 2 p)))]
             (recur (inc p) (min x0 x) (min y0 y) (max x1 x) (max y1 y)))
           (and (>= x1 (aget box 0)) (<= x0 (aget box 2))
                (>= y1 (aget box 1)) (<= y0 (aget box 3))))))

     (defn- kept-rings
       "The ring indexes of `flat` that `box` keeps, as #js {:rings :parts}:
        each part whose shell meets the box, with each of its holes that
        meets the box. :parts holds the index in :rings where a part starts."
       [flat box]
       (let [coords (.-coords flat)
             rings  (.-rings flat)
             nr     (dec (.-length rings))
             parts  (or (.-parts flat) #js [0 nr])
             meets? (fn [r] (ring-meets-box? coords (aget rings r) (aget rings (inc r)) box))
             out    #js {:rings #js [] :parts #js []}]
         (dotimes [p (dec (.-length parts))]
           (let [r0 (aget parts p)]
             (when (meets? r0)
               (.push (.-parts out) (.-length (.-rings out)))
               (.push (.-rings out) r0)
               (loop [r (inc r0)]
                 (when (< r (aget parts (inc p)))
                   (when (meets? r) (.push (.-rings out) r))
                   (recur (inc r)))))))
         out))

     (defn- clip-flat
       "`flat` with only the rings of a polygon that `box` keeps. `flat` comes
        back as it is when `box` is nil, when it keeps each ring and when it
        keeps no part."
       [flat box]
       (if (or (nil? box)
               (not (contains? #{"Polygon" "MultiPolygon"} (.-type flat))))
         flat
         (let [kept  (kept-rings flat box)
               keep  (.-rings kept)
               rings (.-rings flat)]
           (if (or (zero? (.-length keep))
                   (= (.-length keep) (dec (.-length rings))))
             flat
             (let [coords (.-coords flat)
                   n      (reduce (fn [acc r] (+ acc (- (aget rings (inc r)) (aget rings r)))) 0 keep)
                   out    (js/Float64Array. (* 2 n))
                   starts #js [0]]
               (doseq [r keep]
                 (let [a (aget rings r)
                       b (aget rings (inc r))
                       at (aget starts (dec (.-length starts)))]
                   (.set out (.subarray coords (* 2 a) (* 2 b)) (* 2 at))
                   (.push starts (+ at (- b a)))))
               (if (> (.-length (.-parts kept)) 1)
                 (let [parts (.-parts kept)]
                   (.push parts (.-length keep))
                   #js {:type "MultiPolygon" :coords out :rings starts :parts parts})
                 #js {:type "Polygon" :coords out :rings starts :parts nil}))))))

     (defn- read-gdal-wkb
       "The geometry of a WKB from GDAL: through fromFlat, or through
        WKBReader when wkb->flat gives nil or fromFlat refuses the rings.
        `box` clips the flat arrays, see clip-flat."
       [bytes box]
       (or (try
             (when-let [flat (some-> (wkb->flat bytes) (clip-flat box))]
               (js/globalThis.wasmts.geom.fromFlat
                (.-type flat) (.-coords flat) 2 (.-rings flat) (.-parts flat)))
             (catch :default _ nil))
           (read-wkb bytes)))

     ;; GDAL parses a page into a json-c tree in the heap of worker 0, about
     ;; seven times the size of the page, and an Emscripten heap does not
     ;; shrink: the page of 34 MB grows it by 243 MB. A page of 2D polygons
     ;; with plain fields is read here with JSON.parse. On each page of the
     ;; Roslindale layers this gives the flat arrays of the GDAL WKB.

     (def ^:private esri-plain-field-types
       #js {"esriFieldTypeOID" true "esriFieldTypeInteger" true
            "esriFieldTypeSmallInteger" true "esriFieldTypeDouble" true
            "esriFieldTypeString" true})

     (defn- esri-ring-area2
       "Twice the signed area of the closed ring from point `a` to point `b`."
       [coords a b]
       (loop [p a acc 0]
         (if (>= p (dec b))
           acc
           (recur (inc p)
                  (+ acc (- (* (aget coords (* 2 p)) (aget coords (+ 3 (* 2 p))))
                            (* (aget coords (+ 2 (* 2 p))) (aget coords (inc (* 2 p))))))))))

     (defn- esri-rings->flat
       "The fromFlat arguments of the rings of an ESRI JSON polygon. nil for an
        open ring, a ring of under four points or a first ring that is not
        clockwise, which GDAL may read in a different way."
       [rs]
       (let [total  (reduce (fn [a r] (+ a (.-length r))) 0 rs)
             coords (js/Float64Array. (* 2 total))
             rings  #js [0]
             st     #js {:n 0 :ok true}]
         (doseq [r rs]
           (let [k  (.-length r)
                 r0 (.-n st)]
             (dotimes [i k]
               (aset coords (* 2 (+ r0 i)) (aget (aget r i) 0))
               (aset coords (inc (* 2 (+ r0 i))) (aget (aget r i) 1)))
             (when (or (< k 4)
                       (not= (aget coords (* 2 r0)) (aget coords (* 2 (+ r0 k -1))))
                       (not= (aget coords (inc (* 2 r0))) (aget coords (inc (* 2 (+ r0 k -1))))))
               (set! (.-ok st) false))
             (set! (.-n st) (+ r0 k))
             (.push rings (.-n st))))
         (when (and (.-ok st) (neg? (esri-ring-area2 coords 0 (aget rings 1))))
           ;; The ring rule of cg.featureserver-pbf: a clockwise ring opens a
           ;; polygon, a counter-clockwise ring is a hole of the one before.
           (let [nr    (dec (.-length rings))
                 parts #js [0]]
             (loop [r 1]
               (when (< r nr)
                 (when (neg? (esri-ring-area2 coords (aget rings r) (aget rings (inc r))))
                   (.push parts r))
                 (recur (inc r))))
             (.push parts nr)
             (if (> (.-length parts) 2)
               #js {:type "MultiPolygon" :coords coords :rings rings :parts parts}
               #js {:type "Polygon" :coords coords :rings rings :parts nil})))))

     (defn- esri-json-feature
       "#js {:fid :fields :geom} of one feature, or nil when GDAL must read it.
        `box` clips the rings, see clip-flat."
       [f oid names box]
       (let [attrs (.-attributes f)
             g     (.-geometry f)
             rs    (when g (.-rings g))
             flat  (when (and rs (pos? (.-length rs)))
                     (some-> (esri-rings->flat rs) (clip-flat box)))
             geom  (when flat
                     (try
                       (js/globalThis.wasmts.geom.fromFlat
                        (.-type flat) (.-coords flat) 2 (.-rings flat) (.-parts flat))
                       (catch :default _ nil)))
             fid   (when attrs (aget attrs oid))]
         (when (and geom (number? fid))
           (let [row #js {}]
             (doseq [nm names]
               (let [v (aget attrs nm)]
                 (aset row nm (if (nil? v) nil v))))
             #js {:fid fid :fields row :geom geom}))))

     (defn- esri-json-page
       "#js {:features} of an ESRI JSON page of 2D polygons with plain fields,
        or nil when GDAL must read the page."
       [bytes box]
       (let [j      (js/JSON.parse (.decode (js/TextDecoder.) bytes))
             oid    (.-objectIdFieldName j)
             fields (or (.-fields j) #js [])
             names  (.map fields (fn [f] (.-name f)))
             feats  (or (.-features j) #js [])]
         (when (and (= "esriGeometryPolygon" (.-geometryType j))
                    (not (.-hasZ j))
                    (not (.-hasM j))
                    (string? oid)
                    (.some fields (fn [f] (= oid (.-name f))))
                    (.every fields (fn [f] (true? (aget esri-plain-field-types (.-type f))))))
           (let [out #js []]
             (loop [i 0]
               (if (>= i (.-length feats))
                 #js {:features out}
                 (when-let [row (esri-json-feature (aget feats i) oid names box)]
                   (.push out row)
                   (recur (inc i)))))))))

     (defn- feature->row!
       "Return the row of a feature. A field with no value gets 0, or \"\" for a
        string. `box` clips the geometry of a GDAL feature, see clip-flat."
       [feat types fid box]
       (let [row  (.-fields feat)
             wkb  (.-wkb feat)
             geom (.-geom feat)]
         (doseq [k (js/Object.keys row)]
           (when (nil? (aget row k))
             (let [t (aget types k)]
               (aset row k (if (or (= 0 t) (= 2 t) (= 12 t)) 0 "")))))
         (aset row "fid" fid)
         (aset row "geometry" (or geom (when wkb (read-gdal-wkb wkb box))))
         row))

     (defn- ^:async load-geoservices-pages
       "Page the f=json query URL of a layer. `on-page`, when given, gets the
        row count after each page. `box` clips the polygons, see clip-flat.
        Resolves to #js {:rows :sourceCrs}."
       [query-url on-page box]
       (await (gdal/gdal-all-register))
       (let [out #js []
             st  #js {:types nil :crs nil :sequential false :lastFid -1 :firstFid nil}]
         (loop [offset 0 page-idx 0]
           (let [url   (if (zero? page-idx) query-url (str query-url "&resultOffset=" offset))
                 bytes (await (fetch-page-bytes url))
                 label (str "parse page " (inc page-idx) " of " (source-label query-url))
                 ;; The first page goes through GDAL for the field types and
                 ;; the CRS of the layer.
                 json  (when (pos? page-idx)
                         (let [busy (trace/busy-start! "main" "main" label nil)
                               p    (esri-json-page bytes box)]
                           (trace/busy-end! busy)
                           p))
                 page  (or json
                           ;; gdal-wasm sends each GDAL call to worker 0.
                           (let [busy (trace/busy-start! 0 "gdal" label nil)
                                 p    (try
                                        (await (read-page! bytes (zero? page-idx)))
                                        (catch :default e
                                          (trace/busy-end! busy (ex-message e))
                                          (throw e)))]
                             (trace/busy-end! busy)
                             p))
                 feats (if page (.-features page) #js [])
                 n     (.-length feats)
                 fid0  (when (pos? n) (js/Number (.-fid (aget feats 0))))]
             (when (zero? page-idx)
               (set! (.-types st) (.-types page))
               (set! (.-crs st) (.-crs page)))
             ;; The driver numbers the rows in sequence when each page starts
             ;; its fids at 0, and stops when a server repeats the first page.
             (when (and (= 1 page-idx) (= 0 fid0) (= (.-lastFid st) (dec (.-length out))))
               (set! (.-sequential st) true))
             (if (and (= 1 page-idx) (some? fid0) (not= 0 fid0) (= fid0 (.-firstFid st)))
               nil
               (do
                 (doseq [feat feats]
                   (let [fid (if (.-sequential st) (.-length out) (js/Number (.-fid feat)))]
                     (when (zero? (.-length out)) (set! (.-firstFid st) fid))
                     (set! (.-lastFid st) fid)
                     ;; A BigInt, as OGR_F_GetFID gives it.
                     (.push out (feature->row! feat (.-types st) (js/BigInt fid) box))))
                 (when on-page (on-page (.-length out)))
                 (when (and (pos? n) (more-pages? bytes))
                   (recur (+ offset n) (inc page-idx)))))))
         #js {:rows out :sourceCrs (.-crs st)}))

     (defn ^:async load-gdal-source
       "Open source with GDAL through the worker. source is a local path, an
        ESRIJSON: query URL, or a FeatureServer/MapServer layer URL. opts is
        a JS object with the keys where, outFields, orderBy,
        resultRecordCount, extraParams, auth, onPage (a fn of the row
        count after each page) and clipExtent, a box [min-x min-y max-x
        max-y] in the CRS of the response that drops each polygon part and
        hole of a layer URL that misses it. auth {token: t} appends &token=<t> to the
        query URL. The JVM branch sends an X-Esri-Authorization header. This
        fn has one arity because of the multi-arity ^:async problem of
        squint: pass nil for the defaults."
       [source opts]
       (let [opts   (or opts #js {})
             token  (when-let [a (aget opts "auth")] (aget a "token"))
             with-token (fn [url]
                          (if token
                            (str url
                                 (if (str/includes? url "?") "&" "?")
                                 "token=" (url-encode-cljs token))
                            url))]
         (cond
           (str/starts-with? source "ESRIJSON:")
           (await (load-gdal-url (with-token source)))

           (geoservices-layer-url? source)
           (await (load-geoservices-pages
                   (with-token (build-geoservices-query-url-cljs source opts))
                   (aget opts "onPage")
                   (aget opts "clipExtent")))

           :else
           (await (load-source source)))))))
