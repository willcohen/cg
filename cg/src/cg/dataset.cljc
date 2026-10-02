;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; Spatial operations on datasets: tech.ml.dataset on the JVM, Arquero tables
;; on JavaScript. A geometry column holds JTS Geometry on the JVM and WasmTS
;; Geometry on JavaScript.

(ns cg.dataset
  (:refer-clojure :exclude [filter group-by partition-by reduce])
  (:require [cg.feature :as feature]
            [cg.geo :as geo]
            [cg.io :as cio]
            [cg.util :as util]
            #?(:clj [clojure.core.protocols :as protocols])
            #?(:clj [clojure.string :as cstr])
            #?(:clj [clojure.tools.logging :as log])
            #?(:clj [tablecloth.api :as tc])
            #?(:clj [tech.v3.dataset :as ds])
            #?(:clj [tech.v3.dataset.column-filters :as dscf])
            #?(:clj [ham-fisted.api :as hamf])
            #?(:clj [charred.api :as charred])
            #?(:clj [net.willcohen.proj.proj :as proj])
            #?(:cljs ["proj-wasm" :as proj-wasm])
            #?(:cljs ["ffi-wasm" :as ffi])
            #?(:cljs [cg.pool :as cg-pool])
            #?(:clj [cg.trace :as trace]
               :cljs [cg.trace-js :as trace]))
  #?(:clj (:import (org.locationtech.jts.index.hprtree HPRtree)
                   (org.locationtech.jts.geom Geometry)
                   (org.locationtech.jts.geom.prep PreparedGeometry))))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj (declare add-spatial-index))
#?(:cljs (declare add-column column))

(defn stage!
  "Emits a \"stage\" trace event for long work that is not a flow step.
   `state` is \"start\", \"progress\" or \"done\"; `n` and `total` are counts."
  ([label state] (stage! label state nil nil))
  ([label state n] (stage! label state n nil))
  ([label state n total]
   (trace/emit! "stage" (cond-> {:label label :state state}
                          (some? n) (assoc :n n)
                          (some? total) (assoc :total total)))
   nil))

(def ^:private index-label "build spatial index")

(defn- reproject-stage
  "The stage events of one reprojection flow. The stage is done at the last
   row of the table, or at :end! when the batches carry no row count."
  [target-crs]
  (let [label (str "reproject to " target-crs)
        seen  (atom nil)
        total (atom nil)
        end!  (fn [] (when-let [n @seen]
                       (reset! seen nil)
                       (stage! label "done" n @total)))]
    {:begin! (fn [table-rows]
               (when (nil? @seen)
                 (reset! seen 0)
                 (reset! total table-rows)
                 (stage! label "start" 0 table-rows)))
     :add!   (fn [n]
               (when (some? @seen)
                 (swap! seen + n)
                 (stage! label "progress" @seen @total)
                 (when (= @seen @total) (end!))))
     :end!   end!}))

;; Arquero notes: select rows with .reify(indices), because .slice takes a
;; start and an end. Column names are strings; the JVM uses keywords.

#?(:cljs
   (do
     (def ^:private aq-module (atom nil))

     (defn- ensure-arquero!
       "Returns the aq module. Throws when init-arquero! did not run."
       []
       (when (nil? @aq-module)
         (throw (ex-info "Arquero not initialized. Call init-arquero! first." {})))
       @aq-module)

     (defn init-arquero!
       "Loads the Arquero module. Returns a promise. Call it before any
        dataset operation."
       []
       (-> (js/import "arquero")
           (.then (fn [aq]
                    (reset! aq-module aq)
                    ;; The Table class loads async: a top-level extend-type
                    ;; cannot name it. A cross-namespace extend-type needs
                    ;; squint >= 0.14.204 (squint #955).
                    (let [Table (.-Table aq)]
                      (extend-type Table geo/GeoInput
                                   (-geo-map [ds f col-name]
                                     (add-column ds col-name
                                                 (mapv f (column ds :geometry))))))
                    aq))))))

;; The CLJS FlowDataset is a plain JS object with a __flow_dataset marker (see
;; the JVM deftype FlowDataset for its purpose). A deftype with IMeta throws a
;; ReferenceError at module load, because squint does not define the IMeta
;; protocol. Read the metadata with fd-meta.

#?(:cljs
   (do
     (defn ->FlowDataset
       "Returns a FlowDataset wrapper: a plain JS object."
       ([dataset] (->FlowDataset dataset {}))
       ([dataset metadata-map]
        #js {:__flow_dataset true
             :dataset dataset
             :metadata (or metadata-map {})}))

     (defn wrapped-dataset?
       "True when `x` is a FlowDataset wrapper."
       [x]
       (and (some? x)
            (true? (.-__flow_dataset x))))

     (defn unwrap-dataset
       "Returns the dataset inside a FlowDataset. Returns any other input
        unchanged."
       [wrapped]
       (if (wrapped-dataset? wrapped)
         (.-dataset wrapped)
         wrapped))

     (defn wrap-bundle
       "Wraps a map in a plain JS object, because flow iterates over the
        entries of a bare map."
       [m]
       #js {:__flow_bundle true :data m})

     (defn unwrap-bundle
       "Returns the map inside a FlowBundle."
       [fb]
       (.-data fb))))

(defn dataset?
  "True when `x` is a dataset: a tech.ml.dataset on the JVM, an Arquero table
   or a FlowDataset on CLJS."
  [x]
  #?(:clj (ds/dataset? x)
     :cljs (or (wrapped-dataset? x)
               (and (some? x)
                    (some? @aq-module)
                    (instance? (.-Table (ensure-arquero!)) x)))))

(defn row-count
  "Returns the number of rows in a dataset."
  [dataset]
  #?(:clj (ds/row-count dataset)
     :cljs (.numRows (unwrap-dataset dataset))))

(defn column-names
  "Returns the column names of a dataset."
  [dataset]
  #?(:clj (ds/column-names dataset)
     :cljs (vec (.columnNames (unwrap-dataset dataset)))))

(defn has-column?
  "True when the dataset has the column `col-name`."
  [dataset col-name]
  #?(:clj (contains? (set (ds/column-names dataset)) col-name)
     :cljs (let [raw-ds (unwrap-dataset dataset)
                 col-str (str col-name)]
             (.some (.columnNames raw-ds) (fn [c] (= c col-str))))))

(defn rows
  "Returns the rows of a dataset. `fmt` is :as-maps (default) or :as-seqs."
  ([dataset]
   (rows dataset :as-maps))
  ([dataset fmt]
   #?(:clj (ds/rows dataset fmt)
      :cljs
      ;; Under squint the plain JS objects of .objects() are Clojure maps.
      (let [raw-ds (unwrap-dataset dataset)
            objs (vec (.objects raw-ds))]
        (if (= fmt :as-maps)
          objs
          (map vals objs))))))

#?(:cljs
   (defn- coerce-column-map
     "Converts a plain JS object or a seq of row maps to a map of columns,
      one level deep, because a deep js->clj fails on a WasmTS geometry Proxy."
     [m]
     (cond
       (sequential? m)
       (into {} (map (fn [col] [col (mapv #(get % col) m)]))
             (distinct (mapcat keys m)))
       (map? m) m
       (and m (= "Object" (.. m -constructor -name)))
       (clojure.core/reduce (fn [acc k] (assoc acc k (aget m k)))
                            {} (js/Object.keys m))
       :else m)))

(defn ->dataset
  "Creates a dataset from a map of column name to column data, or from a
   sequence of row maps."
  [column-map]
  #?(:clj (ds/->dataset column-map)
     :cljs (let [aq (ensure-arquero!)
                 cm (coerce-column-map column-map)
                 ;; No clj->js here: it descends into the WasmTS Proxy
                 ;; geometry values and throws (UPSTREAM(squint) #11).
                 js-obj (into {}
                              (map (fn [[k v]]
                                     [(str k)
                                      (to-array v)])
                                   cm))]
             (.table aq js-obj))))

(defn maps->dataset
  "Creates a dataset from a sequence of row maps. The inverse of
   `(rows ds :as-maps)`."
  [row-maps]
  #?(:clj (ds/->>dataset row-maps)
     :cljs (->dataset (vec row-maps))))

(declare filter-ds)

(defn- serializable-row-pred
  "Returns a row predicate for `(filter :col :predicate value)`.
   `pred` is a keyword on the JVM and a string under squint."
  [column pred value]
  (let [p (util/unq-name pred)]
    (fn [row]
      (let [v (get row column)]
        (case p
          "in"            (contains? (set value) v)
          ("equals" "=")  (= v value)
          ("not-equals" "not=") (not= v value)
          ("lt" "<")      (< v value)
          ("gt" ">")      (> v value)
          ("lte" "<=")    (<= v value)
          ("gte" ">=")    (>= v value)
          "some?"         (some? v)
          "nil?"          (nil? v)
          "matches-regex" (boolean (re-find (re-pattern (str value)) (str v)))
          (throw (ex-info (str "filter: unsupported predicate " (pr-str pred))
                          {:predicate pred :column column :value value})))))))

(defn filter
  "Filters the rows of a dataset. `(filter dataset pred)` takes a fn of a row
   map. `(filter dataset column predicate value)` takes a predicate keyword
   from cg.dsl.signatures/predicate-kinds, for example :in, :equals or :gt."
  ([dataset pred]
   (filter-ds dataset pred))
  ([dataset column predicate value]
   (filter-ds dataset (serializable-row-pred column predicate value))))

(defn filter-ds
  "Filters dataset rows. `pred` receives a map of column name to value for
   each row."
  [dataset pred]
  #?(:clj (ds/filter dataset pred)
     :cljs (let [raw-ds (unwrap-dataset dataset)
                 indices (keep-indexed
                          (fn [idx row]
                            (when (pred row) idx))
                          (rows raw-ds :as-maps))]
             (.reify raw-ds (vec indices)))))

(defn filter-column
  "Keeps the rows whose value in column `col-name` satisfies `pred`."
  [dataset col-name pred]
  #?(:clj (ds/filter-column dataset col-name pred)
     :cljs (let [raw-ds (unwrap-dataset dataset)
                 col-str (str col-name)
                 col-vals (.array raw-ds col-str)
                 indices (into []
                               (keep-indexed
                                (fn [idx val]
                                  (when (pred val) idx)))
                               col-vals)]
             (.reify raw-ds indices))))

(defn geojson->dataset
  "Converts a GeoJSON string to a dataset. On the JVM, builds a spatial index
   unless `generate-index?` is false. CLJS builds no index."
  ([geojson]
   (geojson->dataset geojson true))
  (#_{:clj-kondo/ignore [:unused-binding]}
   [geojson generate-index?]
   #?(:clj
      (let [gj (charred/read-json geojson :key-fn keyword)
            crs (:crs gj)]
        (-> (->> (:features gj)
                 (map (fn [f]
                        (let [geometry (cio/read-geojson (charred/write-json-str (:geometry f)))
                              _ (if (not (nil? crs))
                                  (feature/set-crs geometry crs)
                                  (feature/set-crs geometry "EPSG:4326"))
                              properties (:properties f)]
                          (assoc properties :geometry geometry))))
                 (ds/->>dataset))
            (cond-> generate-index? add-spatial-index)))

      :cljs
      (let [gj (.parse js/JSON geojson)
            crs (or (:crs gj) "EPSG:4326")
            features (:features gj)
            row-maps (mapv (fn [f]
                             (let [geom-json (.stringify js/JSON (:geometry f))
                                   geometry (cio/read-geojson geom-json)]
                               (feature/set-crs geometry crs)
                               (assoc (:properties f) :geometry geometry)))
                           features)]
        (if (empty? row-maps)
          (->dataset {})
          (let [cols (keys (first row-maps))
                col-map (into {}
                              (map (fn [col]
                                     [col (mapv #(get % col) row-maps)])
                                   cols))]
            (->dataset col-map)))))))

#?(:clj
   (defn- rows-result->dataset
     "Builds a dataset from the {:rows :source-crs} result of cg.io. The
      geometries already have the source CRS."
     [{:keys [rows]}]
     (ds/->>dataset rows)))

#?(:cljs
   (defn- cljs-result->dataset
     "Builds an Arquero table from the #js {:rows :sourceCrs} result of the
      cg.io cljs loaders. Sets the source CRS on each row geometry."
     [result]
     (let [rows    (.-rows result)
           crs-str (cio/source-crs->string (.-sourceCrs result))]
       (doseq [r rows]
         (when (and crs-str (.-geometry r))
           (feature/set-crs (.-geometry r) crs-str)))
       (if (zero? (.-length rows))
         (->dataset {})
         (let [cols    (.keys js/Object (aget rows 0))
               col-map (into {}
                             (map (fn [col]
                                    [col (mapv (fn [row] (aget row col))
                                               rows)])
                                  cols))]
           (->dataset col-map))))))

(defn ^:async vector-source->dataset
  "Reads a vector source through cg.io/load-source into a dataset whose
   geometries have the source CRS. CLJS returns a promise, is Node-only, and
   needs cg.run/init-cg-workers! first."
  [path]
  #?(:clj
     (rows-result->dataset (cio/load-source path))
     :cljs
     (cljs-result->dataset (await (cio/load-source path)))))

;; GDAL selects the driver from the path or the content. The format-named
;; loaders exist for discovery and for DSL op registration.

(defn ^:async load-vector
  "Alias of vector-source->dataset."
  [path]
  (vector-source->dataset path))

(defn ^:async load-gpkg
  "Loads a GeoPackage (.gpkg) file as a dataset."
  [path]
  (load-vector path))

(defn ^:async load-shapefile
  "Loads a Shapefile as a dataset. Pass the .shp path; GDAL finds the sibling
   .shx, .dbf and .prj files."
  [path]
  (load-vector path))

(defn ^:async load-gdb
  "Loads a File Geodatabase (.gdb directory) as a dataset with the read-only
   OpenFileGDB driver."
  [path]
  (load-vector path))

#?(:clj
   (defn load-gdal!
     "Loads a source that GDAL opens: a local vector path, an \"ESRIJSON:\"
      URL, or a GeoServices REST layer URL. Forwards :where, :out-fields,
      :order-by, :result-record-count, :extra-params, :clip-extent and :auth
      to cg.io/load-gdal-source."
     [source & opts]
     (rows-result->dataset (apply cio/load-gdal-source source opts))))

#?(:cljs
   (defn ^:async load-gdal!
     "Loads a source that GDAL opens. Pass nil `opts` for the defaults. `opts`
      keys: where, outFields, orderBy, resultRecordCount, extraParams,
      clipExtent and auth ({token: t}). Needs cg.run/init-cg-workers! first."
     [source opts]
     (cljs-result->dataset
      (await (cio/load-gdal-source source opts)))))

#?(:clj
   (defn- cache-key-hash [url opts version]
     (let [s (pr-str [url (into (sorted-map) opts) version])
           d (.digest (java.security.MessageDigest/getInstance "SHA-256")
                      (.getBytes s "UTF-8"))]
       (subs (apply str (map #(format "%02x" %) d)) 0 12))))

#?(:clj
   (defn- cache-slug
     "Readable cache-file prefix: <service>-<layer> for GeoServices REST
      layer URLs, sanitized URL/path tail otherwise."
     [url]
     (if-let [[_ svc n] (re-find #"/([^/]+)/(?:FeatureServer|MapServer)/(\d+)/?$" url)]
       (str svc "-" n)
       (let [s (cstr/replace url #"[^A-Za-z0-9._-]+" "_")]
         (subs s (max 0 (- (count s) 60)))))))

#?(:clj
   (defn- loader-cols->cache
     "The cache files need string column names; loader datasets carry
      string attribute names plus the :fid / :geometry keywords."
     [dataset]
     (ds/rename-columns dataset
                        (into {} (for [c (ds/column-names dataset)
                                       :when (keyword? c)]
                                   [c (name c)])))))

#?(:clj
   (defn- cache-cols->loader
     "Inverse of loader-cols->cache for the loader-reserved names."
     [dataset]
     (ds/rename-columns dataset
                        (into {} (for [c (ds/column-names dataset)
                                       :when (contains? #{"geometry" "fid"} c)]
                                   [c (keyword c)])))))

#?(:clj
   (def ^:private ^org.locationtech.jts.io.geojson.GeoJsonWriter cache-geojson-writer
     ;; 16 decimals keep the full precision of a double. The JTS default of 8
     ;; would round every cached coordinate.
     (doto (org.locationtech.jts.io.geojson.GeoJsonWriter. 16)
       (.setEncodeCRS false))))

#?(:clj
   (defn- write-geojson-cache!
     "The ensure-dataset cache: write `dataset`, which has
      string column names, as a GeoJSON FeatureCollection."
     [dataset ^String path]
     (with-open [w (java.io.BufferedWriter. (java.io.FileWriter. path))]
       (.write w "{\"type\":\"FeatureCollection\",\"features\":[")
       (doseq [[i row] (map-indexed vector (ds/mapseq-reader dataset))]
         (when (pos? i) (.write w ","))
         (.write w "{\"type\":\"Feature\",\"geometry\":")
         (.write w (if-let [g (get row "geometry")]
                     (.write cache-geojson-writer ^Geometry g)
                     "null"))
         (.write w ",\"properties\":")
         (.write w ^String (charred/write-json-str (dissoc row "geometry")))
         (.write w "}"))
       (.write w "]}"))))

#?(:clj
   (defn- read-geojson-cache
     "Inverse of write-geojson-cache!. JSON objects do not keep key order, so
      `columns` gives the column order."
     [path columns]
     (let [fc (charred/read-json (slurp path))
           rows (mapv (fn [f]
                        (assoc (or (get f "properties") {})
                               "geometry" (some-> (get f "geometry")
                                                  charred/write-json-str
                                                  cio/read-geojson)))
                      (get fc "features"))
           read (ds/->>dataset rows)
           ;; The writer leaves out a missing value. A column with no value
           ;; in any row is therefore not in the file.
           present (set (ds/column-names read))]
       (cond-> read
         columns (ds/select-columns (filterv present columns))))))

#?(:clj
   (defn ensure-dataset
     "Returns the dataset of a GDAL source, cached as GeoJSON under :cache-dir
      (default <util/cache-root>/geoparquet). Keys: :url, :opts (load-gdal!
      kwargs), :version and :refresh?; a new :version or :refresh? refetches."
     [{:keys [url opts version cache-dir refresh?]
       :or {opts {}
            cache-dir (str (java.io.File. ^String (util/cache-root) "geoparquet"))}}]
     (let [base    (str (cache-slug url) "-" (cache-key-hash url opts version))
           dir     (java.io.File. (str cache-dir))
           data    (java.io.File. dir (str base ".geojson"))
           sidecar (java.io.File. dir (str base ".edn"))]
       (when (or refresh? (not (.exists data)) (not (.exists sidecar)))
         (.mkdirs dir)
         (let [label   (str "fetch " (cio/source-label url))
               _       (stage! label "start")
               fetched (apply load-gdal! url (mapcat identity opts))
               _       (stage! label "done" (ds/row-count fetched))
               _       (when (zero? (ds/row-count fetched))
                         (throw (ex-info (str "cg.dataset/ensure-dataset: fetch returned 0 rows"
                                              " (bad :where / wrong layer / auth?): " url)
                                         {:url url :opts opts :version version})))
               crs     (some #(when % (feature/get-crs %)) (fetched :geometry))
               cached  (loader-cols->cache fetched)]
           (write-geojson-cache! cached (.getPath data))
           (util/write-edn! (.getPath sidecar)
                            {:url url :opts opts :version version
                             :crs crs
                             :columns (vec (ds/column-names cached))
                             :row-count (ds/row-count fetched)
                             :fetched-at (str (java.time.Instant/now))})))
       (let [{:keys [crs columns]} (util/read-edn (.getPath sidecar))
             loaded (cache-cols->loader (read-geojson-cache (.getPath data) columns))]
         (when crs
           (doseq [g (loaded :geometry)]
             (when g (feature/set-crs g crs))))
         loaded))))

#?(:cljs (defonce ^:private fetched-datasets (atom {})))

#?(:cljs
   (defn- ^:async fetch-dataset
     "Fetch the dataset of an ensure-dataset spec, with its stage events."
     [{:keys [url opts version]}]
     (let [label   (str "fetch " (cio/source-label url))
           _       (stage! label "start")
           fetched (await (load-gdal! url #js {:where (:where opts)
                                               :outFields (:out-fields opts)
                                               :orderBy (:order-by opts)
                                               :resultRecordCount (:result-record-count opts)
                                               :extraParams (:extra-params opts)
                                               :clipExtent (:clip-extent opts)
                                               :onPage (fn [n] (stage! label "progress" n))}))]
       (stage! label "done" (row-count fetched))
       (when (zero? (row-count fetched))
         (throw (ex-info (str "cg.dataset/ensure-dataset: fetch returned 0 rows"
                              " (bad :where / wrong layer / auth?): " url)
                         {:url url :opts opts :version version})))
       fetched)))

#?(:cljs
   (defn ^:async ensure-dataset
     "The JVM ensure-dataset with no cache file: a fetched dataset stays in
      memory, unless :keep? is false. Needs cg.run/init-cg-workers! first."
     [{:keys [url opts version refresh? keep?] :as spec}]
     (if (= false keep?)
       (await (fetch-dataset spec))
       (let [k (pr-str [url opts version])]
         (when (or refresh? (not (contains? @fetched-datasets k)))
           (swap! fetched-datasets assoc k (await (fetch-dataset spec))))
         (get @fetched-datasets k)))))

(defn filter-intersecting
  "Returns the rows whose :geometry intersects `geom`, which it prepares."
  [dataset geom]
  (let [prep #?(:clj (org.locationtech.jts.geom.prep.PreparedGeometryFactory/prepare ^Geometry geom)
                :cljs (geo/prepared-geometry-factory-prepare geom))]
    (filter-ds dataset
               (fn [row]
                 (when-let [g (:geometry row)]
                   #?(:clj (.intersects prep ^Geometry g)
                      :cljs (geo/prepared-geometry-intersects prep g)))))))

(defn row-map
  "Applies `f` to each row map. `f` returns a map of new column values, and
   each key becomes a column."
  ([dataset f] (row-map dataset f {}))
  (#_{:clj-kondo/ignore [:unused-binding]}
   [dataset f opts]
   #?(:clj (ds/row-map dataset f opts)
      :cljs (let [raw-ds (unwrap-dataset dataset)
                  row-maps (rows raw-ds :as-maps)
                  results (mapv f row-maps)
                  new-cols (when (seq results) (keys (first results)))]
              (clojure.core/reduce (fn [ds col-key]
                                     (let [col-str (str col-key)
                                           vals (mapv #(get % col-key) results)]
                                       (add-column ds col-str vals)))
                                   raw-ds
                                   new-cols)))))

(defn read-json
  "Parses a JSON string. The JVM uses charred with :key-fn (default keyword).
   CLJS uses JSON.parse and ignores :key-fn."
  #_{:clj-kondo/ignore [:unused-binding]}
  [json-str & {:keys [key-fn] :or {key-fn keyword}}]
  #?(:clj (charred/read-json json-str :key-fn key-fn)
     :cljs (.parse js/JSON json-str)))

(defn write-json
  "Converts data to a JSON string."
  [data]
  #?(:clj (charred/write-json-str data)
     :cljs (.stringify js/JSON data)))

(defn- value-name
  "A keyword as its name, any other value as it is. The JS runtime holds a
   keyword as a string already, and squint `name` cuts a string at a slash."
  [v]
  #?(:clj (if (keyword? v) (name v) v) :cljs v))

(defn- geojson-scalar-prop?
  "True for a JSON scalar, the only value a GeoJSON Feature property can hold."
  [v]
  (or (nil? v)
      (string? v)
      (number? v)
      (identical? v true)
      (identical? v false)))

(defn dataset->geojson-string
  "Converts a dataset to a GeoJSON FeatureCollection string. Keeps the
   geometry column and scalar property columns, with keyword values as names."
  ([dataset] (dataset->geojson-string dataset :geometry))
  ([dataset geom-col]
   (let [row-maps (rows dataset :as-maps)
         features (mapv (fn [row]
                          (let [geom (get row geom-col (get row (name geom-col)))
                                props (dissoc row geom-col (name geom-col))]
                            {"type" "Feature"
                             "geometry" (when geom
                                          #?(:clj (charred/read-json
                                                   (cio/write-geojson geom))
                                             :cljs (.parse js/JSON
                                                           (cio/write-geojson geom))))
                             ;; keep, because `filter` here is cg.dataset/filter.
                             ;; A keyword value goes as its name, because a
                             ;; map layer matches it with the :colors of its
                             ;; spec, and JSON has no keyword.
                             "properties" (into {} (keep (fn [[k v]]
                                                           (let [v (value-name v)]
                                                             (when (geojson-scalar-prop? v)
                                                               [(str k) v])))
                                                         props))}))
                        row-maps)
         fc {"type" "FeatureCollection"
             "features" features}]
     #?(:clj (charred/write-json-str fc)
        :cljs (.stringify js/JSON fc)))))

(defn column
  "Returns a column of the dataset as a vector. `col-name` is a keyword or a
   string."
  [dataset col-name]
  #?(:clj (vec (get dataset col-name))
     :cljs (let [raw-ds (unwrap-dataset dataset)
                 col-str (str col-name)
                 arr (.array raw-ds col-str)]
             (vec arr))))

(defn select-columns
  "Returns the dataset with only the columns `col-names`."
  [dataset col-names]
  #?(:clj (ds/select-columns dataset col-names)
     :cljs (let [raw-ds (unwrap-dataset dataset)
                 col-strs (mapv str col-names)]
             (.select raw-ds col-strs))))

(defn select-rows
  "Returns the rows at `indices` (0-based)."
  [dataset indices]
  #?(:clj (ds/select-rows dataset indices)
     :cljs
     (.reify (unwrap-dataset dataset) (vec indices))))

(defn add-column
  "Adds column `col-name` with the vector `values`, one value per row."
  [dataset col-name values]
  #?(:clj (assoc dataset col-name values)
     :cljs (let [raw-ds (unwrap-dataset dataset)
                 col-str (str col-name)
                 new-col-table (.table (ensure-arquero!)
                                       {col-str (to-array values)})]
             (.assign raw-ds new-col-table))))

(defn update-column
  "Applies `f` to the vector of column `col-name` and stores the vector that
   `f` returns."
  [dataset col-name f]
  #?(:clj (ds/update-column dataset col-name f)
     :cljs (let [col-str (str col-name)
                 current-values (column dataset col-name)
                 new-values (f current-values)
                 others (.filter (.columnNames dataset) (fn [c] (not= c col-str)))
                 new-col-table (.table (ensure-arquero!)
                                       {col-str (to-array new-values)})]
             ;; arquero gives a table with no columns 0 rows, and assign
             ;; refuses a column of another length.
             (if (zero? (.-length others))
               new-col-table
               (.assign (.select dataset others) new-col-table)))))

(defn add-or-update-column
  "Adds the column, or replaces it when it exists."
  [dataset col-name values]
  #?(:clj (ds/add-or-update-column dataset col-name values)
     :cljs (if (has-column? dataset col-name)
             (update-column dataset col-name (constantly values))
             (add-column dataset col-name values))))

;; A cg.geo op on a dataset runs over :geometry and puts the result in a new
;; column named after the op. :geometry stays unchanged. This is here, not in
;; cg.geo, because cg.dataset requires cg.geo and owns add-column.
#?(:clj
   (extend-protocol geo/GeoInput
     tech.v3.dataset.impl.dataset.Dataset
     (-geo-map [ds f col-name]
       (add-column ds col-name (mapv f (column ds :geometry))))))

(defn concat-datasets
  "Concatenates the rows of datasets that have the same columns."
  [datasets]
  #?(:clj (apply ds/concat-copying datasets)
     :cljs (let [as (to-array datasets)]
             (.reduce (.slice as 1)
                      (fn [acc ds] (.concat acc ds))
                      (aget as 0)))))

(defn rename-columns
  "Renames columns. `rename-map` is {old-name new-name}."
  [dataset rename-map]
  #?(:clj (tc/rename-columns dataset rename-map)
     :cljs (let [raw-ds (unwrap-dataset dataset)
                 js-map (into {}
                              (map (fn [[k v]]
                                     [(str k)
                                      (str v)]))
                              rename-map)]
             (.rename raw-ds js-map))))

(defn order-by
  "Sorts the dataset by a column. `direction` is :asc (default) or :desc."
  ([dataset col-name] (order-by dataset col-name :asc))
  ([dataset col-name direction]
   #?(:clj (tc/order-by dataset col-name (if (= direction :desc) :desc :asc))
      :cljs (let [raw-ds (unwrap-dataset dataset)
                  col-str (str col-name)]
              (if (= direction :desc)
                (.orderby raw-ds (.desc (ensure-arquero!) col-str))
                (.orderby raw-ds col-str))))))

(defn head
  "Returns the first `n` rows (default 5)."
  ([dataset] (head dataset 5))
  ([dataset n]
   #?(:clj (ds/head dataset n)
      :cljs (.slice (unwrap-dataset dataset) 0 n))))

(defn max-by
  "Returns the element with the largest `key-or-col` value, a row map for a
   dataset. `key-or-col` is a column for a dataset, else a keyword or fn.
   Returns nil for empty input; on a tie, the last element wins."
  [coll-or-dataset key-or-col]
  (let [elements (if (dataset? coll-or-dataset)
                   (rows coll-or-dataset :as-maps)
                   coll-or-dataset)]
    (when (seq elements)
      (apply max-key key-or-col elements))))

(defn min-by
  "As max-by, for the smallest value."
  [coll-or-dataset key-or-col]
  (let [elements (if (dataset? coll-or-dataset)
                   (rows coll-or-dataset :as-maps)
                   coll-or-dataset)]
    (when (seq elements)
      (apply min-key key-or-col elements))))

(defn reduce
  "Folds a collection, not a dataset (use `aggregate`). `reducer-kind` is
   :count, :distinct-count, :first, :last, :sum, :min, :max, :mean or :median;
   the last four give nil for an empty collection."
  [coll reducer-kind]
  (case reducer-kind
    :count          (count coll)
    :distinct-count (count (distinct coll))
    :first          (first coll)
    :last           (last coll)
    :sum            (clojure.core/reduce + 0 coll)
    :min            (when (seq coll) (clojure.core/reduce clojure.core/min coll))
    :max            (when (seq coll) (clojure.core/reduce clojure.core/max coll))
    :mean           (let [n (count coll)]
                      (when (pos? n)
                        (/ (clojure.core/reduce + 0 coll) (double n))))
    :median         (when (seq coll)
                      (let [sorted (vec (sort coll))
                            n      (count sorted)
                            mid    (quot n 2)]
                        (if (odd? n)
                          (nth sorted mid)
                          (/ (+ (nth sorted (dec mid)) (nth sorted mid)) 2.0))))
    (throw (ex-info (str "cg.dataset/reduce: unknown reducer-kind " reducer-kind)
                    {:reducer-kind reducer-kind
                     :supported #{:count :distinct-count :first :last
                                  :sum :min :max :mean :median}}))))

(defn group-by-column
  "Groups dataset rows by the values of a column. Returns a map of
   {group-value -> sub-dataset}."
  [dataset col-name]
  #?(:clj (let [col-vals (vec (get dataset col-name))
                indices-by-val (clojure.core/group-by (fn [i] (nth col-vals i)) (range (ds/row-count dataset)))]
            (into {} (map (fn [[v idxs]]
                            [v (ds/select-rows dataset idxs)])
                          indices-by-val)))
     :cljs (let [raw-ds (unwrap-dataset dataset)
                 col-str (str col-name)
                 row-maps (rows raw-ds :as-maps)
                 groups (clojure.core/group-by (fn [row] (get row col-str (get row col-name))) row-maps)]
             (into {} (map (fn [[k rows]]
                             [k (->dataset (clojure.core/reduce (fn [acc row]
                                                                  (reduce-kv (fn [m k v]
                                                                               (update m k (fnil conj []) v))
                                                                             acc row))
                                                                {} rows))]))
                   groups))))

(defn partition-by
  "Groups dataset rows by the values of a column, for `aggregate`. Returns
   {group-value -> sub-dataset}. `grouped?` identifies this shape at run time."
  [dataset col-name]
  (group-by-column dataset col-name))

(defn group-by
  "Same as `partition-by`, under a name that does not clash with
   clojure.core/partition-by."
  [dataset col-name]
  (partition-by dataset col-name))

(defn grouped?
  "True for the output shape of `partition-by`: a non-empty map whose values
   are all datasets. Returns false, not nil, for all other input."
  [x]
  (boolean
   (and (map? x)
        (seq x)
        (every? dataset? (vals x)))))

(defn into-map
  "Copies a groups container (group key to sub-dataset or result) into a
   plain map, for the `(into {})` form of table->. nil gives {}; throws on a
   non-map."
  [groups]
  (cond
    (nil? groups) {}
    (map? groups) (into {} groups)
    :else (throw (ex-info "into-map expects a groups container (a keyed map of sub-datasets/results)"
                          {:groups groups}))))

;; clojure.core.async.flow walks process state with clojure.walk/postwalk and
;; datafy. A tech.ml.dataset Column satisfies `coll?`, because it is Iterable,
;; but it has no IPersistentCollection.empty(), and postwalk fails on it.
;; FlowDataset is a deftype, not a defrecord: it does not satisfy `coll?`,
;; and postwalk skips it. Wrap a dataset before it goes into flow state.

#?(:clj
   (defn- truncate-geometry-wkt
     [s max-len]
     (if (and (string? s) (> (count s) max-len))
       (str (subs s 0 max-len) "...")
       s)))

#?(:clj
   (defn- dataset-preview
     "A printed preview of the first `max-rows` rows, with short geometry
      text."
     [dataset max-rows]
     (let [preview-ds (ds/select-rows dataset (range (min max-rows (ds/row-count dataset))))
           geom-cols (clojure.core/filter #(or (instance? org.locationtech.jts.geom.Geometry
                                                          (first (get preview-ds %)))
                                               (and (string? (first (get preview-ds %)))
                                                    (re-find #"^(POINT|POLYGON|LINE|MULTI)"
                                                             (str (first (get preview-ds %))))))
                                          (ds/column-names preview-ds))
           display-ds (clojure.core/reduce (fn [ds col]
                                             (ds/update-column ds col
                                                               (fn [geoms]
                                                                 (mapv #(truncate-geometry-wkt (str %) 40) geoms))))
                                           preview-ds
                                           geom-cols)]
       (with-out-str (println display-ds)))))

#?(:clj
   (deftype FlowDataset [dataset metadata-map]
     clojure.lang.IObj
     (meta [_] metadata-map)
     (withMeta [_ m] (FlowDataset. dataset m))

     protocols/Datafiable
     (datafy [_]
       (let [row-count (ds/row-count dataset)
             preview (when (pos? row-count)
                       (dataset-preview dataset 5))]
         {:type :dataset
          :row-count row-count
          :column-names (vec (ds/column-names dataset))
          :name (ds/dataset-name dataset)
          :preview preview
          :metadata metadata-map}))))

#?(:clj
   (do
     (defn unwrap-dataset
       "Returns the dataset inside a FlowDataset. Returns any other input
        unchanged."
       [wrapped]
       (if (instance? FlowDataset wrapped)
         (.-dataset ^FlowDataset wrapped)
         wrapped))

     (defn wrapped-dataset?
       "True when `x` is a FlowDataset wrapper."
       [x]
       (instance? FlowDataset x))))

(defn fd-meta
  "Returns the metadata of a FlowDataset. On CLJS, returns nil for any other
   value. On the JVM, also returns the metadata of a tech.ml.dataset."
  [wrapped]
  #?(:clj (meta wrapped)
     :cljs (when (wrapped-dataset? wrapped)
             (.-metadata wrapped))))

(defn wrap-dataset
  "Wraps a dataset in a FlowDataset with `metadata`. Returns a FlowDataset
   input unchanged."
  ([ds]
   (wrap-dataset ds nil))
  ([ds metadata]
   (if (wrapped-dataset? ds)
     ds
     (->FlowDataset ds (or metadata {})))))

;; The flow library iterates over a map that goes on a channel and sends each
;; MapEntry as a separate message. FlowBundle wraps the coordinate bundles
;; that go between the transform-crs steps.

#?(:clj
   (deftype FlowBundle [data]
     protocols/Datafiable
     (datafy [_]
       {:type :flow-bundle
        :keys (keys data)})))

#?(:clj
   (defn wrap-bundle
     "Wraps a map in a FlowBundle, because flow iterates over the entries of a
      bare map."
     [m]
     (->FlowBundle m)))

#?(:clj
   (defn unwrap-bundle
     "Returns the map inside a FlowBundle."
     [^FlowBundle fb]
     (.-data fb)))

;; The context of each dataset in a flow, by context id:
;; {:schema [col ...] :geom-column :geometry :row-count n
;;  :spatial-index <HPRtree or STRtree, optional> :crs "EPSG:4326"
;;  :source-dataset <dataset>}
;; spatial-index-store is the same atom: on the JVM, add-spatial-index also
;; puts each HPRtree here, by index UUID.

(def dataset-context-store (atom {}))

(def spatial-index-store dataset-context-store)

(defn reset-dataset-context-store!
  "Removes all contexts from the dataset context store."
  []
  (reset! dataset-context-store {}))

(defn get-dataset-context
  "Returns the registered dataset context for `ctx-id`."
  [ctx-id]
  (get @dataset-context-store ctx-id))

(defn unregister-dataset-context!
  "Removes a dataset context from the store."
  [ctx-id]
  (swap! dataset-context-store dissoc ctx-id))

#?(:cljs
   (do
     (defn- get-envelope-internal
       [geom]
       (js/globalThis.wasmts.geom.getEnvelopeInternal geom))

     (defn- create-strtree
       []
       (js/globalThis.wasmts.index.strtree.STRtree.create))

     (defn- strtree-insert!
       [tree envelope item]
       (js/globalThis.wasmts.index.strtree.STRtree.insert tree envelope item))

     (defn- strtree-query
       [tree envelope]
       (js/globalThis.wasmts.index.strtree.STRtree.query tree envelope))))

(defn register-dataset-context!
  "Registers the context of dataset `ds` and returns the context id.
   opts: :geom-column (default :geometry) and :build-index? (default true)."
  ([ds] (register-dataset-context! ds {}))
  ([ds opts]
   (let [ctx-id #?(:clj (random-uuid)
                   :cljs (str (random-uuid)))
         geom-col (get opts :geom-column :geometry)
         build-index? (get opts :build-index? true)
         schema #?(:clj (vec (ds/column-names ds))
                   :cljs (column-names ds))
         row-cnt #?(:clj (ds/row-count ds)
                    :cljs (row-count ds))
         crs (:crs (fd-meta ds))

         spatial-index (when (and build-index?
                                  #?(:clj (ds/has-column? ds geom-col)
                                     :cljs (has-column? ds geom-col)))
                         #?(:clj
                            (let [idx (org.locationtech.jts.index.hprtree.HPRtree.)]
                              (doseq [row (ds/rows ds :as-maps)]
                                (when-let [geom (get row geom-col)]
                                  (let [^Geometry jts-geom (feature/to-jts geom)]
                                    (.insert idx
                                             (.getEnvelopeInternal jts-geom)
                                             {:geometry geom
                                              :row row}))))
                              idx)
                            :cljs
                            ;; No row map in the payload: a query reads the
                            ;; row from the table with :row-idx.
                            (let [idx (create-strtree)
                                  geoms (column ds geom-col)]
                              (dotimes [i (count geoms)]
                                (when-let [geom (nth geoms i)]
                                  (strtree-insert! idx (get-envelope-internal geom)
                                                   {:geometry geom
                                                    :row-idx i})))
                              idx)))

         context {:schema schema
                  :geom-column geom-col
                  :row-count row-cnt
                  :spatial-index spatial-index
                  :crs crs
                  :source-dataset ds}]

     (swap! dataset-context-store assoc ctx-id context)
     ctx-id)))

(defn query-context-index
  "Queries the spatial index of a registered context with an Envelope.
   Returns {:geometry g :row row-map} (JVM) or {:geometry g :row-idx n} (CLJS)
   for each feature whose envelope meets it."
  [ctx-id envelope]
  (when-let [ctx (get-dataset-context ctx-id)]
    (when-let [idx (:spatial-index ctx)]
      #?(:clj (.query ^HPRtree idx envelope)
         :cljs (vec (strtree-query idx envelope))))))

(defn- check-geometry-column!
  "Throws when a transform-crs batch has rows and no `geom-col` column, for
   example a string \"geometry\" column in place of :geometry."
  [batch geom-col]
  (when (and (not (has-column? batch geom-col)) (pos? (row-count batch)))
    (let [cols (vec (column-names batch))]
      (throw (ex-info (str "transform-crs: the dataset has no geometry column ("
                           (pr-str geom-col) "); its columns are " (pr-str cols))
                      {:columns cols})))))

#?(:clj
   (do
     (defn dataset->batches
       "Splits a dataset into a vector of batches of at most :max-batch-size
        rows (default 64000)."
       ([ds] (dataset->batches ds {}))
       ([ds opts]
        (let [max-batch-size (get opts :max-batch-size 64000)
              n-rows (ds/row-count ds)]
          (if (<= n-rows max-batch-size)
            [ds]
            (vec (hamf/pgroups n-rows
                               (fn [^long sidx ^long eidx]
                                 (ds/select-rows ds (range sidx eidx)))
                               {:max-batch-size max-batch-size
                                :min-n 1}))))))

     (defn batches->dataset
       "Concatenates batches into one dataset."
       [batches]
       (if (= 1 (count batches))
         (first batches)
         (apply ds/concat-copying batches)))

     (defn get-spatial-indices
       "Returns the :spatial entries of the dataset metadata."
       [ds]
       (:spatial (meta ds)))

     (defn get-spatial-index
       "Returns the spatial index entry of the dataset, or of `geom-column`."
       ([ds]
        (let [s (get-spatial-indices ds)]
          (case (count s)
            0 nil
            1 (first s)
            (Exception. "More than one index."))))
       ([ds geom-column]
        (let [s (get-spatial-indices ds)
              i (if (nil? s)
                  nil
                  (first (clojure.core/filter #(= (:geometry-column %) geom-column) s)))]
          (when (and (not (nil? (:spatial-index-uuid i)))
                     (= HPRtree
                        (type (get @spatial-index-store
                                   (:spatial-index-uuid i)))))
            i))))

     (defn has-spatial-index?
       "True when the dataset has a spatial index."
       ([ds]
        (not (nil? (get-spatial-index ds))))
       ([ds geom-column]
        (not (nil? (get-spatial-index ds geom-column)))))

     ;; The select-* predicates need a dual-arity contract (see prepared-pred).
     ;; The minted cg.geo prepared-geometry-* ops are plain 2-arity ops.
     (defn- as-prepared
       "Returns a PreparedGeometry for a featurelike, a Geometry or a PreparedGeometry."
       [x]
       (if (instance? PreparedGeometry x)
         x
         (geo/prepared-geometry-factory-prepare (feature/to-jts x))))

     (defn- prepared-pred
       "Wraps a cg.geo/prepared-geometry-* op. The 1-arity returns a fn of the
        other geometry; the 2-arity prepares the first arg unless it is prepared."
       [pred-fn]
       (fn
         ([x] (let [pg (as-prepared x)] (fn [other] (pred-fn pg other))))
         ([a b] (pred-fn (as-prepared a) b))))

     (def ^:private ds-contains?          (prepared-pred geo/prepared-geometry-contains))
     (def ^:private ds-intersects?        (prepared-pred geo/prepared-geometry-intersects))
     (def ^:private ds-within?            (prepared-pred geo/prepared-geometry-within))

     (defn add-spatial-index
       "Builds an HPRtree index on `geom-column` (default :geometry). Returns the
        dataset with `indexed-id-column` (default :indexed-id) and :spatial
        metadata that refers to the index."
       ([ds]
        (add-spatial-index ds :geometry))
       ([ds geom-column]
        (add-spatial-index ds geom-column :indexed-id))
       ([ds geom-column indexed-id-column]
        (let [idx-id (if (has-spatial-index? ds geom-column)
                       (:spatial-index-uuid (:spatial (meta ds)))
                       (random-uuid))
              n-rows (ds/row-count ds)
              _ (stage! index-label "start" 0 n-rows)
              generate-spatial-index-fn
              (fn [ds geom-column indexed-id-column idx-id]
                (let [index (HPRtree.)
                      rows (ds/rows ds :as-maps)
                      ;; After a reproject, apply-coords-step flags the
                      ;; batch for a new index.
                      indexed-id-vec (vec (repeatedly (count rows) random-uuid))
                      _ (doseq [[row id] (map vector rows indexed-id-vec)]
                          (let [featurelike (row geom-column)]
                            (when (not (nil? featurelike))
                              (let [^Geometry jts-geom (feature/to-jts featurelike)]
                                (.insert index
                                         (.getEnvelopeInternal jts-geom)
                                         {indexed-id-column id
                                          :prepared-geometry (as-prepared featurelike)})))))
                      ds-with-id (ds/add-or-update-column ds indexed-id-column indexed-id-vec)]
                  {:dataset ds-with-id
                   :spatial-index-uuid idx-id
                   :geometry-column geom-column
                   :indexed-id-column indexed-id-column
                   :index index}))
              spatial-index (generate-spatial-index-fn ds geom-column
                                                       indexed-id-column
                                                       idx-id)]
          (swap! spatial-index-store merge {idx-id (:index spatial-index)})
          (stage! index-label "done" n-rows n-rows)
          (when (nil? (:spatial (meta (:dataset spatial-index))))
            (vary-meta (:dataset spatial-index) assoc :spatial []))
          (vary-meta (:dataset spatial-index) assoc
                     :spatial (into []
                                    (conj (:spatial (meta (:dataset spatial-index)))
                                          (dissoc spatial-index :dataset :index)))))))

     (defn query-spatial-index
       "Returns the index payloads whose envelopes meet `envelope`."
       [dataset envelope geom-column]
       (let [^HPRtree idx (get @spatial-index-store
                               (:spatial-index-uuid (get-spatial-index dataset geom-column)))]
         (.query idx envelope)))

     (defn ds-featurelike-columns
       "Returns the names of the SimpleFeaturelike columns of a dataset. Reads
        the :object columns, and tests the first row only."
       [dataset]
       (let [featurelike-dtype? #{:object}
             object-cols (into #{} (map :name
                                        (clojure.core/filter
                                         #(featurelike-dtype? (:datatype %))
                                         (map meta (ds/columns dataset)))))
             subset (dscf/column-filter dataset #(object-cols (:name (meta %))))
             ;; TODO: a nil in the first row hides the column.
             first-row (first (ds/rows subset :as-maps))
             ;; satisfies? is slow.
             filtered-row (clojure.core/filter #(satisfies? feature/SimpleFeaturelike (val %)) first-row)]
         (into [] (map key filtered-row))))

     (defn load-geojson
       "Loads a GeoJSON file into a dataset. JVM only. opts: :generate-index? and
        :warn-on-error? (both default true). On failure, logs and returns nil,
        or throws when :warn-on-error? is false."
       ([path]
        (load-geojson path {}))
       ([path opts]
        (let [generate-index? (get opts :generate-index? true)
              warn-on-error? (get opts :warn-on-error? true)]
          (try
            (let [content (slurp path)]
              (log/info "Loaded:" path)
              (geojson->dataset content generate-index?))
            (catch Exception e
              (if warn-on-error?
                (do
                  (log/warn "Failed to load:" path "-" (.getMessage e))
                  nil)
                (throw (ex-info (str "Failed to load GeoJSON: " path)
                                {:path path
                                 :cause (.getMessage e)}
                                e))))))))

     ;; A probe of a dataset with no spatial index scans all rows. In a hot
     ;; loop that looks like a hang. Warn one time per dataset instance.
     (defonce ^:private no-index-warned (atom #{}))

     (defn- warn-once-no-index!
       [dataset geom-column]
       (let [k (System/identityHashCode dataset)]
         (when-not (contains? @no-index-warned k)
           (swap! no-index-warned conj k)
           (log/warn (str "No spatial index on dataset ("
                          (ds/row-count dataset) " rows, geom column "
                          geom-column ") — spatial probes will linear-scan"
                          " every row. Call cg.dataset/add-spatial-index"
                          " on it before hot query loops.")))))

     (defn- de-9im-builder
       "Selects the rows that satisfy `de-9im-operation` with `featurelike`,
        through the spatial index when the dataset has one."
       ([dataset featurelike de-9im-operation]
        (let [featurelike-columns (ds-featurelike-columns dataset)]
          (if (= 1 (count featurelike-columns))
            (de-9im-builder dataset featurelike de-9im-operation (first featurelike-columns))
            (throw (Exception. "More than one featurelike column. Please specify directly.")))))
       ([dataset featurelike de-9im-operation geom-column]
        (if (get-spatial-index dataset geom-column)
          (let [^Geometry g (feature/to-jts featurelike)
                query-results (query-spatial-index dataset (.getEnvelopeInternal g) geom-column)
                de-9im-results (clojure.core/filter #(de-9im-operation (:prepared-geometry %) g) query-results)
                uuids (into #{} (map (:indexed-id-column (get-spatial-index dataset)) de-9im-results))]
            (ds/filter-column dataset (:indexed-id-column (get-spatial-index dataset)) uuids))
          (let [g (feature/to-jts featurelike)
                bool-col (keyword (.toString ^java.util.UUID (random-uuid)))
                de-9im-fn (fn [row] {bool-col ((de-9im-operation g) (row geom-column))})]
            (warn-once-no-index! dataset geom-column)
            (-> (tc/select-rows (ds/row-map dataset de-9im-fn) (comp true? bool-col))
                (ds/drop-columns [bool-col]))))))

     (defn select-contains
       "Selects the rows whose geometry contains `featurelike`."
       ([dataset featurelike]
        (de-9im-builder dataset featurelike ds-contains?))
       ([dataset featurelike geom-column]
        (de-9im-builder dataset featurelike ds-contains? geom-column)))

     (defn select-intersects
       "Selects the rows whose geometry intersects `featurelike`."
       ([dataset featurelike]
        (de-9im-builder dataset featurelike ds-intersects?))
       ([dataset featurelike geom-column]
        (de-9im-builder dataset featurelike ds-intersects? geom-column)))

     (defn index-candidates-fn
       "Returns a fn of a featurelike that gives the index payloads whose
        envelopes meet it; a payload holds :prepared-geometry and the
        indexed-id column. Returns nil when the dataset has no index."
       ([dataset] (index-candidates-fn dataset :geometry))
       ([dataset geom-column]
        (if-let [idx (get-spatial-index dataset geom-column)]
          (let [^HPRtree tree (get @spatial-index-store (:spatial-index-uuid idx))]
            (fn [featurelike]
              (.query tree (.getEnvelopeInternal ^Geometry (feature/to-jts featurelike)))))
          (do (warn-once-no-index! dataset geom-column) nil))))

     (defn intersects-any?
       "True when a geometry in `dataset` intersects `featurelike`. Stops at
        the first hit and builds no result dataset, unlike `select-intersects`."
       ([dataset featurelike]
        (let [featurelike-columns (ds-featurelike-columns dataset)]
          (if (= 1 (count featurelike-columns))
            (intersects-any? dataset featurelike (first featurelike-columns))
            (throw (Exception. "More than one featurelike column. Please specify directly.")))))
       ([dataset featurelike geom-column]
        (let [^Geometry g (feature/to-jts featurelike)]
          (if-let [candidates-of (index-candidates-fn dataset geom-column)]
            (boolean (some #(ds-intersects? (:prepared-geometry %) g) (candidates-of g)))
            (let [hit? (ds-intersects? g)]
              (boolean (some (fn [row]
                               (let [fg (get row geom-column)]
                                 (and (some? fg) (hit? fg))))
                             (ds/rows dataset :as-maps))))))))

     (defn select-within
       "Selects the rows whose geometry is within `featurelike`."
       ([dataset featurelike]
        (de-9im-builder dataset featurelike ds-within?))
       ([dataset featurelike geom-column]
        (de-9im-builder dataset featurelike ds-within? geom-column)))

     (defn map-geometry
       "Applies `f` to each geometry of a column and returns the dataset. opts:
        :geometry-column (default :geometry) and :preserve-index (default
        false, which rebuilds an existing spatial index)."
       ([dataset f]
        (map-geometry dataset f {}))

       ([dataset f opts]
        (let [geom-col (get opts :geometry-column :geometry)
              preserve-index? (get opts :preserve-index false)

              _ (when-not (ds/has-column? dataset geom-col)
                  (throw (ex-info "Geometry column not found"
                                  {:column geom-col
                                   :available-columns (ds/column-names dataset)})))

              transformed (ds/update-column dataset geom-col
                                            (fn [geom-series]
                                              (into [] (map f) geom-series)))

              result (if (and (has-spatial-index? dataset geom-col)
                              (not preserve-index?))
                       (add-spatial-index transformed geom-col)
                       transformed)]

          result)))

     ;; Three flow steps, because the batches can then pipeline.
     ;; transform-coords-step (:workload :compute) runs on a clj-native pool
     ;; whose :proj handler owns one PROJ Context per worker.

     (defn extract-coords-step
       "Flow step: extracts the :geometry coordinates of a wrapped batch into
        {:all-coords [[x y z m]...] :boundaries [n1 n2...] :geoms [...] :batch batch}"
       []
       (fn step-fn
         ([] {:params {}
              :ins {:in "Dataset batch"}
              :outs {:out "Coord bundle"}})
         ([_args]
          (log/info "extract-coords-step: initialized")
          {})
         ([state _lifecycle] state)
         ([state _in-name wrapped-batch]
          (log/info "extract-coords-step: received batch, type=" (type wrapped-batch) "nil?=" (nil? wrapped-batch))
          (let [batch (unwrap-dataset wrapped-batch)
                _ (check-geometry-column! batch :geometry)
                geom-col (batch :geometry)
                geoms (vec geom-col)
                _ (log/info "extract-coords-step: heap-JTS path, extracting coords from"
                            (count geoms) "geometries")
                result (clojure.core/reduce
                        (fn [acc geom]
                          (if (nil? geom)
                            {:all-coords (:all-coords acc)
                             :boundaries (conj (:boundaries acc) 0)
                             :geoms (conj (:geoms acc) nil)}
                            (let [^Geometry g (feature/geometry geom)
                                  {:keys [coords count]} (geo/extract-geometry-coords g)]
                              {:all-coords (into (:all-coords acc) coords)
                               :boundaries (conj (:boundaries acc) count)
                               :geoms (conj (:geoms acc) g)})))
                        {:all-coords [] :boundaries [] :geoms []}
                        geoms)]
            (log/info "extract-coords-step: emitting bundle with"
                      (count (:all-coords result)) "coords")
            [state {:out [(wrap-bundle (assoc result
                                              :batch batch
                                              :table-rows (:cg.dataset/table-rows (fd-meta wrapped-batch))))]}]))))

     (defn transform-coords-step
       "Flow step: transforms :all-coords with PROJ on a compute-pool worker.
        Returns {:transformed-coords :boundaries :geoms :batch}."
       [source-crs target-crs]
       (let [stage (reproject-stage target-crs)]
         (fn step-fn
           ([] {:workload :compute
                ;; 60 s is too short: a PROJ grid or network stall on one
                ;; worker can hold a batch that normally takes seconds.
                :compute-timeout-ms 300000
                :params {:source-crs source-crs :target-crs target-crs}
                :ins {:in "Coord bundle"}
                :outs {:out "Transformed bundle"}})
           ;; No init: the :proj handler owns the per-worker Context.
           ([_args] {})
           ([state lifecycle]
            (when (= lifecycle :clojure.core.async.flow/stop)
              ((:end! stage)))
            state)
           ([state _in-name wrapped-bundle]
            (let [bundle (unwrap-bundle wrapped-bundle)
                  _ ((:begin! stage) (:table-rows bundle))
                  out (let [{:keys [all-coords boundaries geoms batch]} bundle
                            n (count all-coords)]
                        (if (zero? n)
                          {:transformed-coords []
                           :boundaries boundaries
                           :geoms geoms
                           :batch batch}
                          (let [coord-array (proj/coord-array n)
                                _ (proj/set-coords! coord-array all-coords)
                                _ (proj/transform-batch source-crs target-crs coord-array)
                                transformed-coords (geo/read-coord-array coord-array n)]
                            {:transformed-coords transformed-coords
                             :boundaries boundaries
                             :geoms geoms
                             :batch batch})))]
              ((:add! stage) (row-count (:batch bundle)))
              [state {:out [(wrap-bundle out)]}])))))

     (defn apply-coords-step
       "Flow step: applies :transformed-coords to copies of the geometries.
        Returns a FlowDataset flagged :cg.dataset/needs-reindex."
       []
       (fn step-fn
         ([] {:params {}
              :ins {:in "Transformed bundle"}
              :outs {:out "Dataset batch"}})
         ([_args]
          (log/info "apply-coords-step: initialized")
          {})
         ([state _lifecycle] state)
         ([state _in-name wrapped-bundle]
          (let [{:keys [transformed-coords boundaries geoms batch]} (unwrap-bundle wrapped-bundle)
                new-geoms (loop [geom-idx (long 0)
                                 coord-offset (long 0)
                                 result-geoms []]
                            (if (>= geom-idx (count boundaries))
                              result-geoms
                              (let [n (long (nth boundaries geom-idx))
                                    geom-coords (subvec transformed-coords coord-offset (+ coord-offset n))
                                    ^Geometry orig-g (nth geoms geom-idx)
                                    ^Geometry g (when orig-g (.copy orig-g))]
                                (when g
                                  (geo/apply-transformed-coords! g geom-coords))
                                (recur (inc geom-idx)
                                       (+ coord-offset n)
                                       (conj result-geoms g)))))
                updated-batch (-> (ds/add-or-update-column batch :geometry new-geoms)
                                  (vary-meta dissoc :spatial)
                                  (vary-meta assoc :cg.dataset/needs-reindex true))]
            (log/info "apply-coords-step: heap-JTS path, emitting batch with"
                      (count new-geoms) "reprojected geometries")
            [state {:out [(wrap-dataset updated-batch)]}]))))

     (feature/register-operation! "cg.dataset/extract-coords-step"
                                  {:type :step-factory
                                   :params []})

     (feature/register-operation! "cg.dataset/transform-coords-step"
                                  {:type :step-factory
                                   :params [{:name :source-crs :required true :type :string}
                                            {:name :target-crs :required true :type :string}]})

     (feature/register-operation! "cg.dataset/apply-coords-step"
                                  {:type :step-factory
                                   :params []}))

   :cljs
   (do
     (defn- index-context-id
       "The context id of the index of `dataset` on `geom-column`, or nil."
       [dataset geom-column]
       (let [m (fd-meta dataset)]
         (when (= geom-column (get-in m [:spatial :geometry-column]))
           (get-in m [:spatial :context-id]))))

     (defn select-intersects
       "Selects the rows whose geometry intersects `query-geom`.
        opts: :geom-column (default :geometry)."
       ([dataset query-geom]
        (select-intersects dataset query-geom {}))
       ([dataset query-geom opts]
        (let [geom-col (get opts :geom-column :geometry)
              query-env (get-envelope-internal query-geom)
              matching-indices (if-let [ctx-id (index-context-id dataset geom-col)]
                                 (->> (query-context-index ctx-id query-env)
                                      (keep (fn [hit]
                                              (when (geo/intersects? (:geometry hit) query-geom)
                                                (:row-idx hit))))
                                      sort)
                                 (keep-indexed
                                  (fn [idx geom]
                                    (when (and geom
                                               (.intersects (get-envelope-internal geom) query-env)
                                               (geo/intersects? geom query-geom))
                                      idx))
                                  (column dataset geom-col)))]
          (if (empty? matching-indices)
            (->dataset {})
            (select-rows dataset matching-indices)))))

     (defn select-contains
       "Selects the rows whose geometry contains `query-geom`."
       ([dataset query-geom]
        (select-contains dataset query-geom {}))
       ([dataset query-geom opts]
        (let [geom-col (get opts :geom-column :geometry)
              geoms (column dataset geom-col)
              matching-indices (keep-indexed
                                (fn [idx geom]
                                  (when (and geom (geo/contains? geom query-geom))
                                    idx))
                                geoms)]
          (if (empty? matching-indices)
            (->dataset {})
            (select-rows dataset (vec matching-indices))))))

     (defn select-within
       "Selects the rows whose geometry is within `query-geom`."
       ([dataset query-geom]
        (select-within dataset query-geom {}))
       ([dataset query-geom opts]
        (let [geom-col (get opts :geom-column :geometry)
              geoms (column dataset geom-col)
              matching-indices (keep-indexed
                                (fn [idx geom]
                                  (when (and geom (geo/within? geom query-geom))
                                    idx))
                                geoms)]
          (if (empty? matching-indices)
            (->dataset {})
            (select-rows dataset (vec matching-indices))))))

     (defn map-geometry
       "Applies `f` to each geometry of a dataset.
        opts: :geom-column (default :geometry)."
       ([dataset f] (map-geometry dataset f {}))
       ([dataset f opts]
        (let [geom-col (get opts :geom-column :geometry)]
          (update-column dataset geom-col (fn [geoms] (mapv f geoms))))))

     (defn add-spatial-index
       "Adds a WasmTS STRtree index on `geom-column` (default :geometry) and an
        `indexed-id-column` (default :indexed-id). Returns a FlowDataset whose
        metadata refers to the index."
       ([ds]
        (add-spatial-index ds :geometry))
       ([ds geom-column]
        (add-spatial-index ds geom-column :indexed-id))
       ([ds geom-column indexed-id-column]
        (let [raw-ds (unwrap-dataset ds)
              n-rows (row-count raw-ds)
              _ (stage! index-label "start" 0 n-rows)
              busy (trace/busy-start! "main" "main"
                                      (str index-label " of " n-rows " rows") nil)
              ctx-id (register-dataset-context! raw-ds {:geom-column geom-column
                                                        :build-index? true})
              _ (trace/busy-end! busy)
              _ (stage! index-label "done" n-rows n-rows)
              ids (mapv (fn [_] (str (random-uuid))) (range n-rows))
              ds-with-ids (add-column raw-ds indexed-id-column ids)
              spatial-meta {:spatial {:context-id ctx-id
                                      :geometry-column geom-column
                                      :indexed-id-column indexed-id-column}}]
          (->FlowDataset ds-with-ids spatial-meta))))

     (defn has-spatial-index?
       "True when the dataset has a spatial index on `geom-column` (default
        :geometry)."
       ([ds]
        (has-spatial-index? ds :geometry))
       ([ds geom-column]
        (some? (index-context-id ds geom-column))))

     ;; A prepared geometry keeps its segment index, so only the candidates
     ;; used last stay prepared. A polygon under most parcels stays recent.
     (def ^:private prepared-cache-size 2048)

     (defn- lru-lookup!
       "The value of `k` in the js/Map `m`, from (make) on a miss. `m` keeps
        the `limit` keys used last."
       [m k limit make]
       (if-let [v (.get m k)]
         (do (.delete m k)
             (.set m k v)
             v)
         (let [v (make)]
           (.set m k v)
           (when (> (.-size m) limit)
             (.delete m (.-value (.next (.keys m)))))
           v)))

     (defn index-candidates-fn
       "Returns a fn of a featurelike that gives the index payloads
        {:prepared-geometry pg <indexed-id column> id} whose envelopes meet it.
        Returns nil when the dataset has no index on `geom-column`."
       ([dataset] (index-candidates-fn dataset :geometry))
       ([dataset geom-column]
        (when-let [ctx-id (index-context-id dataset geom-column)]
          (or (:candidates-fn (get-dataset-context ctx-id))
              (let [id-column (get-in (fd-meta dataset) [:spatial :indexed-id-column] :indexed-id)
                    ids (column dataset id-column)
                    payloads (js/Map.)
                    payload (fn [hit]
                              (let [i (:row-idx hit)]
                                (lru-lookup! payloads i prepared-cache-size
                                             (fn prepare-payload []
                                               {:prepared-geometry (geo/prepared-geometry-factory-prepare
                                                                    (feature/to-jts (:geometry hit)))
                                                id-column (nth ids i)}))))
                    candidates-of (fn [featurelike]
                                    (mapv payload
                                          (query-context-index
                                           ctx-id (get-envelope-internal (feature/to-jts featurelike)))))]
                (swap! dataset-context-store assoc-in [ctx-id :candidates-fn] candidates-of)
                candidates-of)))))

     (defn intersects-any?
       "True when a geometry in `dataset` intersects `featurelike`, through the
        spatial index when one exists. The 2-arity reads the indexed geometry
        column, or :geometry."
       ([dataset featurelike]
        (intersects-any? dataset featurelike
                         (get-in (fd-meta dataset) [:spatial :geometry-column] :geometry)))
       ([dataset featurelike geom-column]
        (let [g (feature/to-jts featurelike)]
          (if-let [candidates-of (index-candidates-fn dataset geom-column)]
            (boolean (some (fn [c] (geo/intersects? (:prepared-geometry c) g))
                           (candidates-of g)))
            (boolean (some (fn [fg] (and (some? fg) (geo/intersects? fg g)))
                           (column dataset geom-column)))))))

     (defn query-spatial-index
       "Returns the index payloads whose envelopes meet the WasmTS `envelope`.
        Throws when the dataset has no spatial index."
       ([ds envelope]
        (query-spatial-index ds envelope :geometry))
       ([ds envelope _geom-column]
        (let [ctx-id (get-in (fd-meta ds) [:spatial :context-id])]
          (if ctx-id
            (query-context-index ctx-id envelope)
            (throw (ex-info "Dataset has no spatial index"
                            {:columns (column-names ds)}))))))

     (defn dataset->batches
       "Splits a dataset into a seq of datasets of at most :batch-size rows
        (default 1000)."
       ([ds] (dataset->batches ds {}))
       ([ds opts]
        (let [batch-size (get opts :batch-size 1000)
              total-rows (row-count ds)
              num-batches (Math/ceil (/ total-rows batch-size))]
          (for [i (range num-batches)]
            (let [start (* i batch-size)
                  end (min (+ start batch-size) total-rows)]
              (select-rows ds (range start end)))))))

     (defn batches->dataset
       "Concatenates batches into one dataset."
       [batches]
       (if (empty? batches)
         (->dataset {})
         (concat-datasets batches)))

     ;; A 2D batch goes between WasmTS and PROJ as flat arrays on this thread,
     ;; because WasmTS copies a WKB into WASM one byte for each call. A batch
     ;; with a Z keeps the worker path, which writes it as 2D WKB.
     ;; cg.run/init-cg-workers! must run first.

     ;; projCreateCrsToCrs pins a PJ to one worker, and each later
     ;; proj_trans_array call on that PJ goes to that worker. One PJ per CRS
     ;; pair is enough.

     (defonce ^:private *transformers (atom {}))
     (defonce ^:private *transformer-builds (atom {}))

     (defn- ^:async build-transformer!
       "Builds one PJ for (source-crs, target-crs), normalized for
        visualization: lon/lat order, also for a CRS such as EPSG:4326."
       [source-crs target-crs]
       ;; Without the awaits the worker call gets a Promise, and postMessage
       ;; rejects it with DataCloneError.
       (let [ctx    (await (proj-wasm/context-create))
             raw-tx (await (proj-wasm/proj-create-crs-to-crs
                            #js {:context ctx
                                 :source_crs source-crs
                                 :target_crs target-crs}))]
         (await (proj-wasm/proj-normalize-for-visualization
                 #js {:context ctx :obj raw-tx}))))

     (defn- ^:async get-transformer
       "The cached PJ for (source-crs, target-crs). Concurrent first calls
        share one build."
       [source-crs target-crs]
       (let [k [source-crs target-crs]]
         (or (get @*transformers k)
             (let [in-flight (get @*transformer-builds k)]
               (if in-flight
                 (await in-flight)
                 (let [build-promise (build-transformer! source-crs target-crs)]
                   (swap! *transformer-builds assoc k build-promise)
                   (try
                     (let [pj (await build-promise)]
                       (swap! *transformers assoc k pj)
                       pj)
                     (finally
                       (swap! *transformer-builds dissoc k)))))))))

     (defn- geoms->wkb-array
       [geoms]
       (let [n (count geoms)
             out (js/Array. n)]
         (loop [i 0 gs geoms]
           (if (>= i n)
             out
             (let [wkb (cio/write-wkb (first gs))]
               (aset out i wkb)
               (recur (inc i) (rest gs)))))))

     (defn- wkb-array->geoms
       [wkb-array]
       (let [n (.-length wkb-array)]
         (loop [i 0 acc (transient [])]
           (if (>= i n)
             (persistent! acc)
             (recur (inc i) (conj! acc (cio/read-wkb (aget wkb-array i))))))))

     (defn- ^:async each-index
       "Call `(f i)` for each i below `n` on this thread, with a turn of the
        event loop each 100 ms."
       [n f]
       (loop [i 0 turn-start (js/Date.now)]
         (when (< i n)
           (f i)
           (if (> (- (js/Date.now) turn-start) 100)
             (do (await (trace/next-turn))
                 (recur (inc i) (js/Date.now)))
             (recur (inc i) turn-start)))))

     (defn- ^:async proj-coords
       "Transform `coords` (stride 4: x y z m) in the worker of the PJ, as one
        worker-busy pair. Gives a transformed copy."
       [coords source-crs target-crs pid]
       (let [n-coords (quot (.-length coords) 4)
             pj (await (get-transformer source-crs target-crs))
             ;; proj_trans_array changes a WASM-heap coord array in place.
             ;; With a raw Float64Array, proj-wasm transforms a copy on the
             ;; worker and the result is lost.
             proj-buf (await (proj-wasm/coord-array n-coords))
             buf-f64 (.-buffer proj-buf)
             busy (trace/busy-start! (.-worker_idx pj) "proj"
                                     (str "transform " n-coords " coords to " target-crs)
                                     pid)]
         (.set buf-f64 coords)
         (try
           (await (proj-wasm/proj-trans-array
                   #js {:p pj
                        :direction 1
                        :n n-coords
                        :coord proj-buf}))
           (catch :default e
             (trace/busy-end! busy (ex-message e))
             (throw e)))
         (trace/busy-end! busy)
         (js/Float64Array. buf-f64)))

     (defn- has-z?
       [geom]
       (let [c (js/globalThis.wasmts.geom.getCoordinate geom)
             z (when c (.-z c))]
         (and (number? z) (not (js/Number.isNaN z)))))

     ;; Not an async fn: squint puts each pass of a dotimes in an async fn
     ;; of its own when the loop is in an async fn.
     (defn- xy->proj-coords
       "The input of PROJ, one Float64Array of stride 4 (x y 0 0). `offsets`
        has the first vertex of each geometry and, last, the vertex count."
       [flats offsets]
       (let [n (.-length flats)
             coords (js/Float64Array. (* 4 (aget offsets n)))]
         (dotimes [i n]
           (let [flat (aget flats i)
                 off (aget offsets i)]
             (dotimes [k (quot (.-length flat) 2)]
               (aset coords (* 4 (+ off k)) (aget flat (* 2 k)))
               (aset coords (inc (* 4 (+ off k))) (aget flat (inc (* 2 k)))))))
         coords))

     ;; WasmTS reads the array one element for each call, so a geometry gets
     ;; only its x and y.
     (defn- proj-coords->xy
       "The x y array of the vertices `from` to `to` of a PROJ array."
       [coords from to]
       (let [xy (js/Float64Array. (* 2 (- to from)))]
         (dotimes [k (- to from)]
           (aset xy (* 2 k) (aget coords (* 4 (+ from k))))
           (aset xy (inc (* 2 k)) (aget coords (inc (* 4 (+ from k))))))
         xy))

     (defn- ^:async transform-geoms-flat
       "Reproject 2D `geoms` with no WKB: only the PROJ call leaves this
        thread. Gives the new geometries, or nil when a geometry has a Z."
       [geoms source-crs target-crs pid]
       (let [n (count geoms)
             flats (js/Array. n)
             offsets (js/Int32Array. (inc n))
             st #js {:z false}]
         (await (each-index
                 n
                 (fn [i]
                   (let [g (nth geoms i)]
                     (if (has-z? g)
                       (set! (.-z st) true)
                       (let [flat (js/globalThis.wasmts.geom.getCoordinatesFlat g)]
                         (aset flats i flat)
                         (aset offsets (inc i)
                               (+ (aget offsets i) (quot (.-length flat) 2)))))))))
         (when-not (.-z st)
           (let [coords (xy->proj-coords flats offsets)
                 moved (if (zero? (.-length coords))
                         coords
                         (await (proj-coords coords source-crs target-crs pid)))
                 out (js/Array. n)]
             (await (each-index
                     n
                     (fn [i]
                       (aset out i
                             (js/globalThis.wasmts.geom.applyCoordinates
                              (nth geoms i)
                              (proj-coords->xy moved (aget offsets i) (aget offsets (inc i)))
                              2)))))
             (vec out)))))

     (defn- ^:async transform-geoms-on-worker
       "Reproject `geoms` as 2D WKB on one claimed worker, whose WASM heap
        keeps the geometry handles between extractCoords and applyCoords."
       [geoms source-crs target-crs pid]
       (let [the-pool (cg-pool/require-pool)
             claim (cg-pool/claim-worker)
             worker-idx (.-index claim)
             release-fn (.-release claim)
             batch-id (str (random-uuid))
             busy (trace/busy-start! worker-idx "wasmts"
                                     (str "reproject " (count geoms) " features to " target-crs)
                                     pid)]
         (try
           (let [extract-result
                 (await (ffi/worker-call
                         the-pool "net.willcohen.wasmts" "extractCoords"
                         #js [#js {:batchId batch-id
                                   :geomsWkb (geoms->wkb-array geoms)}]
                         worker-idx))
                 coords (await (proj-coords (.-coords extract-result)
                                            source-crs target-crs pid))
                 apply-result
                 (await (ffi/worker-call
                         the-pool "net.willcohen.wasmts" "applyCoords"
                         #js [#js {:batchId batch-id
                                   :transformedCoords coords
                                   :boundaries (.-boundaries extract-result)}]
                         worker-idx))
                 new-geoms (wkb-array->geoms (.-wkb apply-result))]
             (release-fn)
             (trace/busy-end! busy)
             new-geoms)
           (catch :default e
             (try
               (await (ffi/worker-call
                       the-pool "net.willcohen.wasmts" "cleanup"
                       #js [#js {:batchId batch-id}]
                       worker-idx))
               (catch :default _ignored nil))
             (release-fn)
             (trace/busy-end! busy (ex-message e))
             (throw e)))))

     ;; A top-level defn, because squint drops :async on (fn ^:async ...).
     (defn- ^:async run-transform-batch
       "Reprojects one batch. Returns a promise."
       [batch source-crs target-crs]
       (check-geometry-column! batch "geometry")
       (let [pid (trace/current-step-pid)
             geoms (column batch "geometry")
             geom-count (count geoms)]
         (if (zero? geom-count)
           (wrap-dataset batch {:cg.dataset/needs-reindex true})
           (let [busy (trace/busy-start! "main" "main"
                                         (str "reproject " geom-count " features to " target-crs)
                                         pid)
                 new-geoms (try
                             (or (await (transform-geoms-flat geoms source-crs target-crs pid))
                                 (await (transform-geoms-on-worker geoms source-crs target-crs pid)))
                             (catch :default e
                               (trace/busy-end! busy (ex-message e))
                               (throw e)))]
             (trace/busy-end! busy)
             (wrap-dataset (add-or-update-column batch "geometry" new-geoms)
                           {:cg.dataset/needs-reindex true})))))

     (defn transform-batch-step
       "Flow step: reprojects a wrapped batch through PROJ in the joint pool.
        Returns a Promise of a wrapped batch flagged :cg.dataset/needs-reindex."
       [source-crs target-crs]
       (let [stage (reproject-stage target-crs)]
         (fn step-fn
           ([] {:params {:source-crs source-crs :target-crs target-crs}
                :ins  {:in "Dataset batch"}
                :outs {:out "Reprojected batch"}})
           ([_args] {})
           ([state lifecycle]
            (when (= lifecycle :clojure.core.async.flow/stop)
              ((:end! stage)))
            state)
           ([state _in-name wrapped-batch]
            ((:begin! stage) (:cg.dataset/table-rows (fd-meta wrapped-batch)))
            (-> (run-transform-batch (unwrap-dataset wrapped-batch)
                                     source-crs target-crs)
                (.then (fn [out]
                         ((:add! stage) (row-count out))
                         [state {:out [out]}])))))))

     (feature/register-operation! "cg.dataset/transform-batch-step"
                                  {:type :step-factory
                                   :params [{:name :source-crs :required true :type :string}
                                            {:name :target-crs :required true :type :string}]})))

;; transform-crs is an operation group: one DSL operation that expands to a
;; sub-graph of flow steps.

(defn- transform-crs-expansion
  "Returns the sub-graph of the transform-crs group: extract, transform and
   apply nodes on the JVM, one transform-batch-step node on CLJS, because
   three steps would round-trip WKB at each hand-off."
  [{:keys [source-crs target-crs]}]
  (let [mk-id (fn [prefix]
                #?(:clj (keyword (gensym prefix))
                   :cljs (str prefix (.toString (.random js/Math) 36))))]
    #?(:clj
       (let [extract-id (mk-id "extract-coords-")
             transform-id (mk-id "transform-coords-")
             apply-id (mk-id "apply-coords-")]
         {:nodes [{:id extract-id
                   :fn "cg.dataset/extract-coords-step"
                   :params {}}
                  {:id transform-id
                   :fn "cg.dataset/transform-coords-step"
                   :params {:source-crs source-crs :target-crs target-crs}}
                  {:id apply-id
                   :fn "cg.dataset/apply-coords-step"
                   :params {}}]
          :edges [{:from extract-id :to transform-id}
                  {:from transform-id :to apply-id}]
          :entry extract-id
          :exit apply-id})
       :cljs
       (let [step-id (mk-id "transform-batch-")]
         {:nodes [{:id step-id
                   :fn "cg.dataset/transform-batch-step"
                   :params {:source-crs source-crs :target-crs target-crs}}]
          :edges []
          :entry step-id
          :exit step-id}))))

;; A group has no defn: :expansion-fn is its runtime. bb gen:signatures reads
;; :dsl-name, :kind, :inputs and :outputs of each registration, group or
;; operation, and generates the parser contract. The runtime ignores those
;; keys.
(feature/register-group! "cg.dataset/transform-crs"
                         {:params [{:name :source-crs :required true :type :string}
                                   {:name :target-crs :required true :type :string}]
                          :expansion-fn transform-crs-expansion
                          :dsl-name "ds/transform-crs"
                          :kind :transformer
                          :flags #{:idempotent}
                          :threading-modes #{:table}
                          :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                   {:name "target-crs" :type :string :role nil :optional false}
                                   {:name "source-crs" :type :string :role nil :optional true}]
                          :outputs [{:name "dataset" :type :dataset :level :dataset}]})

;; The config-map `aggregate` op is in cg.run, because its reducer dispatch
;; needs resolve-operation and cg.run already requires cg.dataset. The
;; standard reducers are in cg.dataset.agg.

(defn coalesce
  "Returns `value` when it is not nil, else `default`."
  [value default]
  (if (nil? value) default value))

(defn write-geojson
  "Writes `dataset` as a GeoJSON FeatureCollection to `path`. Returns :ok.
   `geom-col` defaults to :geometry. Throws on CLJS, which has no file sink."
  ([dataset path]
   (write-geojson dataset path :geometry))
  ([dataset path geom-col]
   #?(:clj (spit path (dataset->geojson-string dataset geom-col))
      :cljs (throw (ex-info "cg.dataset/write-geojson :not-implemented in CLJS"
                            {:fn "cg.dataset/write-geojson"
                             :path path
                             :dataset dataset
                             :geom-col geom-col})))
   :ok))

(defn- geoparquet-off!
  "Throws the error of every GeoParquet call, because this build has no
   GeoParquet support."
  [fn-name path]
  (throw (ex-info (str fn-name ": GeoParquet is not available in this build.")
                  {:fn fn-name :path path :unsupported :geoparquet})))

(defn write-geoparquet
  "Throws, because this build has no GeoParquet support."
  [_dataset path & _]
  (geoparquet-off! "cg.dataset/write-geoparquet" path))

(defn read-geoparquet
  "Throws, because this build has no GeoParquet support."
  [path]
  (geoparquet-off! "cg.dataset/read-geoparquet" path))

#?(:clj
   (defn- vector-field-type
     "The cg.io field type of a column, from its first non-nil value. A
      column that has only nil values is :string."
     [values]
     (let [v (first (remove nil? values))]
       (cond
         (boolean? v) :boolean
         (integer? v) :integer
         (number? v)  :real
         :else        :string))))

#?(:clj
   (defn- vector-geometry-type
     "Returns the JTS type that all geometries share, else :unknown, as the
      geometry type to declare for a layer."
     [geoms]
     (let [types (into #{} (comp (remove nil?) (map #(.getGeometryType ^Geometry %)))
                       geoms)]
       (if (= 1 (count types))
         (keyword (cstr/lower-case (first types)))
         :unknown))))

(defn write-gdal?
  "True where write-gdal! can write a file. The JS runtime has no OGR write."
  []
  #?(:clj true :cljs false))

(defn write-gdal!
  "Writes `dataset` to `path` through GDAL/OGR. Returns
   {:path :layer :features <count>}. JVM only.

   opts: {:driver \"GPKG\"        GDAL driver name (default \"GPKG\")
          :layer  \"parcels\"      layer name (default the file basename)
          :geometry-column :geometry
          :epsg   4326}          declared CRS (default 4326)

   The values of a column must have one type, because the field type comes
   from the first non-nil value. A nil is an OGR NULL. A driver can truncate
   or change column names, for example the 10-character limit of a Shapefile
   DBF."
  ([dataset path] (write-gdal! dataset path {}))
  (#_{:clj-kondo/ignore [:unused-binding]}
   [dataset path opts]
   #?(:clj
      (let [geom-col (get opts :geometry-column :geometry)
            driver   (get opts :driver "GPKG")
            epsg     (get opts :epsg 4326)
            layer    (or (:layer opts)
                         (let [b (last (cstr/split path #"/"))
                               i (.lastIndexOf ^String b ".")]
                           (if (pos? i) (subs b 0 i) b)))
            has-geom (boolean (some #(= % geom-col) (ds/column-names dataset)))
            geoms    (when has-geom (vec (dataset geom-col)))
            val-cols (vec (remove #(= % geom-col) (ds/column-names dataset)))
            columns  (mapv #(vec (dataset %)) val-cols)
            fields   (mapv (fn [nm vs] {:name (name nm) :type (vector-field-type vs)})
                           val-cols columns)
            features (for [i (range (ds/row-count dataset))]
                       {:wkb (when-let [g (when has-geom (nth geoms i))]
                               (cio/write-wkb g))
                        :values (mapv #(nth % i) columns)})]
        (cio/write-ogr-layer! path
                              {:driver driver
                               :layer layer
                               :epsg epsg
                               :geometry-type (if has-geom
                                                (vector-geometry-type geoms)
                                                :unknown)
                               :fields fields}
                              features))
      :cljs (throw (ex-info "cg.dataset/write-gdal! :not-implemented in CLJS"
                            {:fn "cg.dataset/write-gdal!"
                             :path path})))))

(defn write-csv
  "Writes `dataset` to `path`. Returns :ok. On the JVM, tech.v3.dataset/write!
   selects the format from the file extension. Throws on CLJS."
  [dataset path]
  #?(:clj (ds/write! dataset path)
     :cljs (throw (ex-info "cg.dataset/write-csv :not-implemented in CLJS"
                           {:fn "cg.dataset/write-csv"
                            :path path
                            :dataset dataset})))
  :ok)

;; Tabulators with serializable specs. A spec is plain data, because a flow
;; graph stores data, never functions.
;;
;; Spec grammar:
;;   numeric fragment  {:eq n} | {:gte n} | {:lte n} | {:range [lo hi]}
;;   bucket            {:label "4-6" :range [4 6]}          (tests the value)
;;   presence row      {:label l :all <frag>}               (every value)
;;                     | {:label l :any <frag>}             (some value)
;;                     | {:label l :count <frag> :min k}    (>= k values)
;;   where condition   {:col k :present true}               (truthy column)
;;                     | {:col k :positive-number true}
;;                     | {:col k :prefix "1F"}              (string prefix)
;;                     | {:col k :eq v}
;;   where clause      [<condition> ...]                    (ANDed)

(defn- num-frag->pred
  "Numeric fragment -> predicate over a scalar."
  [frag]
  (cond
    (some? (:eq frag))    (fn [n] (= n (:eq frag)))
    (some? (:gte frag))   (fn [n] (>= n (:gte frag)))
    (some? (:lte frag))   (fn [n] (<= n (:lte frag)))
    (some? (:range frag)) (let [[lo hi] (:range frag)]
                            (fn [n] (and (>= n lo) (<= n hi))))
    :else (throw (ex-info "bucket spec needs one of :eq / :gte / :lte / :range"
                          {:spec frag}))))

(defn- presence-spec->pred
  "Presence-row spec -> predicate over a list of values."
  [spec]
  (cond
    (some? (:all spec))
    (let [p (num-frag->pred (:all spec))] (fn [vs] (every? p vs)))

    (some? (:any spec))
    (let [p (num-frag->pred (:any spec))] (fn [vs] (boolean (some p vs))))

    (some? (:count spec))
    (let [p (num-frag->pred (:count spec))
          k (or (:min spec) 1)]
      (fn [vs] (>= (count (clojure.core/filter p vs)) k)))

    :else (throw (ex-info "presence spec needs one of :all / :any / :count"
                          {:spec spec}))))

(defn- where-cond->pred
  [c]
  (let [col (:col c)]
    (cond
      (:present c)         (fn [r] (boolean (get r col)))
      (:positive-number c) (fn [r] (let [v (get r col)]
                                     (and (number? v) (pos? v))))
      (some? (:prefix c))  (fn [r] (let [v (get r col)]
                                     (and (string? v)
                                          #?(:clj  (cstr/starts-with? v (:prefix c))
                                             :cljs (.startsWith v (:prefix c))))))
      (some? (:eq c))      (fn [r] (= (get r col) (:eq c)))
      :else (throw (ex-info "where condition needs :present / :positive-number / :prefix / :eq"
                            {:spec c})))))

(defn- where->pred
  [conds]
  (let [ps (mapv where-cond->pred conds)]
    (fn [r] (every? (fn [p] (p r)) ps))))

(defn category-tally
  "Sums `weight-fn` (nil counts rows) over `rows*` into each [label pred] of
   `cats`, where pred tests (value-fn row). Returns {:total w :cells [{:label l
   :n w :pct p} ...]}, with :pct nil at a zero total."
  [rows* value-fn weight-fn cats]
  (let [wf  (or weight-fn (fn [_] 1))
        tot (clojure.core/reduce + 0 (map wf rows*))]
    {:total tot
     :cells (mapv (fn [[label pred]]
                    (let [n (clojure.core/reduce
                             + 0 (map wf (clojure.core/filter
                                          (fn [r] (pred (value-fn r))) rows*)))]
                      {:label label
                       :n n
                       :pct (when (pos? tot)
                              #?(:clj  (* 100.0 (/ (double n) tot))
                                 :cljs (* 100.0 (/ n tot))))}))
                  cats)}))

(defn scoped-category-tallies
  "Returns a vector of `category-tally` maps, one for all rows and one per
   scope value, each with {:scope <:all|value> :label}."
  [rows* scope-fn scopes scope-label value-fn weight-fn cats]
  (into [(assoc (category-tally rows* value-fn weight-fn cats) :scope :all :label "ALL")]
        (mapv (fn [v]
                (assoc (category-tally (clojure.core/filter (fn [r] (= v (scope-fn r))) rows*)
                                       value-fn weight-fn cats)
                       :scope v :label (scope-label v)))
              scopes)))

(defn modal-group-summaries
  "Groups `rows*` by `group-fn`, without nil keys, into {:class <modal
   class-fn value> :values [(value-fn row) ...]}. On a modal tie, the last wins."
  [rows* group-fn class-fn value-fn]
  (into {}
        (for [[g rs] (clojure.core/group-by group-fn rows*) :when g]
          [g {:class (key (apply max-key val (frequencies (map class-fn rs))))
              :values (mapv value-fn rs)}])))

(defn presence-tally
  "Counts the group summaries of each class in `classes` whose :values
   satisfy each [label list-pred] of `preds`. Returns {:counts {class n}
   :rows [{:label l :cells {class {:n :of :pct}}} ...]}."
  [summaries classes preds]
  (let [by-class (into {} (for [c classes]
                            [c (vec (for [[_ s] summaries :when (= c (:class s))]
                                      (:values s)))]))]
    ;; (get by-class c), not (by-class c): squint maps are plain JS
    ;; objects, not callable.
    {:counts (into {} (for [c classes] [c (count (get by-class c))]))
     :rows (mapv (fn [[label pred]]
                   {:label label
                    :cells (into {}
                                 (for [c classes]
                                   (let [lst (get by-class c)
                                         of  (count lst)
                                         n   (count (clojure.core/filter pred lst))]
                                     [c {:n n :of of
                                         :pct (when (pos? of)
                                                #?(:clj  (* 100.0 (/ (double n) of))
                                                   :cljs (* 100.0 (/ n of))))}])))})
                 preds)}))

#?(:cljs
   (defn- eval-boundary->plain
     "Converts ClojureScript persistent data from the eval in the browser to
      plain squint data at any depth: keywords to names, maps to objects and
      iterables to arrays. Plain data passes through unchanged."
     [x]
     (cond
       (or (nil? x) (string? x) (number? x) (boolean? x) (fn? x))
       x

       ;; Keyword object: prints as :name
       (and (some? (aget x "name")) (.startsWith (str x) ":"))
       (.slice (str x) 1)

       ;; persistent-map-like (ES6 Map surface)
       (and (fn? (aget x "get")) (fn? (aget x "keys")) (fn? (aget x "forEach")))
       (let [o #js {}]
         (.forEach x (fn [v k]
                       (aset o (str (eval-boundary->plain k))
                             (eval-boundary->plain v))))
         o)

       (js/Array.isArray x)
       (.map x (fn [v] (eval-boundary->plain v)))

       ;; persistent vector / seq / set (ES6 iterable, not a string)
       (some? (aget x js/Symbol.iterator))
       (.map (js/Array.from x) (fn [v] (eval-boundary->plain v)))

       (= (.-constructor x) js/Object)
       (let [o #js {}]
         (.forEach (js/Object.keys x)
                   (fn [k] (aset o k (eval-boundary->plain (aget x k)))))
         o)

       :else x)))

(defn- plain-spec
  "Converts a spec to plain squint data on cljs (see eval-boundary->plain).
   Returns it unchanged on the JVM."
  [spec]
  #?(:clj spec :cljs (eval-boundary->plain spec)))

(defn- report-rows
  "The rows of `data` as a vector of row maps. `data` is a dataset or a seq
   of row maps."
  [data]
  (if (dataset? data) (vec (rows data :as-maps)) (vec data)))

(defn- spec-value-fn
  [spec]
  (let [col (:value spec)]
    (if (:as-int spec)
      (fn [r] (let [v (get r col)]
                (if (number? v) #?(:clj (long v) :cljs (Math/trunc v)) v)))
      (fn [r] (get r col)))))

(defn summarize
  "Bucketed composition summary over a dataset (or seq of row maps), driven
   entirely by a serializable spec:

     {:value   :rs-existing-units  ; column bucket predicates test
      :as-int  true                ; truncate the value to an integer first
      :weight  :rows | :value      ; row count (default) or value sum
      :scope   :rs-district        ; optional: ALL row + one row per scope
      :scopes  [:rs-2 :rs-3]       ;   scope column values, in display order
      :where   [{:col :rs-district :present true} ...]  ; ANDed row filter
      :buckets [{:label \"1\" :eq 1} {:label \"4-6\" :range [4 6]} ...]}

   With :scope, returns the scoped-category-tallies vector (each entry
   {:scope :label :total :cells}); without, a single category-tally map."
  [data spec]
  (let [spec   (plain-spec spec)
        rows*  (report-rows data)
        rows*  (if-let [w (:where spec)] (filterv (where->pred w) rows*) rows*)
        vf     (spec-value-fn spec)
        wf     (when (= :value (:weight spec)) vf)
        cats   (mapv (fn [b] [(:label b) (num-frag->pred b)]) (:buckets spec))]
    (if-let [scope (:scope spec)]
      (scoped-category-tallies rows* (fn [r] (get r scope)) (:scopes spec)
                               (fn [v] (name v)) vf wf cats)
      (category-tally rows* vf wf cats))))

(defn block-presence
  "Presence/clustering summary over groups (blocks), driven by a spec:

     {:group   :census-block       ; grouping column (nil-group rows dropped)
      :class   :rs-district        ; per-group modal class (table columns)
      :classes [:rs-2 :rs-3]       ;   class display order
      :value   :rs-existing-units  ; per-row value collected into group lists
      :as-int  true
      :where   [...]               ; ANDed row filter (same grammar)
      :rows    [{:label \"all single-family\" :all {:eq 1}}
                {:label \"has >=2 two-family\" :count {:eq 2} :min 2} ...]}

   Returns the presence-tally map {:counts {class n} :rows [...]}."
  [data spec]
  (let [spec      (plain-spec spec)
        rows*     (report-rows data)
        rows*     (if-let [w (:where spec)] (filterv (where->pred w) rows*) rows*)
        vf        (spec-value-fn spec)
        summaries (modal-group-summaries rows*
                                         (fn [r] (get r (:group spec)))
                                         (fn [r] (get r (:class spec)))
                                         vf)
        preds     (mapv (fn [p] [(:label p) (presence-spec->pred p)]) (:rows spec))]
    (presence-tally summaries (:classes spec) preds)))

(feature/register-operation! "cg.dataset/summarize"
                             {:type :transformer
                              :params [{:name :spec :required true :type :map}]
                              :dsl-name "summarize"
                              :dsl-aliases ["ds/summarize"]
                              :kind :transformer
                              :threading-modes #{:table}
                              :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                       {:name "spec" :type :any :role nil :optional false}]
                              :outputs [{:name "summary" :type :any}]})

(feature/register-operation! "cg.dataset/block-presence"
                             {:type :transformer
                              :params [{:name :spec :required true :type :map}]
                              :dsl-name "block-presence"
                              :dsl-aliases ["ds/block-presence"]
                              :kind :transformer
                              :threading-modes #{:table}
                              :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                       {:name "spec" :type :any :role nil :optional false}]
                              :outputs [{:name "summary" :type :any}]})

(defn- fmt-report-pct [pct]
  (if (some? pct)
    (str " (" #?(:clj (Math/round (double pct)) :cljs (js/Math.round pct)) "%)")
    ""))

(defn- summarize-table
  "Scoped/unscoped summarize output -> {:columns :rows}."
  [result]
  (let [tallies (if (map? result) [result] result)
        buckets (mapv :label (:cells (first tallies)))]
    {:columns (into ["scope" "total"] buckets)
     :rows (mapv (fn [t]
                   (into [(str (or (:label t) "ALL")) (str (:total t))]
                         (mapv (fn [c] (str (:n c) (fmt-report-pct (:pct c))))
                               (:cells t))))
                 tallies)}))

(defn- presence-table
  "block-presence output -> {:columns :rows}. Column headers carry the
   per-class group counts; cells read n (pct%) over the class's blocks."
  [result]
  (let [classes (vec (keys (:counts result)))]
    {:columns (into ["block category"]
                    (mapv (fn [c] (str (name c) " (" (get (:counts result) c) ")"))
                          classes))
     :rows (mapv (fn [row]
                   (into [(str (:label row))]
                         (mapv (fn [c]
                                 (let [cell (get (:cells row) c)]
                                   (if (and cell (pos? (:of cell)))
                                     (str (:n cell) (fmt-report-pct (:pct cell)))
                                     "-")))
                               classes)))
                 (:rows result))}))

(defn category-crosstab
  "Crosstab of categorical columns, from a serializable spec:

     {:categories {:numerator :over               ; category counted on top
                   :known [:over :within]         ; its denominator
                   :unknown :unknown}             ; the undecided category
      :columns    [{:label \"Current\"} {:label \"Proposed\"} ...]
      :dimensions [{:label \"Height\"
                    :cols [:height-check          ; one column key per
                           :height-check-b        ; :columns entry, and
                           nil]}                  ; nil where none exists
                   ...]
      :where      [{:col :study-area :present true} ...]}  ; ANDed row filter

   Returns {:columns [...] :rows [{:label l :cells [cell ...]}]}: one row per
   dimension and one cell per column key. A cell is {:col key :n rows
   :counts {category n} :pct-of-known p :pct-unknown p}, or nil for a nil
   column key. `:pct-of-known` is nil when no row is decided. `:pct-unknown`
   is nil when there are no rows."
  [data spec]
  (let [spec  (plain-spec spec)
        rows* (report-rows data)
        rows* (if-let [w (:where spec)] (filterv (where->pred w) rows*) rows*)
        cats  (:categories spec)
        known (vec (:known cats))
        n     (count rows*)
        cell  (fn [col]
                (when col
                  (let [vals   (map (fn [r] (get r col)) rows*)
                        counts (frequencies vals)
                        ;; Sum with apply: `reduce` in this namespace is
                        ;; cg.dataset/reduce, not clojure.core/reduce.
                        n-known (apply + (map (fn [c] (get counts c 0)) known))
                        n-num   (get counts (:numerator cats) 0)
                        n-unk   (get counts (:unknown cats) 0)]
                    {:col col
                     :n n
                     :counts counts
                     :pct-of-known (when (pos? n-known)
                                     (* 100.0 (/ (double n-num) n-known)))
                     :pct-unknown (when (pos? n)
                                    (* 100.0 (/ (double n-unk) n)))})))]
    {:columns (vec (:columns spec))
     :rows (mapv (fn [d]
                   {:label (:label d)
                    :cells (mapv cell (:cols d))})
                 (:dimensions spec))}))

(feature/register-operation! "cg.dataset/category-crosstab"
                             {:type :transformer
                              :params [{:name :spec :required true :type :map}]
                              :dsl-name "category-crosstab"
                              :dsl-aliases ["ds/category-crosstab"]
                              :kind :transformer
                              :threading-modes #{:table}
                              :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                       {:name "spec" :type :any :role nil :optional false}]
                              :outputs [{:name "crosstab" :type :any}]})

(defn- label-column->label
  "Copies column `col` to a \"label\" string key on each row, because
   dataset->geojson-string writes a keyword key as \":label\"."
  [dataset col]
  (let [row-maps (rows dataset :as-maps)]
    (maps->dataset (mapv (fn [r] (assoc r "label" (str (get r col (get r (name col))))))
                         row-maps))))

(defn- named-data
  "`x` with each keyword as its name, at any depth. The JS runtime gives
   plain data (see plain-spec), which comes back as it is."
  [x]
  #?(:clj (cond
            (keyword? x)    (name x)
            (map? x)        (into {} (map (fn [[k v]] [(named-data k) (named-data v)])) x)
            (sequential? x) (mapv named-data x)
            :else           x)
     :cljs x))

(defn map-layer
  "Sink: converts a table to map-layer data {:layer true :var :geojson
   :spec} for the host. opts: :kind (:fill or :line), :color, :color-by,
   :colors, :popup, :label-by, :offset, :opacity, :counts, :name, :geometry.
   Needs EPSG:4326."
  ([dataset] (map-layer dataset nil))
  ([dataset opts]
   (let [opts (plain-spec opts)
         geom-col (or (:geometry opts) :geometry)
         labeled (if-let [lc (:label-by opts)]
                   (label-column->label dataset lc)
                   dataset)]
     {:layer true
      :var (or (:name opts) "result")
      :geojson (dataset->geojson-string labeled geom-col)
      :spec {:kind (when-let [k (:kind opts)] (name k))
             :color (:color opts)
             ;; (str c), not (name c): the host matches a geojson property
             ;; name, and dataset->geojson-string writes those with (str k).
             :color-by (when-let [c (:color-by opts)] (str c))
             ;; Pairs, because JSON keeps the order of an array.
             :colors (when-let [c (:colors opts)]
                       (mapv (fn [[value color]] [(str (value-name value)) color]) c))
             ;; The host reads the popup as JSON, where a keyword has no
             ;; form of its own.
             :popup (some-> (:popup opts) named-data)
             :offset (:offset opts)
             :opacity (:opacity opts)
             :counts (:counts opts)}})))

(defn report-table
  "Sink: converts a summarize or block-presence result to table data
   {:title :columns [<string>] :rows [[<string>]]}. opts: {:title \"...\"}."
  ([result] (report-table result nil))
  ([result opts]
   (let [opts (plain-spec opts)
         t (cond
             (and (map? result) (:counts result)) (presence-table result)
             :else (summarize-table result))]
     (assoc t :title (or (:title opts) "report")))))

(feature/register-operation! "cg.dataset/map-layer"
                             {:type :sink
                              :params [{:name :opts :required false :type :map}]
                              :dsl-name "map-layer"
                              :dsl-aliases ["ds/map-layer"]
                              :kind :sink
                              :threading-modes #{:table}
                              :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                       {:name "opts" :type :any :role nil :optional true}]
                              :outputs [{:name "layer" :type :any}]})

(feature/register-operation! "cg.dataset/report-table"
                             {:type :sink
                              :params [{:name :opts :required false :type :map}]
                              :dsl-name "report-table"
                              :dsl-aliases ["ds/report-table"]
                              :kind :sink
                              :threading-modes #{:table}
                              :inputs [{:name "summary" :type :any :role :row :optional false}
                                       {:name "opts" :type :any :role nil :optional true}]
                              :outputs [{:name "table" :type :any}]})

(feature/register-operation! "cg.dataset/max-by"
                             {:type :selector
                              :params [{:name :column_name :required true :type :keyword}]})

(feature/register-operation! "cg.dataset/min-by"
                             {:type :selector
                              :params [{:name :column_name :required true :type :keyword}]})

(feature/register-operation! "cg.dataset/reduce"
                             {:type :extractor
                              :params [{:name :reducer_kind :required true :type :keyword}]})

;; The bare DSL `filter` resolves here. :params describe the form
;; `(filter :col :predicate value)`; the dataset is the threaded receiver.
;; The parser contract is on cg.dataset/filter-column.
(feature/register-operation! "cg.dataset/filter"
                             {:type :transformer
                              :params [{:name :column :required true :type :keyword}
                                       {:name :predicate :required true :type :keyword}
                                       {:name :value :required true :type :any}]})

;; This registration holds the parser contract of `filter` and has no runtime
;; :params. At run time `filter` resolves to cg.dataset/filter above.
(feature/register-operation! "cg.dataset/filter-column"
                             {:type :transformer
                              :dsl-name "filter"
                              :dsl-aliases ["ds/filter-by-column"]
                              :kind :transformer
                              :threading-modes #{:table}
                              :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                       {:name "column" :type :keyword :role nil :optional false}
                                       {:name "predicate" :type :keyword :role nil :optional false}
                                       {:name "value" :type :any :role nil :optional false}]
                              :outputs [{:name "dataset" :type :dataset :level :dataset}]})

(feature/register-operation! "cg.dataset/group-by"
                             {:type :transformer
                              :params [{:name :column_name :required true :type :keyword}]
                              :dsl-name "group-by"
                              :kind :transformer
                              :threading-modes #{:table}
                              :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                       {:name "column" :type :keyword :role nil :optional false}]
                              :outputs [{:name "dataset" :type :dataset :level :groups}]})

(feature/register-operation! "cg.dataset/partition-by"
                             {:type :transformer
                              :params [{:name :column_name :required true :type :keyword}]
                              :dsl-name "partition-by"
                              :kind :transformer
                              :threading-modes #{:table}
                              :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                       {:name "column" :type :keyword :role nil :optional false}]
                              :outputs [{:name "dataset" :type :dataset :level :groups}]})

;; `map` and `into` have no defn: cg.run runs `map` over the groups (JVM
;; only), and into-map runs `into`. The component of `(map <component>)` is a
;; signature input, because the type checker must count one argument.
;; :row-shape is explicit, because `map` has no :params to derive it from, and
;; a rule on :level :groups also matches group-by. :fuseable false keeps these
;; ops out of the per-row fused chain.
(feature/register-operation! "cg.dataset/map"
                             {:type :transformer
                              :row-shape :over-groups
                              :fuseable false
                              :dsl-name "map"
                              :kind :transformer
                              :threading-modes #{:table}
                              :inputs [{:name "groups" :type :any :role :row :optional false}
                                       {:name "component" :type :any :role nil :optional false}]
                              :outputs [{:name "result" :type :any :level :groups}]})

(feature/register-operation! "cg.dataset/into"
                             {:type :transformer
                              :row-shape :whole-dataset
                              :fuseable false
                              :dsl-name "into"
                              :kind :transformer
                              :threading-modes #{:table}
                              :inputs [{:name "groups" :type :any :role :row :optional false}
                                       {:name "target" :type :any :role nil :optional false}]
                              :outputs [{:name "result" :type :any :level :map}]})

;; The loaders have :type :source for discovery only. The flow runtime reads
;; :source as a geometry constructor, and no table-> flow runs a loader.

(feature/register-operation! "cg.dataset/load-geojson"
                             {:type :source
                              :params [{:name :path :required true :type :string}]
                              :dsl-name "ds/load-geojson" :kind :source :threading-modes #{}
                              :inputs [{:name "path" :type :string :role nil :optional false}]
                              :outputs [{:name "dataset" :type :dataset :level :dataset}]})

(feature/register-operation! "cg.dataset/load-vector"
                             {:type :source
                              :params [{:name :path :required true :type :string}]})

(feature/register-operation! "cg.dataset/load-gpkg"
                             {:type :source
                              :params [{:name :path :required true :type :string}]})

(feature/register-operation! "cg.dataset/load-shapefile"
                             {:type :source
                              :params [{:name :path :required true :type :string}]
                              :dsl-name "ds/load-shapefile" :kind :source :threading-modes #{}
                              :inputs [{:name "path" :type :string :role nil :optional false}]
                              :outputs [{:name "dataset" :type :dataset :level :dataset}]})

(feature/register-operation! "cg.dataset/load-gdb"
                             {:type :source
                              :params [{:name :path :required true :type :string}]})

(feature/register-operation! "cg.dataset/load-gdal!"
                             {:type :source
                              :params [{:name :source :required true :type :string}]
                              :dsl-name "ds/load-gdal" :kind :source :threading-modes #{}
                              :inputs [{:name "source" :type :string :role nil :optional false}]
                              :outputs [{:name "dataset" :type :dataset :level :dataset}]})

;; Only the sinks have :params, because no table-> flow runs the other ops.

(feature/register-operation! "cg.dataset/->dataset"
                             {:type :transformer
                              :dsl-name "ds/->dataset" :kind :transformer :threading-modes #{}
                              :inputs [{:name "data" :type :any :role nil :optional false}]
                              :outputs [{:name "dataset" :type :dataset :level :dataset}]})

(feature/register-operation! "cg.dataset/geojson->dataset"
                             {:type :transformer
                              :dsl-name "ds/geojson->dataset" :kind :transformer :threading-modes #{}
                              :inputs [{:name "geojson" :type :string :role nil :optional false}]
                              :outputs [{:name "dataset" :type :dataset :level :dataset}]})

(feature/register-operation! "cg.dataset/row-count"
                             {:type :transformer
                              :dsl-name "ds/row-count" :kind :transformer :threading-modes #{}
                              :inputs [{:name "dataset" :type :dataset :role nil :optional false}]
                              :outputs [{:name "count" :type :number}]})

(feature/register-operation! "cg.dataset/column-names"
                             {:type :transformer
                              :dsl-name "ds/column-names" :kind :transformer :threading-modes #{}
                              :inputs [{:name "dataset" :type :dataset :role nil :optional false}]
                              :outputs [{:name "names" :type :any}]})

(feature/register-operation! "cg.dataset/has-column?"
                             {:type :transformer
                              :dsl-name "ds/has-column?" :kind :transformer :threading-modes #{}
                              :inputs [{:name "dataset" :type :dataset :role nil :optional false}
                                       {:name "column" :type :keyword :role nil :optional false}]
                              :outputs [{:name "present?" :type :boolean}]})

(feature/register-operation! "cg.dataset/rows"
                             {:type :transformer
                              :dsl-name "ds/rows" :kind :transformer :threading-modes #{}
                              :inputs [{:name "dataset" :type :dataset :role nil :optional false}
                                       {:name "shape" :type :keyword :role nil :optional true}]
                              :outputs [{:name "rows" :type :any}]})

(feature/register-operation! "cg.dataset/concat"
                             {:type :transformer
                              :dsl-name "ds/concat" :kind :transformer :threading-modes #{}
                              :inputs [{:name "datasets" :type :any :role nil :optional false}
                                       {:name "more" :type :any :role nil :optional true}]
                              :outputs [{:name "dataset" :type :dataset}]})

(feature/register-operation! "cg.dataset/write-geojson"
                             {:type :sink
                              :params [{:name :path :required true :type :string}]
                              :dsl-name "ds/write-geojson" :kind :sink :threading-modes #{:table}
                              :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                       {:name "path" :type :string :role nil :optional false}]
                              :outputs [{:name "ok" :type :any}]})

(feature/register-operation! "cg.dataset/write-csv"
                             {:type :sink
                              :params [{:name :path :required true :type :string}]
                              :dsl-name "ds/write-csv" :kind :sink :threading-modes #{:table}
                              :inputs [{:name "dataset" :type :dataset :role :row :optional false}
                                       {:name "path" :type :string :role nil :optional false}]
                              :outputs [{:name "ok" :type :any}]})
