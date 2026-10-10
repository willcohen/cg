;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; The cg.dataset loaders and cg.io/load-source on both runtimes, with tiny.gpkg
;; (3 polygons a, b, c in EPSG:4326) and two GeoServices pages (2 + 1
;; features). Under /clip/, the two pages hold polygons with holes and parts
;; on both sides of the box [0 0 10 10]. Under /nan/, page 1 has a NaN value.
;; On cljs GDAL runs in the worker pool, and the footer starts the pool before
;; the tests run.
(ns cg.loaders-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.string :as str]
                    [clojure.java.io :as jio]
                    [charred.api :as charred]
                    [cg.dataset :as ds]
                    [cg.feature :as feature]
                    [cg.io :as cio]
                    [cg.io-test :as io-test]
                    [cg.test-runner :refer [after]])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [clojure.string :as str]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["../../src/cg/io.mjs" :as cio]
                     ["../../src/cg/trace_js.mjs" :as trace]
                     ["node:fs" :as fs]
                     ["node:http" :as http]
                     ["node:path" :as node-path]
                     ["./pool_setup.mjs" :refer [init_cg_pool_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_ after error-message]]))
  #?(:clj (:import (org.locationtech.jts.geom Geometry Polygon))))

#?(:clj (set! *warn-on-reflection* true))

(defn- fixture-path
  "The absolute path of a file in test/fixtures. Both runtimes run from the cg
   module root."
  [rel]
  #?(:clj (.getCanonicalPath (jio/file (str "test/fixtures/" rel)))
     :cljs (node-path/resolve (str "test/fixtures/" rel))))

(def ^:private tiny-gpkg-path (fixture-path "gdal/tiny.gpkg"))
(def ^:private geoservices-dir (fixture-path "geoservices"))

(defn- geoservices-fixture-for
  "Select the fixture file for a request: page 2, page 1, or the layer
   descriptor. The driver reads the layer descriptor before the two pages."
  [uri]
  (let [prefix (if (str/includes? uri "/clip/") "clip-" "")]
    (cond
      (str/includes? uri "resultOffset=2") (str prefix "page2.json")
      (str/includes? uri "/query")         (str prefix "page1.json")
      :else                                "layer.json")))

(defn- clip-layer-url [base-url]
  (str/replace base-url "/FeatureServer/0" "/clip/FeatureServer/0"))

(defn- nan-layer-url [base-url]
  (str/replace base-url "/FeatureServer/0" "/nan/FeatureServer/0"))

(defn- geoservices-body
  "The text of the fixture file `nm` for a request to `uri`. Under /nan/, page
   1 has a NaN value. JSON.parse rejects that page, and the json-c parser of
   GDAL reads it."
  [nm uri]
  (let [text #?(:clj (slurp (jio/file geoservices-dir nm))
                :cljs (fs/readFileSync (node-path/join geoservices-dir nm) "utf8"))]
    (if (and (str/includes? uri "/nan/") (= "page1.json" nm))
      (str/replace text "\"exceededTransferLimit\": true,"
                   "\"exceededTransferLimit\": true, \"maxScale\": NaN,")
      text)))

#?(:clj
   (defn- with-geoservices-server
     "Serve the GeoServices paging fixtures. Call (f base-layer-url requests).
      Then stop the server. requests is an atom of {:uri :headers} maps."
     [f]
     (io-test/with-fixture-server
       (fn [uri] ["application/json"
                  (.getBytes ^String (geoservices-body (geoservices-fixture-for uri) uri) "UTF-8")])
       f)))

#?(:cljs
   (defn- with-geoservices-server
     "Serve the GeoServices paging fixtures. Call (f base-layer-url requests),
      and stop the server when the Promise of f settles. requests is an atom
      of {:uri :headers} maps. `fixture-for` gives the fixture file for the
      URI of a request (default geoservices-fixture-for), or nil for a 404."
     ([f] (with-geoservices-server geoservices-fixture-for f))
     ([fixture-for f]
      (let [requests (atom [])
            server   (http/createServer
                      (fn [req res]
                        (let [uri (.-url req)
                              nm  (fixture-for uri)]
                          (swap! requests conj {:uri uri :headers (.-headers req)})
                          (.writeHead res (if nm 200 404) #js {"Content-Type" "application/json"})
                          (.end res (some-> nm (geoservices-body uri))))))]
        (-> (js/Promise. (fn [listening] (.listen server 0 "127.0.0.1" listening)))
            (.then (fn [_]
                     (f (str "http://127.0.0.1:" (.-port (.address server)) "/FeatureServer/0")
                        requests)))
            (.finally (fn []
                        (js/Promise. (fn [closed] (.close server (fn [_] (closed nil))))))))))))

(defn- source-crs
  "The CRS descriptor that load-source returns. On the JVM it is a Clojure map
   at :source-crs. On cljs it is a JS object at sourceCrs."
  [result]
  #?(:clj (:source-crs result)
     :cljs (.-sourceCrs result)))

(defn- parse-json [s]
  #?(:clj (charred/read-json s)
     :cljs (js/JSON.parse s)))

(defn- sorted-names
  "The values of a column as a sorted vector. Both runtimes then compare it
   structurally."
  [dataset col]
  (vec (sort (ds/column dataset col))))

(defn- first-page-uris
  "The URIs of the first-page queries that the fixture server received. The
   layer descriptor and the second page are not included."
  [requests]
  (filter (fn [u] (and (str/includes? u "/query")
                       (not (str/includes? u "resultOffset"))))
          (map :uri @requests)))

(defn- query-uri [requests]
  (first (first-page-uris requests)))

(defn- second-page-requested? [requests]
  (some? (first (filter (fn [u] (str/includes? u "resultOffset=2"))
                        (map :uri @requests)))))

;; :geometry reads the geometry column on both runtimes, because squint compiles
;; a keyword to its name. An attribute column name is a string on both runtimes.
(defn- check-tiny-gpkg! [d]
  (is (= 3 (ds/row-count d)) "tiny.gpkg has 3 features")
  (is (ds/has-column? d :geometry) "the dataset has a geometry column"))

(defn- check-names! [d]
  (is (= ["a" "b" "c"] (sorted-names d "name")) "the name column carries a/b/c"))

(defn- check-paged-result! [d requests]
  (is (= 3 (ds/row-count d)) "3 features across 2 pages prove the driver paged")
  (is (= ["A" "B" "C"] (sorted-names d "NAME")) "NAME round-trips from both pages")
  (let [q (query-uri requests)]
    (is (str/includes? q "f=json") "the query asks for JSON")
    (is (str/includes? q "where=1%3D1") "the default where is 1=1")
    (is (str/includes? q "outFields=*") "the default field list is *")
    (is (str/includes? q "orderByFields=OBJECTID") "paging orders by OBJECTID")
    (is (str/includes? q "resultRecordCount=1000") "the default page size is 1000"))
  (is (second-page-requested? requests) "the driver asked for a second page"))

(defn- shape
  "[type, part count, point count, hole count of the first part] of `g`."
  [^Geometry g]
  [(.getGeometryType g) (.getNumGeometries g) (.getNumPoints g)
   (.getNumInteriorRing ^Polygon (.getGeometryN g 0))])

(defn- shapes-by-name [d]
  (reduce (fn [m row] (assoc m (get row "NAME") (shape (:geometry row))))
          {}
          (ds/rows d :as-maps)))

;; HOLES and PARTS come from the first page, which GDAL reads on cljs. The
;; other three come from the second page, which JSON.parse reads.
(defn- check-clipped! [d]
  (let [shapes (shapes-by-name d)]
    (doseq [nm ["HOLES" "HOLES2"]]
      (is (= ["Polygon" 1 15 2] (get shapes nm))
          (str nm " keeps its shell and the holes in and across the box")))
    (doseq [nm ["PARTS" "PARTS2"]]
      (is (= ["Polygon" 1 5 0] (get shapes nm))
          (str nm " keeps the part in the box only")))
    (is (= ["Polygon" 1 10 1] (get shapes "AWAY"))
        "a polygon with no part in the box keeps each ring")))

(defn- check-unclipped! [d]
  (let [shapes (shapes-by-name d)]
    (is (= ["Polygon" 1 20 3] (get shapes "HOLES2")) "with no box each hole stays")
    (is (= ["MultiPolygon" 2 10 0] (get shapes "PARTS2")) "with no box each part stays")))

(defn- check-source-crs! [result]
  (let [crs (source-crs result)]
    (is (some? crs) "load-source returns a CRS descriptor")
    (is (= "EPSG" (:authority crs)) "tiny.gpkg is an EPSG CRS")
    (is (= "4326" (:code crs)) "tiny.gpkg is EPSG:4326")
    ;; The descriptor always carries the fully-expressive form, so a code-less
    ;; custom CRS survives the same path an EPSG-coded one takes.
    (let [projjson (parse-json (:projjson crs))]
      (is (= "GeographicCRS" (get projjson "type")) "tiny.gpkg is a geographic CRS")
      (is (= 4326 (get-in projjson ["id" "code"]))
          "the PROJJSON top-level id is EPSG:4326"))))

(defn- geojson-coords [g]
  (get (parse-json (cio/write-geojson g)) "coordinates"))

#?(:cljs
   (defn- events-of [events kind]
     (filter #(= kind (:kind %)) events)))

;; On cljs a loader gives a Promise. Each test gives its checks to
;; cg.test-runner/after and returns the result. cljs.test in squint waits for
;; a Promise that a test returns.

(deftest write-geojson-writes-coordinates-as-x-and-y
  (let [p (geojson-coords (feature/point 1 2))
        mp (geojson-coords (feature/multi-point [(feature/point 1 2) (feature/point 3 4)]))
        ring (first (geojson-coords (feature/polygon [[0 0] [1 0] [1 1] [0 0]])))]
    (is (= 2 (count p)) "a Point is [x y]")
    (is (and (== 1 (first p)) (== 2 (second p))))
    (is (= 2 (count mp)) "a MultiPoint is a list of [x y]")
    (is (and (== 3 (first (second mp))) (== 4 (second (second mp)))))
    (is (= [2 2 2 2] (mapv count ring)) "a Polygon ring is a list of [x y]")
    (is (and (== 1 (first (nth ring 2))) (== 1 (second (nth ring 2)))))))

(deftest load-gpkg-reads-the-fixture
  (after (ds/load-gpkg tiny-gpkg-path)
         (fn [d]
           (check-tiny-gpkg! d)
           (check-names! d))))

(deftest load-vector-dispatches-by-driver-discovery
  (after (ds/load-vector tiny-gpkg-path) check-tiny-gpkg!))

(deftest load-gdal-pages-a-geoservices-layer-url
  (with-geoservices-server
    (fn [layer-url requests]
      (after (ds/load-gdal! layer-url nil) #(check-paged-result! % requests)))))

(deftest load-gdal-threads-opts-and-auth
  (with-geoservices-server
    (fn [layer-url requests]
      (after (ds/load-gdal! layer-url
                            #?(:clj {:where "STATEFP='25'"
                                     :out-fields "OBJECTID,NAME"
                                     :result-record-count 2
                                     :extra-params {:resultType "standard"}
                                     :auth {:token "sekrit"}}
                               :cljs {:where "STATEFP='25'"
                                      :outFields "OBJECTID,NAME"
                                      :resultRecordCount 2
                                      :extraParams {:resultType "standard"}
                                      :auth {:token "sekrit"}}))
             (fn [_]
               (let [q (query-uri requests)]
                 (is (str/includes? q "where=STATEFP%3D%2725%27") "where threads through")
                 (is (str/includes? q "outFields=OBJECTID%2CNAME") "outFields threads through")
                 (is (str/includes? q "resultRecordCount=2") "resultRecordCount threads through")
                 (is (str/includes? q "resultType=standard") "extra params thread through"))
               ;; The JVM sends the token as a header. The JS runtime adds it to
               ;; the query string. Each runtime asserts on its own form.
               #?(:clj (doseq [r @requests]
                         (is (= "Bearer sekrit" (get (:headers r) "x-esri-authorization"))
                             (str "the auth header is on " (:uri r))))
                  :cljs (is (str/includes? (query-uri requests) "token=sekrit")
                            "the auth token rides the query string on the JS runtime")))))))

(deftest load-gdal-clips-polygons-to-a-box
  (with-geoservices-server
    (fn [layer-url _]
      (let [url (clip-layer-url layer-url)]
        (-> (ds/load-gdal! url #?(:clj {:clip-extent [0 0 10 10]}
                                  :cljs {:clipExtent [0 0 10 10]}))
            (after check-clipped!)
            (after (fn [_] (ds/load-gdal! url nil)))
            (after check-unclipped!))))))

(deftest load-gdal-pages-past-a-page-that-json-parse-rejects
  (testing "a page with NaN and exceededTransferLimit: true is not the last page"
    (is (str/includes? (geoservices-body "page1.json" "/nan/FeatureServer/0/query") "NaN")
        "page 1 under /nan/ has the NaN value")
    (with-geoservices-server
      (fn [layer-url requests]
        (after (ds/load-gdal! (nan-layer-url layer-url) nil)
               #(check-paged-result! % requests))))))

#?(:cljs
   (deftest the-page-fetch-runs-as-a-flow
     (with-geoservices-server
       (fn [layer-url _]
         (let [events   (atom [])
               listener (trace/add-trace-listener (fn [e] (swap! events conj e)))]
           (-> (ds/load-gdal! layer-url nil)
               (after (fn [_]
                        (let [start (first (events-of @events "flow-start"))
                              [fetch-pid parse-pid] (:pids start)
                              of-pid (fn [kind pid]
                                       (filter #(= pid (:pid %)) (events-of @events kind)))]
                          (is (str/ends-with? (str fetch-pid) "__fetch")
                              "flow-start names the fetch")
                          (is (str/ends-with? (str parse-pid) "__parse")
                              "flow-start names the parse")
                          (is (= 2 (count (of-pid "proc-complete" fetch-pid)))
                              "one fetch event for each page")
                          (is (= 3 (:done (last (of-pid "rows-progress" parse-pid))))
                              "the rows of both pages")
                          (is (seq (events-of @events "flow-complete")) "the flow completes"))))
               (.finally (fn [] (trace/remove-trace-listener listener)))))))))

#?(:cljs
   (deftest a-fetch-in-a-def-counts-its-rows-on-the-card
     (with-geoservices-server
       (fn [layer-url _]
         (let [events   (atom [])
               listener (trace/add-trace-listener (fn [e] (swap! events conj e)))]
           (trace/def-start! "parcels")
           (-> (ds/load-gdal! layer-url nil)
               (after (fn [_]
                        (trace/def-end!)
                        (is (= [2 3] (mapv :done (filter #(= "binding-parcels" (:pid %))
                                                         (events-of @events "def-progress"))))
                            "the card of the def counts the rows of each page")))
               (.finally (fn [] (trace/remove-trace-listener listener)))))))))

#?(:cljs
   (deftest a-repeated-first-page-stops-the-fetch
     ;; A server that ignores resultOffset gives page 1 for each request.
     (with-geoservices-server
       (fn [uri] (if (str/includes? uri "/query") "page1.json" "layer.json"))
       (fn [layer-url requests]
         (after (ds/load-gdal! layer-url nil)
                (fn [d]
                  (is (= 2 (ds/row-count d)) "the repeated page adds no rows")
                  (is (< (count (filter #(str/includes? (:uri %) "/query") @requests)) 5)
                      "the fetch stops after the repeated page")))))))

#?(:cljs
   (deftest a-404-page-rejects-the-load
     (with-geoservices-server
       (fn [uri] (when-not (str/includes? uri "/query") "layer.json"))
       (fn [layer-url _]
         (after (error-message #(ds/load-gdal! layer-url nil))
                (fn [msg]
                  (is (str/starts-with? (str msg) "cg.io/load-gdal-source: HTTP 404 "))))))))

#?(:cljs
   (deftest ensure-dataset-keeps-a-dataset-unless-keep-is-false
     (with-geoservices-server
       (fn [layer-url requests]
         (let [spec {:url layer-url :version "keep-test"}]
           (-> (ds/ensure-dataset (assoc spec :keep? false))
               (after (fn [_] (ds/ensure-dataset (assoc spec :keep? false))))
               (after (fn [_] (is (= 2 (count (first-page-uris requests)))
                                  "each call with :keep? false fetches")))
               (after (fn [_] (ds/ensure-dataset spec)))
               (after (fn [_] (ds/ensure-dataset spec)))
               (after (fn [_] (is (= 3 (count (first-page-uris requests)))
                                  "a kept dataset is fetched once")))))))))

(deftest load-source-captures-the-layer-crs
  (after (cio/load-source tiny-gpkg-path) check-source-crs!))

(deftest load-gpkg-tags-geometries-with-a-compact-crs
  (testing "a coded CRS stays authority:code on the geometry"
    (after (ds/load-gpkg tiny-gpkg-path)
           (fn [d]
             (is (= "EPSG:4326" (feature/get-crs (nth (ds/column d :geometry) 0)))
                 "an exact registry match stays compact, not PROJJSON")))))

(deftest load-gdal-passes-local-paths-through
  (after (ds/load-gdal! tiny-gpkg-path nil)
         (fn [d]
           (is (= 3 (ds/row-count d)) "a local .gpkg path routes to load-source")
           (check-names! d))))

#?(:cljs
   (defn ^:async setup! []
     (await (init_cg_pool_BANG_ {:gdal true}))
     (await (ds/init-arquero!))
     (await (geo/init-proj!))))

#?(:cljs (-> (setup!)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.loaders-test")))))
