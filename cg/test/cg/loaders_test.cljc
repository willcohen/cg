;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; The cg.dataset loaders and cg.io/load-source on both runtimes, with tiny.gpkg
;; (3 polygons a, b, c in EPSG:4326) and two GeoServices pages (2 + 1
;; features). Under /clip/, the two pages hold polygons with holes and parts
;; on both sides of the box [0 0 10 10]. On cljs GDAL runs in the worker pool, and each body starts the
;; pool and awaits a Promise of a dataset.
(ns cg.loaders-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.string :as str]
                    [clojure.java.io :as jio]
                    [charred.api :as charred]
                    [cg.dataset :as ds]
                    [cg.feature :as feature]
                    [cg.io :as cio])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     [clojure.string :as str]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["../../src/cg/io.mjs" :as cio]
                     ["node:fs" :as fs]
                     ["node:http" :as http]
                     ["node:path" :as node-path]
                     ["./pool_setup.mjs" :refer [init_cg_pool_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]]))
  #?(:clj (:import (com.sun.net.httpserver HttpServer HttpHandler HttpExchange)
                   (java.net InetSocketAddress)
                   (org.locationtech.jts.geom Geometry Polygon))))

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

#?(:clj
   (defn- with-geoservices-server
     "Serve the GeoServices paging fixtures. Call (f base-layer-url requests).
      Then stop the server. requests is an atom of {:uri :headers} maps."
     [f]
     (let [requests (atom [])
           server   (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
           handler  (reify HttpHandler
                      (^void handle [_ ^HttpExchange ex]
                        (let [uri  (str (.getRequestURI ex))
                              hdrs (into {}
                                         (for [[k vs] (.getRequestHeaders ex)]
                                           [(.toLowerCase ^String k) (first vs)]))
                              body (java.nio.file.Files/readAllBytes
                                    (.toPath (jio/file geoservices-dir
                                                       (geoservices-fixture-for uri))))]
                          (swap! requests conj {:uri uri :headers hdrs})
                          (.set (.getResponseHeaders ex) "Content-Type" "application/json")
                          (.sendResponseHeaders ex 200 (alength body))
                          (with-open [os (.getResponseBody ex)]
                            (.write os body)))))]
       (.createContext server "/" handler)
       (.start server)
       (try
         (f (str "http://127.0.0.1:" (.getPort (.getAddress server)) "/FeatureServer/0")
            requests)
         (finally
           (.stop server 0))))))

#?(:cljs
   (defn- spin-geoservices-server!
     "Resolve to a map with the keys :base-url, :requests and :close. The
      requests come from the CPLHTTPFetch callback in the worker, through the
      sync http-bridge. They do not come from js/fetch."
     []
     (let [requests (atom [])]
       (js/Promise.
        (fn [resolve-start]
          (let [handle (fn [req res]
                         (let [uri (.-url req)]
                           (swap! requests conj {:uri uri :headers (.-headers req)})
                           (.writeHead res 200 #js {"Content-Type" "application/json"})
                           (.end res (fs/readFileSync
                                      (node-path/join geoservices-dir
                                                      (geoservices-fixture-for uri))))))
                server (http/createServer handle)
                started (fn []
                          (resolve-start
                           {:base-url (str "http://127.0.0.1:"
                                           (.-port (.address server))
                                           "/FeatureServer/0")
                            :requests requests
                            :close (fn close-server []
                                     (js/Promise.
                                      (fn [done] (.close server (fn [] (done nil)))))) }))]
            (.listen server 0 "127.0.0.1" started)))))))

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

(defn- query-uri
  "The first query request that the fixture server received. The layer
   descriptor and the second page are not included."
  [requests]
  (first (filter (fn [u] (and (str/includes? u "/query")
                              (not (str/includes? u "resultOffset"))))
                 (map :uri @requests))))

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
    (is (some? q) "the driver issued a query request")
    (is (str/includes? q "f=json") "the query asks for JSON")
    (is (str/includes? q "where=1%3D1") "the default where is 1=1")
    (is (str/includes? q "outFields=*") "the default field list is *")
    (is (str/includes? q "orderByFields=OBJECTID") "paging orders by OBJECTID")
    (is (str/includes? q "resultRecordCount=1000") "the default page size is 1000"))
  (is (second-page-requested? requests) "the driver asked for a second page"))

(defn- check-threaded-opts! [requests]
  (let [q (query-uri requests)]
    (is (str/includes? q "where=STATEFP%3D%2725%27") "where threads through")
    (is (str/includes? q "outFields=OBJECTID%2CNAME") "outFields threads through")
    (is (str/includes? q "resultRecordCount=2") "resultRecordCount threads through")
    (is (str/includes? q "resultType=standard") "extra params thread through")))

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

(defn- check-compact-crs-tag! [d]
  (let [g (nth (ds/column d :geometry) 0)]
    (is (= "EPSG:4326" (feature/get-crs g))
        "an exact registry match stays compact, not PROJJSON")))

;; Each driver is a named ^:async fn. squint applies ^:async only to the outer
;; dispatcher of a multi-arity defn. An anonymous (fn ^:async [] ...) puts the
;; metadata on the arglist and loses it. Each deftest returns the Promise, and
;; cljs.test in squint waits for it.

(defn- geojson-coords [g]
  (get #?(:clj (charred/read-json (cio/write-geojson g))
          :cljs (js/JSON.parse (cio/write-geojson g)))
       "coordinates"))

(defn- check-point-coords! []
  (let [p (geojson-coords (feature/point 1 2))
        mp (geojson-coords (feature/multi-point [(feature/point 1 2) (feature/point 3 4)]))]
    (is (= 2 (count p)) "a Point is [x y]")
    (is (and (== 1 (first p)) (== 2 (second p))))
    (is (= 2 (count mp)) "a MultiPoint is a list of [x y]")
    (is (and (== 3 (first (second mp))) (== 4 (second (second mp)))))))

#?(:cljs
   (do
     (defn ^:async setup! []
       (await (init_cg_pool_BANG_ {:gdal true}))
       (await (ds/init-arquero!))
       (await (geo/init-proj!)))

     (defn ^:async run-load-gpkg! []
       (await (setup!))
       (let [d (await (ds/load-gpkg tiny-gpkg-path))]
         (check-tiny-gpkg! d)
         (check-names! d)))

     (defn ^:async run-point-coords! []
       (await (setup!))
       (check-point-coords!))

     (defn ^:async run-load-vector! []
       (await (setup!))
       (check-tiny-gpkg! (await (ds/load-vector tiny-gpkg-path))))

     (defn ^:async run-paging! []
       (await (setup!))
       (let [srv (await (spin-geoservices-server!))]
         (try
           (let [d (await (ds/load-gdal! (:base-url srv) nil))]
             (check-paged-result! d (:requests srv)))
           (finally
             (await ((:close srv)))))))

     (defn ^:async run-threaded-opts! []
       (await (setup!))
       (let [srv (await (spin-geoservices-server!))]
         (try
           (await (ds/load-gdal! (:base-url srv)
                                 {:where "STATEFP='25'"
                                  :outFields "OBJECTID,NAME"
                                  :resultRecordCount 2
                                  :extraParams {:resultType "standard"}
                                  :auth {:token "sekrit"}}))
           (check-threaded-opts! (:requests srv))
           ;; The cljs branch adds the token to the query string. The JVM
           ;; branch sends an X-Esri-Authorization header. Each runtime asserts
           ;; on its own form.
           (is (str/includes? (query-uri (:requests srv)) "token=sekrit")
               "the auth token rides the query string on the JS runtime")
           (finally
             (await ((:close srv)))))))

     (defn ^:async run-clip! []
       (await (setup!))
       (let [srv (await (spin-geoservices-server!))
             url (clip-layer-url (:base-url srv))]
         (try
           (check-clipped! (await (ds/load-gdal! url {:clipExtent [0 0 10 10]})))
           (check-unclipped! (await (ds/load-gdal! url nil)))
           (finally
             (await ((:close srv)))))))

     (defn- first-page-requests [requests]
       (count (filter (fn [u] (and (str/includes? u "/query")
                                   (not (str/includes? u "resultOffset"))))
                      (map :uri @requests))))

     (defn ^:async run-keep! []
       (await (setup!))
       (let [srv  (await (spin-geoservices-server!))
             spec {:url (:base-url srv) :version "keep-test"}]
         (try
           (await (ds/ensure-dataset (assoc spec :keep? false)))
           (await (ds/ensure-dataset (assoc spec :keep? false)))
           (is (= 2 (first-page-requests (:requests srv))) "each call with :keep? false fetches")
           (await (ds/ensure-dataset spec))
           (await (ds/ensure-dataset spec))
           (is (= 3 (first-page-requests (:requests srv))) "a kept dataset is fetched once")
           (finally
             (await ((:close srv)))))))

     (defn ^:async run-source-crs! []
       (await (setup!))
       (check-source-crs! (await (cio/load-source tiny-gpkg-path))))

     (defn ^:async run-compact-crs! []
       (await (setup!))
       (check-compact-crs-tag! (await (ds/load-gpkg tiny-gpkg-path))))

     (defn ^:async run-local-path! []
       (await (setup!))
       (let [d (await (ds/load-gdal! tiny-gpkg-path nil))]
         (is (= 3 (ds/row-count d)) "a local .gpkg path routes to load-source")
         (check-names! d)))))

(deftest write-geojson-writes-points-as-x-and-y
  #?(:clj (check-point-coords!)
     :cljs (run-point-coords!)))

(deftest load-gpkg-reads-the-fixture
  (testing "load-gpkg returns a 3-row dataset whose name column is a/b/c"
    #?(:clj (let [d (ds/load-gpkg tiny-gpkg-path)]
              (check-tiny-gpkg! d)
              (check-names! d))
       :cljs (run-load-gpkg!))))

(deftest load-vector-dispatches-by-driver-discovery
  (testing "load-vector picks the GPKG driver for a .gpkg path"
    #?(:clj (check-tiny-gpkg! (ds/load-vector tiny-gpkg-path))
       :cljs (run-load-vector!))))

(deftest load-gdal-pages-a-geoservices-layer-url
  (testing "a FeatureServer layer URL pages through the GeoServices driver"
    #?(:clj (with-geoservices-server
              (fn [layer-url requests]
                (check-paged-result! (ds/load-gdal! layer-url) requests)))
       :cljs (run-paging!))))

(deftest load-gdal-threads-opts-and-auth
  (testing "where / outFields / result count / extra params reach the query"
    #?(:clj (with-geoservices-server
              (fn [layer-url requests]
                (ds/load-gdal! layer-url
                               :where "STATEFP='25'"
                               :out-fields "OBJECTID,NAME"
                               :result-record-count 2
                               :extra-params {:resultType "standard"}
                               :auth {:token "sekrit"})
                (check-threaded-opts! requests)
                ;; The JVM branch sends the token as a header. The cljs branch
                ;; adds it to the query string. Each runtime asserts on its own
                ;; form.
                (is (pos? (count @requests)) "the server received a request")
                (doseq [r @requests]
                  (is (= "Bearer sekrit" (get (:headers r) "x-esri-authorization"))
                      (str "the auth header is on " (:uri r))))))
       :cljs (run-threaded-opts!))))

(deftest load-gdal-clips-polygons-to-a-box
  (testing "a clip extent drops each part and hole that misses it, on each page"
    #?(:clj (with-geoservices-server
              (fn [layer-url _]
                (check-clipped! (ds/load-gdal! (clip-layer-url layer-url)
                                               :clip-extent [0 0 10 10]))
                (check-unclipped! (ds/load-gdal! (clip-layer-url layer-url)))))
       :cljs (run-clip!))))

#?(:cljs
   (deftest ensure-dataset-keeps-a-dataset-unless-keep-is-false
     (testing "the JS runtime keeps a fetched dataset in memory unless :keep? is false"
       (run-keep!))))

(deftest load-source-captures-the-layer-crs
  (testing "load-source reports authority, code, and the PROJJSON document"
    #?(:clj (check-source-crs! (cio/load-source tiny-gpkg-path))
       :cljs (run-source-crs!))))

(deftest load-gpkg-tags-geometries-with-a-compact-crs
  (testing "a coded CRS stays authority:code on the geometry"
    #?(:clj (check-compact-crs-tag! (ds/load-gpkg tiny-gpkg-path))
       :cljs (run-compact-crs!))))

(deftest load-gdal-passes-local-paths-through
  (testing "load-gdal! routes a local path to the file loader"
    #?(:clj (let [d (ds/load-gdal! tiny-gpkg-path)]
              (is (= 3 (ds/row-count d)) "a local .gpkg path routes to load-source")
              (check-names! d))
       :cljs (run-local-path!))))

;; No init-wasmts! preamble: nothing at load time builds a geometry, and each
;; cljs body awaits setup! before it touches the pool.
#?(:cljs (run_tests_and_exit_BANG_ "cg.loaders-test"))
