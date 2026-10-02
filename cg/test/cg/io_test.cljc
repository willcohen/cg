;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.io-test
  "cg.io/load-source-from-bytes and the GeoServices PBF path. JVM only;
   cg.loaders-test covers the loaders on both runtimes."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.io :as io]
                    [clojure.string]
                    [clojure.java.io :as jio]))
  #?(:clj (:import (org.locationtech.jts.geom Geometry Polygon)
                   (com.sun.net.httpserver HttpServer HttpHandler HttpExchange)
                   (java.net InetSocketAddress))))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (def ^:private tiny-gpkg-path
     ;; Absolute, so GDAL's filename routing does not depend on the working
     ;; directory.
     (.getCanonicalPath
      (jio/file "test/fixtures/gdal/tiny.gpkg"))))

#?(:clj
   (deftest load-source-from-bytes-tiny-gpkg
     (testing "bytes-form round-trip"
       (let [bytes      (with-open [in (jio/input-stream tiny-gpkg-path)]
                          (.readAllBytes in))
             files-map  {"tiny.gpkg" bytes}
             result     (io/load-source-from-bytes files-map "tiny.gpkg")
             rows       (:rows result)]
         (is (= 3 (count rows))
             "bytes-form yields the same 3 rows as the disk-form")
         (is (= #{"a" "b" "c"}
                (set (map #(get % "name") rows)))
             "name column round-trips a/b/c through the bytes path")
         (doseq [r rows]
           (is (instance? Polygon (:geometry r))
               ":geometry is a Polygon"))))
     (testing "missing open-name throws"
       (is (thrown? clojure.lang.ExceptionInfo
                    (io/load-source-from-bytes {"a.gpkg" (byte-array 0)}
                                                "missing.gpkg"))))))

;; The fixture server serves two GeoServices pages (2 + 1 features), and 3
;; rows show that paging ran. It records the headers for the :auth test.

#?(:clj
   (def ^:private geoservices-fixture-dir
     (.getCanonicalPath (jio/file "test/fixtures/geoservices"))))

#?(:clj
   (defn- geoservices-fixture-bytes ^bytes [nm]
     (java.nio.file.Files/readAllBytes
      (.toPath (jio/file geoservices-fixture-dir nm)))))

#?(:clj
   (defn- geoservices-fixture-for [uri]
     (cond
       (.contains ^String uri "resultOffset=2") "page2.json"
       (.contains ^String uri "/query")         "page1.json"
       :else                                    "layer.json")))

#?(:clj
   (defn- with-fixture-server
     "Spin up an HttpServer whose handler answers each request with
      (respond uri) => [content-type ^bytes body], call
      (f base-layer-url requests-atom), then stop. requests-atom accumulates
      {:uri :headers} per request (headers lower-cased)."
     [respond f]
     (let [requests (atom [])
           server   (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
           handler  (reify HttpHandler
                      (^void handle [_ ^HttpExchange ex]
                        (let [uri  (str (.getRequestURI ex))
                              hdrs (into {}
                                         (for [[k vs] (.getRequestHeaders ex)]
                                           [(.toLowerCase ^String k) (first vs)]))
                              [ctype ^bytes body] (respond uri)]
                          (swap! requests conj {:uri uri :headers hdrs})
                          (.set (.getResponseHeaders ex) "Content-Type" ctype)
                          (.sendResponseHeaders ex 200 (alength body))
                          (with-open [os (.getResponseBody ex)]
                            (.write os body)))))]
       (.createContext server "/" handler)
       (.start server)
       (try
         (let [port (.getPort (.getAddress server))]
           (f (str "http://127.0.0.1:" port "/FeatureServer/0") requests))
         (finally
           (.stop server 0))))))

#?(:clj
   (defn- with-geoservices-server
     "Serve the GeoServices paging fixtures, which are f=json only.
      A request for f=pbf gets the same JSON, which is how a server that does
      not serve PBF behaves: HTTP 200 with a body the decoder cannot read."
     [f]
     (with-fixture-server
       (fn [uri] ["application/json" (geoservices-fixture-bytes (geoservices-fixture-for uri))])
       f)))

#?(:clj
   (defn- pbf-fixture-bytes ^bytes [nm]
     (java.nio.file.Files/readAllBytes
      (.toPath (jio/file (jio/resource (str "data/featureserver-pbf/" nm ".pbf")))))))

#?(:clj
   (defn- with-pbf-server
     "Serve captured f=pbf pages. The 10-row polygon fixture carries
      exceededTransferLimit set on the wire and the 11-row point fixture
      carries it clear, so the paging loop runs exactly twice for a total of
      21 rows."
     [f]
     (with-fixture-server
       (fn [uri]
         ["application/x-protobuf"
          (pbf-fixture-bytes (if (.contains ^String uri "resultOffset=10") "point" "polygon-donut"))])
       f)))

;; f=pbf fetch path. A layer URL reads PBF by default and falls
;; back to the driver, so these cover both the fast path and the degrade.

#?(:clj
   (deftest load-gdal-source-reads-pbf-and-pages
     (with-pbf-server
       (fn [layer-url requests]
         (let [result (io/load-gdal-source layer-url)]
           (testing "both pages decoded"
             (is (= 21 (count (:rows result))))
             (is (= 2 (:pages result)))
             (doseq [r (:rows result)]
               (is (instance? Geometry (:geometry r)))))
           (testing "the query asked for pbf and carried the paging cursor"
             (is (every? #(.contains ^String (:uri %) "f=pbf") @requests))
             (is (= 2 (count @requests)))
             (is (.contains ^String (:uri (first @requests)) "resultOffset=0"))
             (is (.contains ^String (:uri (second @requests)) "resultOffset=10")))
           (testing "the page size is the observed server cap, not the json default"
             (is (.contains ^String (:uri (first @requests)) "resultRecordCount=2000")))
           (testing "no GDAL request was made"
             (is (not-any? #(.contains ^String (:uri %) "f=json") @requests))))))))

#?(:clj
   (deftest pbf-auth-header-reaches-every-page
     (with-pbf-server
       (fn [layer-url requests]
         (io/load-gdal-source layer-url :auth {:token "sekrit"})
         (is (= 2 (count @requests)))
         (doseq [{:keys [headers uri]} @requests]
           (is (= "Bearer sekrit" (get headers "x-esri-authorization"))
               (str "auth header present on " uri)))))))

#?(:clj
   (deftest pbf-falls-back-to-the-driver-on-a-json-error-body
     ;; A FeatureServer that cannot serve PBF answers 200 with a JSON body.
     ;; Feeding that to the decoder is the failure this fallback exists for.
     (with-geoservices-server
       (fn [layer-url requests]
         (let [result (io/load-gdal-source layer-url)]
           (is (= 3 (count (:rows result))) "the driver path produced the rows")
           (is (some #(.contains ^String (:uri %) "f=pbf") @requests)
               "pbf was tried first")
           (is (some #(.contains ^String (:uri %) "f=json") @requests)
               "the driver ran after it"))))))

#?(:clj
   (deftest pbf-strict-refuses-to-fall-back
     (with-geoservices-server
       (fn [layer-url _requests]
         (is (thrown-with-msg?
              clojure.lang.ExceptionInfo #"error body, not PBF"
              (io/load-gdal-source layer-url :fetch-format :pbf-strict)))))))

#?(:clj
   (deftest load-gdal-source-driver-prefix-passes-through
     (with-geoservices-server
       (fn [layer-url _requests]
         (let [url    (str "ESRIJSON:" layer-url
                           "/query?where=1%3D1&outFields=*&f=json"
                           "&orderByFields=OBJECTID+ASC")
               result (io/load-gdal-source url)]
           (is (= 3 (count (:rows result)))
               "caller-built ESRIJSON: URL opens and pages"))))))

