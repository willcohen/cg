;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.ensure-dataset-test
  "ensure-dataset (cache-or-fetch over load-gdal!) tests. The cache file is
   GeoJSON."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as jio]
            [cg.dataset :as cds]
            [cg.feature :as f]
            [cg.cache-root-test :refer [with-cache-dir-property]]
            [cg.io-test :refer [with-geoservices-server]])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (org.locationtech.jts.geom Geometry)))

(set! *warn-on-reflection* true)

(defn- temp-cache-dir []
  (str (Files/createTempDirectory "cg-ensure-dataset" (make-array FileAttribute 0))))

(defn- cache-data-files
  "The cached datasets in `dir`, without the sidecars."
  [dir]
  (filter #(.endsWith ^String % ".geojson")
          (.list (jio/file dir))))

(defn- comparable-rows
  "Row maps with geometry replaced by WKT so datasets compare with =."
  [dataset]
  (mapv #(assoc % :geometry (str (:geometry %))) (cds/rows dataset :as-maps)))

(deftest ensure-dataset-fetches-once-then-serves-from-cache
  (with-geoservices-server
    (fn [layer-url requests]
      (let [cache-dir (temp-cache-dir)
            spec      {:url layer-url :version "v1" :cache-dir cache-dir}
            first-ds  (cds/ensure-dataset spec)
            n-fetch   (count @requests)]
        (testing "cold call fetches through the driver and pages"
          (is (pos? n-fetch))
          (is (= 3 (cds/row-count first-ds)))
          (is (= #{"A" "B" "C"}
                 (set (map #(get % "NAME") (cds/rows first-ds :as-maps))))))
        (testing "cache artifacts written"
          (is (= 1 (count (cache-data-files cache-dir))))
          (is (some #(.endsWith ^String % ".edn") (.list (jio/file cache-dir)))))
        (testing "warm call makes no requests and returns an identical dataset"
          (let [second-ds (cds/ensure-dataset spec)]
            (is (= n-fetch (count @requests)) "no new HTTP requests")
            (is (= (comparable-rows first-ds) (comparable-rows second-ds)))))
        (testing "geometries are JTS and re-tagged with the source CRS"
          (let [cached (cds/ensure-dataset spec)]
            (doseq [row (cds/rows cached :as-maps)]
              (is (instance? Geometry (:geometry row)))
              (is (= "EPSG:4326" (f/get-crs (:geometry row)))))))))))

(deftest ensure-dataset-caches-under-the-cache-root-by-default
  (with-geoservices-server
    (fn [layer-url _requests]
      (let [root (temp-cache-dir)]
        (with-cache-dir-property root
          (fn []
            (cds/ensure-dataset {:url layer-url :version "v1"})
            (is (= 1 (count (cache-data-files (str root "/geoparquet")))))))))))

(deftest ensure-dataset-version-and-refresh-refetch
  (with-geoservices-server
    (fn [layer-url requests]
      (let [cache-dir (temp-cache-dir)]
        (cds/ensure-dataset {:url layer-url :version "v1" :cache-dir cache-dir})
        (let [n1 (count @requests)]
          (testing "new :version refetches into a new cache entry"
            (cds/ensure-dataset {:url layer-url :version "v2" :cache-dir cache-dir})
            (is (> (count @requests) n1))
            (is (= 2 (count (cache-data-files cache-dir)))))
          (testing ":refresh? true refetches in place"
            (let [n2 (count @requests)]
              (cds/ensure-dataset {:url layer-url :version "v2"
                                   :cache-dir cache-dir :refresh? true})
              (is (> (count @requests) n2))
              (is (= 2 (count (cache-data-files cache-dir)))))))))))

(deftest ensure-dataset-distinct-opts-get-distinct-cache-entries
  (with-geoservices-server
    (fn [layer-url requests]
      (let [cache-dir (temp-cache-dir)]
        (cds/ensure-dataset {:url layer-url :version "v1" :cache-dir cache-dir})
        (let [n1 (count @requests)]
          (cds/ensure-dataset {:url layer-url :version "v1" :cache-dir cache-dir
                               :opts {:where "NAME='A'"}})
          (is (> (count @requests) n1) "different :opts is a cache miss")
          (is (= 2 (count (cache-data-files cache-dir)))))))))

;; A layer can give a field with no value in each row, as the parcel of
;; examples/parcel.cg does. The cache file has no such column.
(deftest cache-read-skips-a-column-with-no-value-in-any-row
  (let [path (str (temp-cache-dir) "/rows.geojson")]
    (spit path (str "{\"type\":\"FeatureCollection\",\"features\":[{\"type\":\"Feature\","
                    "\"geometry\":{\"type\":\"Point\",\"coordinates\":[1,2]},"
                    "\"properties\":{\"NAME\":\"A\"}}]}"))
    (let [read (#'cds/read-geojson-cache path ["NAME" "EMPTY" "geometry"])]
      (is (= ["NAME" "geometry"] (vec (cds/column-names read))))
      (is (= "A" (get (first (cds/rows read :as-maps)) "NAME"))))))
