;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dataset-write-gdal-test
  "Round-trip acceptance for cg.dataset/write-gdal! — dataset out through
   OGR, read back through the GPKG driver. JVM only: the OGR vector-write
   surface it calls needs a heap-write helper the GraalVM/wasm backend does
   not have.

   The nils are the point of the fixture. A parcel export carries columns
   that only apply to some rows (an RD dimensional requirement is nil off
   an RD district), and a nil that lands in the file as 0.0 instead of
   NULL is a silent data corruption — it reads as a real measurement."
  (:require [clojure.test :refer [deftest is testing]]
            [cg.dataset :as ds]
            [cg.feature :as feature]))

(set! *warn-on-reflection* true)

(defn- square
  [x0 y0 side]
  (feature/polygon [[x0 y0] [(+ x0 side) y0] [(+ x0 side) (+ y0 side)]
                    [x0 (+ y0 side)] [x0 y0]]))

(def ^:private fixture-rows
  [{:geometry (square 0.0 0.0 1.0) :name "a" :area 1.5 :yr 1900 :corner true}
   {:geometry (square 2.0 2.0 1.0) :name "b" :area 4.25 :yr 1950 :corner false}
   {:geometry (square 4.0 4.0 1.0) :name nil :area nil :yr nil :corner nil}])

(defn- with-temp-gpkg
  "Call f with a path to a not-yet-existing .gpkg, then clean up. The GPKG
   driver creates the file itself and will not write over one."
  [f]
  (let [tmp  (java.io.File/createTempFile "cg-write-gdal" ".gpkg")
        path (.getAbsolutePath tmp)]
    (.delete tmp)
    (try
      (f path)
      (finally
        (doseq [suffix ["" "-wal" "-shm"]]
          (.delete (java.io.File. (str path suffix))))))))

(deftest write-gdal-round-trips-a-gpkg
  (with-temp-gpkg
    (fn [path]
      (let [result (ds/write-gdal! (ds/->dataset fixture-rows) path
                                   {:layer "squares" :epsg 4326})]
        (testing "the write reports what it wrote"
          (is (= 3 (:features result)))
          (is (= "squares" (:layer result))))
        (let [back (ds/load-gpkg path)]
          (testing "every row survives"
            (is (= 3 (ds/row-count back))))
          (testing "values round-trip by type"
            (is (= ["a" "b"] (vec (take 2 (ds/column back "name")))))
            (is (= [1.5 4.25] (mapv double (take 2 (ds/column back "area")))))
            (is (= [1900 1950] (mapv long (take 2 (ds/column back "yr")))))
            ;; The boolean field is declared OFTInteger + OFSTBoolean, which
            ;; is what makes GPKG store a real BOOLEAN column. Reading it
            ;; back through OGR yields the integer, because the read path
            ;; dispatches on field TYPE and the subtype rides along with the
            ;; declaration, not the value.
            (is (= [1 0] (mapv long (take 2 (ds/column back "corner"))))))
          (testing "a nil writes NULL, never a zero-valued measurement"
            (is (nil? (nth (vec (ds/column back "name")) 2)))
            (is (nil? (nth (vec (ds/column back "area")) 2)))
            (is (nil? (nth (vec (ds/column back "yr")) 2)))
            (is (nil? (nth (vec (ds/column back "corner")) 2))))
          (testing "geometry survives with its declared CRS"
            (let [geoms (vec (ds/column back :geometry))]
              (is (= 3 (count (remove nil? geoms))))
              (is (every? #(= "Polygon" (.getGeometryType ^org.locationtech.jts.geom.Geometry %)) geoms))
              (is (= "EPSG:4326" (feature/get-crs (first geoms)))))))))))

(deftest write-gdal-defaults-the-layer-name-to-the-file-basename
  (with-temp-gpkg
    (fn [path]
      (let [result (ds/write-gdal! (ds/->dataset fixture-rows) path {})
            base   (let [n (.getName (java.io.File. ^String path))]
                     (subs n 0 (.lastIndexOf n ".")))]
        (is (= base (:layer result)))))))
