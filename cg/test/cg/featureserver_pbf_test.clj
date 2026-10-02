;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.featureserver-pbf-test
  "Parity assertions for cg.featureserver-pbf against GDAL.

   Every fixture is a pair: the same FeatureServer query captured as f=pbf and as
   f=json. GDAL parses the JSON and is the oracle, so these tests compare two
   independent readers of the same features rather than comparing the decoder
   with itself.

   Three decoder defects during development were silent wrong answers that a
   row count accepted, so the assertions here compare every value and every
   vertex."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as jio]
            [cg.featureserver-pbf :as pbf]
            [cg.io :as cio])
  (:import (org.locationtech.jts.geom Geometry Coordinate)))

(set! *warn-on-reflection* true)

;; PBF geometry is quantized and f=json geometry is not, so the two readers
;; never agree exactly. Measured deviation across the fixtures is at most
;; 1.6e-4 layer units. The defects this catches were about 1e8 out.
(def ^:private coord-tolerance 1e-3)

(defn- fixture ^java.io.File [nm ext]
  (let [r (jio/resource (str "data/featureserver-pbf/" nm "." ext))]
    (assert r (str "missing fixture: " nm "." ext))
    (jio/file r)))

(defn- decoded [nm & [opts]]
  (pbf/decode-page (java.nio.file.Files/readAllBytes (.toPath (fixture nm "pbf")))
                   (or opts {})))

(defn- oracle [nm]
  (cio/load-source (.getPath (fixture nm "json"))))

(defn- geom-type [row] (some-> ^Geometry (:geometry row) .getGeometryType))
(defn- n-points [row] (some-> ^Geometry (:geometry row) .getNumPoints))

(defn- max-deviation [rows-a rows-b]
  (reduce max 0.0
          (for [[a b] (map vector rows-a rows-b)
                :let [^Geometry x (:geometry a) ^Geometry y (:geometry b)]
                :when (and x y (= (.getNumPoints x) (.getNumPoints y)))
                [^Coordinate ca ^Coordinate cb] (map vector (.getCoordinates x)
                                                     (.getCoordinates y))]
            (max (Math/abs (- (.getX ca) (.getX cb)))
                 (Math/abs (- (.getY ca) (.getY cb)))))))

(defn- check-parity [nm]
  (let [got (decoded nm)
        want (oracle nm)
        gr (:rows got) wr (:rows want)]
    (testing (str nm ": row count")
      (is (= (count wr) (count gr))))
    (testing (str nm ": geometry types")
      (is (= (mapv geom-type wr) (mapv geom-type gr))))
    (testing (str nm ": vertex counts")
      (is (= (mapv n-points wr) (mapv n-points gr))))
    (testing (str nm ": attributes")
      (is (= (mapv #(dissoc % :geometry) wr) (mapv #(dissoc % :geometry) gr))))
    (testing (str nm ": CRS descriptor")
      (is (= (:source-crs want) (:source-crs got))))
    (testing (str nm ": coordinates within quantization tolerance")
      (is (< (max-deviation gr wr) coord-tolerance)))
    got))

(deftest point-layer-matches-gdal
  ;; The point geometry type is enum value 0, so proto3 leaves geometryType off
  ;; the wire. Absent must mean Point, not missing.
  (let [got (check-parity "point")]
    (is (every? #(= "Point" (geom-type %)) (:rows got)))))

(deftest line-layer-matches-gdal
  (let [got (check-parity "line")]
    (is (seq (filter #(= "LineString" (geom-type %)) (:rows got))))))

(deftest polygon-with-hole-matches-gdal
  ;; The delta chain resets at every ring. Carrying it across rings puts the
  ;; hole about 1.2e8 units away, and only a multi-ring feature shows it.
  (let [got (check-parity "polygon-donut")
        multi (filter #(> (count (re-seq #"\(\(" (str (:geometry %)))) 0) (:rows got))]
    (is (seq multi))
    (is (some #(> (n-points %) 30) (:rows got))
        "the donut feature carries both rings")))

(deftest date-layer-matches-gdal
  (check-parity "dates"))

(deftest null-attributes-follow-the-gdal-reader
  ;; OGR_F_GetFieldAs* answers 0 or "" for an unset field. PBF reports the null.
  (let [coerced (decoded "dates")
        raw (decoded "dates" {:preserve-nulls? true})
        nils (fn [rows] (count (for [r rows [k v] (dissoc r :geometry)
                                     :when (nil? v)] k)))]
    (is (zero? (nils (:rows coerced))))
    (is (pos? (nils (:rows raw)))
        "the fixture carries at least one null attribute")))

(defn- fixture-bytes ^bytes [nm]
  (java.nio.file.Files/readAllBytes (.toPath (fixture nm "pbf"))))

;; A FeatureResult that carries nothing but exceededTransferLimit, hand-encoded:
;;   12 04     field 2 (queryResult), length-delimited, 4 bytes
;;   0A 02       field 1 (featureResult), length-delimited, 2 bytes
;;   48 01         field 9 (exceededTransferLimit), varint, true
;; A server that has run past the end of a result set answers this way, and the
;; test below is what proves the shape decodes as an empty page.
(def ^:private empty-page-with-flag
  (byte-array (map unchecked-byte [0x12 0x04 0x0A 0x02 0x48 0x01])))

(defn- recording-fetch
  "A fetch-page fn that answers from `pages` in order and records the offsets
   it was called with."
  [pages log]
  (fn [offset]
    (let [n (count @log)]
      (swap! log conj offset)
      (nth pages (min n (dec (count pages)))))))

(deftest transfer-limit-flag-reads-from-the-wire
  ;; proto3 elides a false bool, so absent must mean "the whole result set".
  ;; Two captured fixtures are truncated queries and two are complete.
  (is (true? (:exceeded-transfer-limit? (decoded "line"))))
  (is (true? (:exceeded-transfer-limit? (decoded "polygon-donut"))))
  (is (false? (:exceeded-transfer-limit? (decoded "point"))))
  (is (false? (:exceeded-transfer-limit? (decoded "dates")))))

(deftest empty-page-decodes-as-no-rows
  (let [r (pbf/decode-page empty-page-with-flag)]
    (is (= [] (:rows r)))
    (is (true? (:exceeded-transfer-limit? r)))))

(deftest load-pages-concatenates-and-offsets-fids
  ;; polygon-donut is the one fixture whose query did not return the object-id
  ;; column, so its fids are positional and a missing :fid-offset shows up as
  ;; page 2 repeating page 1's numbers.
  (let [log (atom [])
        donut (fixture-bytes "polygon-donut")
        point (fixture-bytes "point")
        result (pbf/load-pages (recording-fetch [donut donut point] log))]
    (testing "the loop ran until the server cleared the flag"
      (is (= [0 10 20] @log))
      (is (= 3 (:pages result))))
    (testing "rows from every page"
      (is (= 31 (count (:rows result)))))
    (testing "fids continue across the page boundary"
      (is (= (range 0 20) (map :fid (take 20 (:rows result))))))
    (testing "the CRS comes from the first page"
      (is (= (:source-crs (decoded "polygon-donut")) (:source-crs result))))))

(deftest load-pages-stops-on-an-empty-page
  ;; A server can truncate without setting the flag, and one that ignores
  ;; resultOffset would repeat page 1 forever. The empty page is the second
  ;; stop condition. :max-pages keeps a regression here a failure, not a hang.
  (let [log (atom [])
        result (pbf/load-pages
                (recording-fetch [(fixture-bytes "polygon-donut") empty-page-with-flag] log)
                {:max-pages 5})]
    (is (= [0 10] @log))
    (is (= 2 (:pages result)))
    (is (= 10 (count (:rows result))))))

(deftest load-pages-refuses-to-run-past-the-ceiling
  ;; line carries the flag set on every page, so this fetch never terminates.
  (let [log (atom [])
        fetch (recording-fetch [(fixture-bytes "line")] log)]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"page ceiling"
                          (pbf/load-pages fetch {:max-pages 3})))
    (is (= 3 (count @log)) "the ceiling counts pages read, not pages attempted")))

(deftest dates-format-like-the-gdal-reader
  (let [formatted (decoded "dates")
        raw (decoded "dates" {:raw-dates? true})
        date-keys (for [[k v] (dissoc (first (:rows formatted)) :geometry)
                        :when (and (string? v) (re-matches #"\d{4}/\d{2}/\d{2} .*" v))]
                    k)]
    (is (seq date-keys) "the fixture carries at least one date column")
    (doseq [k date-keys]
      (is (number? (get (first (:rows raw)) k))
          (str k " is epoch milliseconds when :raw-dates? is set")))))
