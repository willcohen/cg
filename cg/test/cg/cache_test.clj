;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.cache-test
  "cg.cache: the path of each cached layer, and the prune of stale files.

   Each file of an analysis cache has the name
   <prefix>-<layer>-<hash>.gpkg. The prune deletes the files of one
   family (a name prefix) whose hash is not the current one, so a family
   prefix must not be the start of another family's names."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [cg.run :as run])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(set! *warn-on-reflection* true)

(use-fixtures :once
  (fn [t]
    (run/load-module "src/cg/cache.cg")
    (t)))

(defn- cache-fn [n]
  (deref (run/eval-cg (str "#'cg.cache/" n))))

(deftest each-layer-has-one-path-shape
  (let [layer-path (cache-fn "layer-path")]
    (is (= "d/ex-rows-0a1b2c3d.gpkg" (layer-path "d" "ex" "0a1b2c3d" "rows")))
    (is (= "d/ex-debug-edges-0a1b2c3d.gpkg"
           (layer-path "d" "ex" "0a1b2c3d" "debug-edges")))
    (is (= "d/ex-study-zones-090c0be0.gpkg"
           (layer-path "d" "ex-study" "090c0be0" "zones")))))

(deftest the-prune-keeps-the-current-hash-and-other-families
  (let [dir   (str (Files/createTempDirectory "cg-cache" (make-array FileAttribute 0)))
        names ["ex-rows-aaaaaaaa.gpkg" "ex-rows-bbbbbbbb.gpkg"
               "ex-district-rows-cccccccc.gpkg" "ex-city-rows-dddddddd.gpkg"
               "ex-debug-edges-bbbbbbbb.gpkg"]]
    (doseq [n names] (spit (str dir "/" n) ""))
    ((cache-fn "prune-stale!") dir "ex-rows-" "aaaaaaaa")
    (is (= #{"ex-rows-aaaaaaaa.gpkg" "ex-district-rows-cccccccc.gpkg"
             "ex-city-rows-dddddddd.gpkg" "ex-debug-edges-bbbbbbbb.gpkg"}
           (set (.list (File. dir)))))))

;; A rows file has a sidecar of its value kinds, <prefix>-rows-<hash>.edn.
;; A stale sidecar goes with its stale rows file.
(deftest the-prune-removes-a-stale-sidecar
  (let [dir   (str (Files/createTempDirectory "cg-cache" (make-array FileAttribute 0)))
        names ["ex-rows-aaaaaaaa.gpkg" "ex-rows-aaaaaaaa.edn"
               "ex-rows-bbbbbbbb.gpkg" "ex-rows-bbbbbbbb.edn"
               "ex-district-rows-cccccccc.edn"]]
    (doseq [n names] (spit (str dir "/" n) ""))
    ((cache-fn "prune-stale!") dir "ex-rows-" "aaaaaaaa")
    (is (= #{"ex-rows-aaaaaaaa.gpkg" "ex-rows-aaaaaaaa.edn"
             "ex-district-rows-cccccccc.edn"}
           (set (.list (File. dir)))))))

;; version-hash builds its eight hex digits with no format (squint has
;; none), and on the JVM it must give the name that format gave: every
;; cache file on disk carries one.
(deftest version-hash-is-the-jvm-hash-in-eight-hex-digits
  (let [version-hash (cache-fn "version-hash")
        keys         (cons ["0a1b2c3d" "2026-07-31"] (map (fn [i] [i "k"]) (range 200)))]
    (is (some #(neg? (hash (pr-str %))) keys) "the keys include negative hashes")
    (is (= (mapv #(format "%08x" (hash (pr-str %))) keys)
           (mapv version-hash keys)))))
