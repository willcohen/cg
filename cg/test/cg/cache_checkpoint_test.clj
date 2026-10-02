;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.cache-checkpoint-test
  "cg.cache/checkpoint: a long step stored as a file and read back.

   The checkpoint runs its component on a miss, writes the result and a
   sidecar of its value kinds, and returns what it reads back. On a hit it
   reads. A hit and a miss therefore give the same rows. The
   component is a table op, named at the call site."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [cg.dataset :as dataset]
            [cg.run :as run])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(set! *warn-on-reflection* true)

(def ^:private fixture
  "(ns cg.checkpoint-fixture
     (:require [cg.dataset :as ds]
               [cg.feature :as f]
               [cg.macros :refer [table->]]
               [cg.run :as run]))
   (run/load-module \"src/cg/cache.cg\")
   (clojure.core/require '[cg.cache :as cache])
   (def runs (atom 0))
   (defn ^:op cp-double [^:table rows]
     (swap! runs inc)
     (ds/->dataset
      (mapv (fn [r] {:id (:id r)
                     :n2 (* 2 (:n r))
                     :parity (if (even? (:n r)) :even :odd)
                     :big? (< 1 (:n r))
                     :sure (if (even? (:n r)) true :unknown)
                     :half (if (even? (:n r)) 1 0.5)
                     :geometry (f/point (:n r) 0.0)})
            (ds/rows rows :as-maps))))")

(use-fixtures :once
  (fn [t]
    (run/eval-cg fixture)
    (t)))

(defn- fixture-val [n]
  (deref (run/eval-cg (str "#'cg.checkpoint-fixture/" n))))

(defn- temp-dir ^String []
  (str (Files/createTempDirectory "cg-checkpoint" (make-array FileAttribute 0))))

(def ^:private input
  (delay (dataset/->dataset [{:id "a" :n 1} {:id "b" :n 2}])))

(defn- checkpoint [spec]
  ((deref (run/eval-cg "#'cg.cache/checkpoint")) @input spec "cp-double"))

(defn- plain-rows [dset]
  (mapv #(dissoc % :geometry) (dataset/rows dset :as-maps)))

(deftest a-miss-computes-and-a-hit-reads
  (let [dir  (temp-dir)
        spec {:dir dir :prefix "t" :layer "doubled" :key [1 "v1"]}
        runs (fixture-val "runs")
        _    (reset! runs 0)
        miss (checkpoint spec)
        hit  (checkpoint spec)]
    (testing "the miss runs the component once, and the hit does not"
      (is (= 1 @runs)))
    (testing "a hit and a miss give the same rows, with keyword names and values"
      (is (= [{:id "a" :n2 2 :parity :odd :big? false :sure :unknown :half 0.5}
              {:id "b" :n2 4 :parity :even :big? true :sure true :half 1.0}]
             (plain-rows miss)))
      (is (= (plain-rows miss) (plain-rows hit))))
    (testing "the file is a GeoPackage, and the geometry and its CRS come back"
      (let [g (:geometry (first (dataset/rows hit :as-maps)))]
        (is (= "Point" (.getGeometryType ^org.locationtech.jts.geom.Geometry g)))))
    (testing "the file and its sidecar carry the hash of the key"
      (let [names (set (.list (File. dir)))]
        (is (= 2 (count names)))
        (is (every? #(re-matches #"t-doubled-[0-9a-f]{8}\.(gpkg|edn)" %) names))))))

(deftest a-new-key-computes-again-and-prunes-the-old-version
  (let [dir  (temp-dir)
        runs (fixture-val "runs")
        _    (reset! runs 0)
        _    (checkpoint {:dir dir :prefix "t" :layer "doubled" :key [1 "v1"]})
        v1   (set (.list (File. dir)))
        _    (checkpoint {:dir dir :prefix "t" :layer "doubled" :key [1 "v2"]})
        v2   (set (.list (File. dir)))]
    (is (= 2 @runs))
    (is (= 2 (count v2)) "one version of the family stays")
    (is (empty? (set/intersection v1 v2)))))

(deftest a-checkpoint-is-a-rung-that-names-its-component
  (let [dir   (temp-dir)
        model (run/eval-cg
               (str "(in-ns 'cg.checkpoint-fixture)"
                    "(table-> :rows (cg.cache/checkpoint {:dir " (pr-str dir)
                    " :prefix \"t\" :layer \"doubled\" :key [3]} cp-double))"))
        node  (first (filter #(= "cg.cache/checkpoint" (str (:fn %))) (:nodes model)))]
    (testing "the model node carries the component's name"
      (is (= "cp-double" (:component node))))
    (testing "the rung runs the checkpoint over the table"
      (is (= [{:id "a" :n2 2 :parity :odd} {:id "b" :n2 4 :parity :even}]
             (mapv #(select-keys % [:id :n2 :parity])
                   (plain-rows (run/run-model model {:rows @input} {:timeout-ms 60000}))))))))
