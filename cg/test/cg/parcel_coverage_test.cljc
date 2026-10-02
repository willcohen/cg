;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parcel-coverage-test
  "The building coverage of analyze-parcel, on both runtimes. With
   :coverage-exempt-structure in the thresholds, a small, detached,
   one-story structure that is not the main building does not count. The
   lot here is 50 by 100 with a house, two sheds and a garage, and each
   number is made up."
  #?(:clj (:require [clojure.test :refer [deftest is testing use-fixtures]]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private parcel "src/cg/parcel.cg")

#?(:clj (use-fixtures :once (fn [t] (run/load-module parcel) (t))))

;; Each threshold that analyze-parcel reads, in feet, without the
;; carve-out.
(def ^:private base-thresholds
  "{:units {:length \"ft\" :area \"sf\" :large-area \"acres\" :area-per-large-area 43560.0}
    :min-front-street-width 20.0 :corner-max-angle-deg 135.0
    :rear-angle-tolerance-deg 45.0 :pointed-lot-max-angle-deg 80.0
    :pointed-lot-min-rear-length 10.0 :construct-imaginary-rear? true
    :primary-front-selector :widest-right-of-way :treat-curved-street-as-tangent? true
    :split-edge-at-abutting-changes? true :abutting-split-min-segment-length 2.0
    :min-frontage-length 10.0 :same-street-angle-deg 25.0 :same-street-offset 15.0
    :same-street-gap 25.0 :corner-gap 40.0 :through-parallel-tol-deg 25.0
    :vertex-tolerance 1.0e-3 :right-of-way-width-step 5 :right-of-way-width-max 500
    :right-of-way-width-resolution 0.1 :slice-vertex-snap 0.1 :slice-dedup 1.0e-3
    :split-min-coarse 6.0 :pointed-rear-step 0.5 :pointed-rear-max 500}")

;; The shed is 10 by 12 (120), the big shed 12 by 15 (180), the garage 20
;; by 20 (400) and the house 30 by 40 (1,200). `coverage` gives the counted
;; area, the count of the excluded structures and their area. The first
;; building is the main building.
(def ^:private scene
  (str "(require '[cg.parcel :as parcel] '[cg.feature :as f] '[cg.geo :as geo]
                 '[cg.dataset :as ds] '[cg.util :as util])
        (let [rect      (fn [x y w d]
                          (f/polygon [[x y] [(+ x w) y] [(+ x w) (+ y d)] [x (+ y d)] [x y]]))
              lot       (rect 0 0 50 100)
              house     {:geometry (rect 10 10 30 40) :building-height 28.0}
              shed      {:geometry (rect 5 80 10 12) :building-height 9.0}
              big-shed  {:geometry (rect 5 60 12 15) :building-height 10.0}
              garage    {:geometry (rect 25 70 20 20) :building-height 12.0}
              all       [house shed big-shed garage]
              carve-out {:max-area 120.0 :small-dwelling-max-area 200.0
                         :small-dwelling-max-units 2 :max-height 15.0}
              base      " base-thresholds "
              schema    {:parcel-id-keys [:id] :units-col :UNITS :commercial-units-col :COM}
              plain     {:schema schema :thresholds base}
              opts      {:schema schema
                         :thresholds (assoc base :coverage-exempt-structure carve-out)}
              scored    (fn [b] {:building b
                                 :area (geo/area (geo/intersection lot (:geometry b)))})
              coverage  (fn [row buildings opts]
                          (let [out (parcel/split-coverage-buildings
                                     (mapv scored buildings) (first buildings) row opts)]
                            [(reduce + 0.0 (map :area (:counted out)))
                             (count (:excluded out))
                             (reduce + 0.0 (map :area (:excluded out)))]))
              analyzed  (fn [row buildings opts]
                          (let [lot-row (assoc row :geometry lot :id \"lot-1\")
                                m (:metrics
                                   (persistent!
                                    (parcel/analyze-parcel
                                     (transient lot-row)
                                     (ds/add-spatial-index (ds/->dataset [lot-row]))
                                     (ds/add-spatial-index (ds/->dataset buildings))
                                     opts)))]
                            [(:building-coverage-area m)
                             (:building-coverage-ratio m)
                             (:building-coverage-excluded-count m)
                             (:building-coverage-excluded-area m)]))]
          "))

(defn- ev [expr]
  (vec (run/eval-cg (str scene expr ")"))))

(deftest a-small-dwelling-lot-excludes-each-shed
  (testing "1 unit: the two sheds are at most 200, and the garage is not"
    (is (= [1600.0 2 300.0] (ev "(coverage {:UNITS 1.0 :COM 0} all opts)"))))
  (testing "2 units, and a row with no commercial column"
    (is (= [1600.0 2 300.0] (ev "(coverage {:UNITS 2} all opts)")))))

(deftest another-lot-excludes-only-the-small-shed
  (testing "3 units: only the shed is at most 120"
    (is (= [1780.0 1 120.0] (ev "(coverage {:UNITS 3.0 :COM 0} all opts)"))))
  (testing "2 units and a commercial unit"
    (is (= [1780.0 1 120.0] (ev "(coverage {:UNITS 2.0 :COM 1} all opts)"))))
  (testing "no dwelling unit, and no unit count"
    (is (= [1780.0 1 120.0] (ev "(coverage {:UNITS 0.0 :COM 0} all opts)")))
    (is (= [1780.0 1 120.0] (ev "(coverage {} all opts)")))))

(deftest a-shed-that-touches-a-building-counts
  (testing "the shed shares a wall with the house"
    (is (= [1320.0 0 0.0]
           (ev "(coverage {:UNITS 1.0}
                          [house {:geometry (rect 40 20 10 12) :building-height 9.0}]
                          opts)"))))
  (testing "a gap wider than the vertex tolerance is detached"
    (is (= [1200.0 1 96.0]
           (ev "(coverage {:UNITS 1.0}
                          [house {:geometry (rect 40.125 20 8 12) :building-height 9.0}]
                          opts)")))))

(deftest a-shed-with-two-stories-counts
  (testing "a height above :max-height"
    (is (= [1320.0 0 0.0]
           (ev "(coverage {:UNITS 1.0} [house (assoc shed :building-height 18.0)] opts)"))))
  (testing "a height equal to :max-height, and no height record"
    (is (= [1200.0 1 120.0]
           (ev "(coverage {:UNITS 1.0} [house (assoc shed :building-height 15.0)] opts)")))
    (is (= [1200.0 1 120.0]
           (ev "(coverage {:UNITS 1.0} [house (dissoc shed :building-height)] opts)"))))
  (testing ":building-height-fn of the opts names the height"
    (is (= [1320.0 0 0.0]
           (ev "(coverage {:UNITS 1.0} [house (assoc shed :HT 18.0)]
                          (assoc opts :building-height-fn (fn [b] (:HT b))))")))))

(deftest the-main-building-counts-always
  (is (= [120.0 0 0.0] (ev "(coverage {:UNITS 1.0} [shed] opts)"))))

(deftest the-whole-footprint-decides
  (testing "a 20 by 20 building with 5 by 20 on the lot counts"
    (is (= [1300.0 0 0.0]
           (ev "(coverage {:UNITS 1.0}
                          [house {:geometry (rect 45 70 20 20) :building-height 12.0}]
                          opts)"))))
  (testing "a shed across the lot line: the excluded area is the part on the lot"
    (is (= [1200.0 1 60.0]
           (ev "(coverage {:UNITS 1.0}
                          [house {:geometry (rect 45 80 10 12) :building-height 9.0}]
                          opts)")))))

(deftest thresholds-without-the-key-count-each-building
  (is (= [1900.0 0 0.0] (ev "(coverage {:UNITS 1.0 :COM 0} all plain)")))
  (testing "the row needs no unit column, and the schema no unit key"
    (is (= [1900.0 0 0.0]
           (ev "(coverage {} all {:thresholds base :schema {:parcel-id-keys [:id]}})")))))

;; The JS runtime holds a keyword as a string, therefore the key in its message
;; has no colon.
(deftest a-carve-out-without-a-key-throws-and-names-it
  (let [message (fn [expr]
                  (first (ev (str "[(util/try-catch (fn [] " expr " nil)
                                                    (fn [e] (ex-message e)))]"))))]
    (is (re-find #"the :coverage-exempt-structure of the thresholds has no :?max-height"
                 (str (message "(coverage {:UNITS 1.0} all
                                  (assoc-in opts [:thresholds :coverage-exempt-structure]
                                            (dissoc carve-out :max-height)))"))))
    (is (re-find #"the :schema of opts has no :?commercial-units-col"
                 (str (message "(coverage {:UNITS 1.0} all
                                  (assoc opts :schema {:units-col :UNITS}))"))))))

(deftest analyze-parcel-gives-the-coverage-and-what-it-excluded
  (testing "a 1-unit lot"
    (is (= [1600.0 0.32 2 300.0] (ev "(analyzed {:UNITS 1.0 :COM 0} all opts)"))))
  (testing "the same lot with 3 units"
    (is (= [1780.0 0.356 1 120.0] (ev "(analyzed {:UNITS 3.0 :COM 0} all opts)"))))
  (testing "thresholds without the key"
    (is (= [1900.0 0.38 0 0.0] (ev "(analyzed {:UNITS 1.0 :COM 0} all plain)")))))

(deftest flatten-row-gives-the-excluded-columns
  (is (= [2 300.0]
         (ev "(let [flat (parcel/flatten-row
                          {:metrics {:building-coverage-excluded-count 2
                                     :building-coverage-excluded-area 300.0}})]
                [(:building-coverage-excluded-count flat)
                 (:building-coverage-excluded-area flat)])"))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run/load-module parcel)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.parcel-coverage-test")))))
