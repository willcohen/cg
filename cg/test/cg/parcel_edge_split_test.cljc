;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parcel-edge-split-test
  "Regression cover for the abutting-split slice geometry. parcel.cg is a .cg
   module, so these go through eval-cg, the same context the backend runs.

   The invariant: a sub-edge produced by slicing must never open or close with
   a segment shorter than the length edge-junction-angle calls degenerate. When
   it did, the incident direction at a shared vertex was unusable, the junction
   angle came back nil, and a plain corner lot classified :corner-or-through.

   Slice boundaries land near vertices as the rule rather than the exception:
   the abutting condition changes where a neighbor's corner meets the frontage,
   and that corner is normally a vertex of this edge too."
  #?(:clj (:require [clojure.test :refer [deftest is use-fixtures]]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["../../src/cg/run.mjs" :as run]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private parcel "src/cg/parcel.cg")

#?(:clj (use-fixtures :once (fn [t] (run/load-module parcel) (t))))

(defn- ev [code]
  (run/eval-cg (str "(require '[cg.parcel :as parcel] '[cg.math :as math] '[cg.feature :as f])\n"
                    code)))

;; An L-shaped edge: 25 ft east, then 20 ft north. The corner vertex sits at
;; arc length 25.
(def ^:private l-coords "[[0.0 0.0] [25.0 0.0] [25.0 20.0]]")
(def ^:private l-arcs "[0.0 25.0 45.0]")

;; snap-tol 0.1 and dedup-tol 1e-3, as :slice-vertex-snap and :slice-dedup
;; of a thresholds map in feet.
(deftest no-slice-across-a-vertex-emits-a-degenerate-segment
  (let [gaps (ev (str "
(let [spacings (fn [pts]
                 (mapv (fn [i]
                         (let [[x1 y1] (nth pts i)
                               [x2 y2] (nth pts (inc i))]
                           (math/sqrt (+ (* (- x2 x1) (- x2 x1))
                                            (* (- y2 y1) (- y2 y1))))))
                       (range (dec (count pts)))))]
  (vec (for [off [-0.05 -0.01 -0.001 -0.0001 0.0 0.0001 0.001 0.01 0.05]]
         (apply min (spacings (parcel/arc-slice-coords " l-coords " " l-arcs "
                                                        0.0 (+ 25.0 off) 0.1 1.0e-3))))))"))]
    (is (= 9 (count gaps)))
    (is (every? #(> % 1.0e-3) gaps)
        (str "every sliced sub-edge clears the degeneracy threshold; got " gaps))))

;; On a closed ring coords[n] is coords[0]. An edge across the seam must not
;; repeat that point: a zero-length segment gives a nil junction angle and
;; turns a corner lot :corner-or-through.
(def ^:private square-ring
  "[[0.0 0.0] [10.0 0.0] [10.0 10.0] [0.0 10.0] [0.0 0.0]]")
(def ^:private square-corners
  "[{:type :vertex :idx 0} {:type :vertex :idx 1}
    {:type :vertex :idx 2} {:type :vertex :idx 3}]")

(deftest ring-closure-edge-carries-no-repeated-point
  (let [edges (ev (str "(parcel/corners->edge-coords " square-ring " "
                       square-corners ")"))]
    (is (= 4 (count edges)))
    (is (= [[0.0 10.0] [0.0 0.0]] (last edges)))
    (is (every? (fn [e] (= (count e) (count (dedupe e)))) edges)
        (str "edges: " edges))))

(deftest ring-closure-edge-yields-a-measurable-junction-angle
  (let [interior (ev (str "
(let [edges (mapv f/linestring
                  (parcel/corners->edge-coords " square-ring " " square-corners "))]
  (:interior (parcel/edge-junction-angle (last edges) (first edges) 1.0e-3 true)))"))]
    (is (< 89.0 interior 91.0) (str "square corner should read ~90, got " interior))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run/load-module parcel)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.parcel-edge-split-test")))))
