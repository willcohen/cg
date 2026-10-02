;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.rung-args-test
  "A rung argument that is not a literal evaluates where the threading
   macro is, on both runtimes: a def, a let local, a call, and a param of the
   table op whose body is the model. The zoning chain passes its layers
   this way, (cg.parcel/analyze-parcel parcels buildings opts). A
   literal map is a map value on both runtimes, as
   (determine-effective-boundary {}) needs."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.run :as run]
                    [cg.geo :as geo]
                    [cg.dataset :as ds]
                    [cg.feature :as feature])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/parser/core.mjs" :as parser]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private prelude
  "(require '[cg.macros :refer [item->]])
   (require '[cg.geo :as geo])
   (require '[cg.feature :as f])")

(def ^:private def-src
  (str prelude
       " (def rung-d 5)
         (item-> :squares (geo/buffer rung-d) (geo/area))"))

(def ^:private let-src
  (str prelude
       " (let [rung-d 5]
           (item-> :squares (geo/buffer rung-d) (geo/area)))"))

(def ^:private call-src
  (str prelude
       " (def rung-half 2.5)
         (item-> :squares (geo/buffer (* 2 rung-half)) (geo/area))"))

(def ^:private op-src
  (str prelude
       " (defn ^:op buffered-areas [^:table parcels d]
           (item-> :parcels (geo/buffer d) (geo/area)))"))

;; A table op returns its model. The model holds d, the arg of the call.
(def ^:private op-call-src
  "(buffered-areas nil 5)")

;; The op reads the map it gets after an assoc, as find-corners does with
;; (assoc opts :rect rect).
(def ^:private map-arg-src
  "(require '[cg.macros :refer [table->]])
   (defn ^:op tag-from-opts [^:row row opts]
     (assoc! row :k (get (assoc opts :extra 1) :k)))
   (table-> :rows (tag-from-opts {:k 7}))")

(defn- square []
  (feature/polygon [[0 0] [10 0] [10 10] [0 10] [0 0]]))

(defn- expected-area []
  (geo/area (geo/buffer (square) 5)))

(defn- check-area! [what result]
  (is (= [(expected-area)] (vec result))
      (str what ": the rung gets 5, as geo/buffer on the square gives")))

(defn- rows-dataset []
  (ds/maps->dataset [{:id 1 :geometry (square)} {:id 2 :geometry (square)}]))

(defn- check-map-arg! [result]
  (is (= [7 7] (mapv (fn [r] (get r :k))
                     (ds/rows (if (sequential? result) (first result) result) :as-maps)))
      "the op reads :k from the literal map {:k 7}"))

#?(:cljs
   (do
     (defn ^:async run-map-arg! []
       (check-map-arg! (await (run/run-model (run/eval-cg map-arg-src)
                                             {:rows (rows-dataset)} {}))))

     (defn ^:async run-src! [src]
       (await (run/run-model (run/eval-cg src) {:squares [(square)]} {})))

     (defn ^:async run-all! []
       (check-area! "a def" (await (run-src! def-src)))
       (check-area! "a let local" (await (run-src! let-src)))
       (check-area! "a call" (await (run-src! call-src)))
       (run/eval-cg op-src)
       (check-area! "a param of a table op"
                    (await (run/run-model (run/eval-cg op-call-src)
                                          {:parcels [(square)]} {}))))))

(deftest a-rung-argument-evaluates-where-the-macro-is
  (testing "a def, a let local, a call and a table op param as geo/buffer's distance"
    #?(:clj (let [run-src (fn [src] (run/run-model (run/eval-cg src) {:squares [(square)]} {}))]
              (check-area! "a def" (run-src def-src))
              (check-area! "a let local" (run-src let-src))
              (check-area! "a call" (run-src call-src))
              (run/eval-cg op-src)
              (check-area! "a param of a table op"
                           (run/run-model (run/eval-cg op-call-src)
                                          {:parcels [(square)]} {})))
       :cljs (run-all!))))

(deftest a-literal-map-argument-is-a-map
  (testing "(tag-from-opts {:k 7}) as a table-> rung"
    #?(:clj (check-map-arg! (run/run-model (run/eval-cg map-arg-src)
                                           {:rows (rows-dataset)} {}))
       :cljs (run-map-arg!))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (parser/init! nil)))
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.rung-args-test")))))
