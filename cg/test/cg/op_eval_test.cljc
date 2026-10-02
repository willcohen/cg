;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; An op defined through cg.run/eval-cg runs on both runtimes: the JVM
;; registers it from its var, the JS runtime from the parser decl. A direct call
;; of a row op gives a value, not a Promise.
(ns cg.op-eval-test
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

;; measure-parcel as test/fixtures/typed-clean.cg declares it.
(def ^:private measure-parcel-src
  "(require '[cg.macros :refer [item->]])
   (require '[cg.geo :as geo])
   (defn ^:op measure-parcel
     {:outputs {:area {:type :number}}}
     [^:row ^{:type :feature} parcel]
     (geo/area parcel))")

(def ^:private call-src
  "(require '[cg.feature :as f])
   (measure-parcel (f/polygon [[0 0] [10 0] [10 10] [0 10] [0 0]]))")

(def ^:private item-rung-src
  (str measure-parcel-src
       " (item-> :squares (measure-parcel))"))

;; A row op as a table-> rung: it gets each row, as apply-row-chain gives
;; it (a transient on the JVM).
(def ^:private table-rung-src
  "(require '[cg.macros :refer [table->]])
   (require '[cg.geo :as geo])
   (defn ^:op tag-area [^:row row]
     (assoc! row :area (geo/area (get row :geometry))))
   (table-> :rows (tag-area))")

(def ^:private plain-body-src
  "(defn ^:op tag-one [^:row row]
     (assoc row :tagged true))")

(defn- square [side]
  (feature/polygon [[0 0] [side 0] [side side] [0 side] [0 0]]))

(defn- check-area! [area]
  (is (= (geo/area (square 10)) area)
      "the op gives the area that geo/area gives"))

(defn- check-rung-areas! [result]
  (is (= [(geo/area (square 10)) (geo/area (square 20))] (vec result))
      "each item gets the op's area, in order"))

(defn- table-rows [d]
  (ds/rows (if (sequential? d) (first d) d) :as-maps))

(defn- check-tagged-rows! [result]
  (is (= [[1 (geo/area (square 10))] [2 (geo/area (square 20))]]
         (mapv (fn [r] [(get r :id) (get r :area)]) (table-rows result)))
      "each row gets its :area from the op, in order"))

(defn- rows-dataset []
  (ds/maps->dataset [{:id 1 :geometry (square 10)} {:id 2 :geometry (square 20)}]))

#?(:cljs
   (do
     (defn ^:async run-item-rung! []
       (check-rung-areas!
        (await (run/run-model (run/eval-cg item-rung-src)
                              {:squares [(square 10) (square 20)]} {}))))

     (defn ^:async run-table-rung! []
       (check-tagged-rows!
        (await (run/run-model (run/eval-cg table-rung-src)
                              {:rows (rows-dataset)} {}))))))

(deftest an-op-defined-through-eval-cg-gives-its-value
  (testing "measure-parcel from typed-clean.cg, on one 10 x 10 square"
    (run/eval-cg measure-parcel-src)
    (check-area! (run/eval-cg call-src))))

(deftest an-op-as-an-item-rung
  (testing "(item-> :squares (measure-parcel)) over two squares"
    #?(:clj (check-rung-areas!
             (run/run-model (run/eval-cg item-rung-src)
                            {:squares [(square 10) (square 20)]} {}))
       :cljs (run-item-rung!))))

(deftest an-op-as-a-table-rung
  (testing "(table-> :rows (tag-area)) over two rows"
    #?(:clj (check-tagged-rows!
             (run/run-model (run/eval-cg table-rung-src)
                            {:rows (rows-dataset)} {}))
       :cljs (run-table-rung!))))

(deftest an-op-with-a-map-arg-gives-its-value
  (testing "(tag-one {:a 1}) is the row, not a Promise of it"
    (run/eval-cg plain-body-src)
    (is (= true (get (run/eval-cg "(tag-one {:a 1})") :tagged)))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (parser/init! nil)))
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.op-eval-test")))))
