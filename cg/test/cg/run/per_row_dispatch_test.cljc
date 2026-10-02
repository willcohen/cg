;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.per-row-dispatch-test
  "End-to-end tests for per-row operations in :table-level mode.

   Verifies the dispatch path that unwraps a batch, iterates its rows,
   applies a per-row operation to each row-map, and rebuilds the dataset.
   Foundation for the fusion path: a fusion group of length 1 must work
   before chains of 2+ can collapse onto it.

   ONE .cljc suite covers both runtimes, but each runtime registers its
   operation its own way. The JVM registers a host defn with ^:op through
   run/register-op!, as eval-cg does for a .cg op. cljs uses the public
   extension API: feature/register-operation! for the shape, then
   run/register-namespace! for the code. The dispatch under test, and the
   assertions on the rows, are the same. The per-geometry shape has no JVM
   twin here, so it runs on cljs only."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.run :as run]
                    cg.feature
                    [cg.macros :refer [table->]]
                    [tech.v3.dataset :as ds])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../../src/cg/run.mjs" :as run]
                     ["../../../src/cg/dataset.mjs" :as dataset]
                     ["../../../src/cg/feature.mjs" :as feature]
                     ["../wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- check-doubled-rows!
  "Each row keeps its :n and gains a :doubled of twice that value."
  [rows]
  (is (= 4 (count rows)) "all 4 rows pass through")
  (is (= [2 4 6 8] (vec (mapv :doubled rows)))
      "each row :doubled is 2x the input :n")
  (is (= [1 2 3 4] (vec (mapv :n rows))) ":n preserved on the row"))

#?(:clj
   (do
     (defn- register-op! [v]
       (run/register-op! (str (:ns (meta v))) (run/var-op-decl v) @v))

     (defn ^:op tdt-double-n
       {:row-reads #{:n} :row-writes #{:doubled}}
       [^:row parcel-row]
       (assoc! parcel-row :doubled (* 2 (:n parcel-row))))
     (register-op! #'tdt-double-n)

     (defn ^:op tdt-incr-doubled
       {:row-reads #{:doubled} :row-writes #{:doubled}}
       [^:row parcel-row]
       (assoc! parcel-row :doubled (inc (:doubled parcel-row))))
     (register-op! #'tdt-incr-doubled)

     (deftest per-row-op-runs-in-table-mode
       (testing "a single per-row op inside table-> sees each row as a map"
         (let [d (ds/->>dataset [{:n 1} {:n 2} {:n 3} {:n 4}])
               model (table-> :input (tdt-double-n))
               result (run/run-model model {:input d} {:timeout-ms 10000})]
           (check-doubled-rows! (ds/rows result :as-maps)))))

     (deftest per-row-op-chain-fuses-in-table-mode
       (testing "a 2-op per-row chain in table-> compiles to ONE fused proc + applies both fns per row"
         (let [d (ds/->>dataset [{:n 1} {:n 2} {:n 3} {:n 4}])
               model (table-> :input (tdt-double-n) (tdt-incr-doubled))
               ;; Inspect the flow config to assert fusion collapsed the two
               ;; per-row operation nodes into a single proc. Source + sink
               ;; stay separate; before fusion we'd have 4 procs (src + 2 ops
               ;; + sink); with fusion the two ops collapse → 3 procs.
               flow-cfg (run/model->flow model {:input d} {})
               op-procs (filter (fn [[pid _]]
                                  (and (not= pid :input-src)
                                       (not= pid :result)))
                                (:procs (:config flow-cfg)))
               result (run/run-model model {:input d} {:timeout-ms 10000})
               rows (ds/rows result :as-maps)]
           (is (= 1 (count op-procs))
               (str "expected exactly 1 operation proc after fusion, got: "
                    (mapv first op-procs)))
           (is (= 4 (count rows)) "all 4 rows pass through")
           (is (= [3 5 7 9] (mapv :doubled rows))
               ":doubled = 2n + 1 after both ops apply"))))))

;; resolve-operation qualifies a bare namespace to "cg.<ns>", so the probe ops
;; are registered under "cg.probe". squint munges a name with a dash, so the
;; namespace object holds identity_geom and double_n.

#?(:cljs
   (do
     (def ^:private geometry-op-calls (atom 0))

     (defn- register-probe-ops! []
       (feature/register-operation! "cg.probe/identity-geom"
                                    {:type "transformer" :row-shape "per-geometry"})
       (feature/register-operation! "cg.probe/double-n"
                                    {:type "transformer" :row-shape "per-row"})
       (run/register-namespace!
        "cg.probe"
        #js {:identity_geom (fn [g] (swap! geometry-op-calls inc) g)
             :double_n (fn [row] (aset row "doubled" (* 2 (:n row))) row)}))

     (defn- table-model [op-fn]
       {:nodes [{:type :source :id :input-src :data-ref :input :schema {}}
                {:type :operation :id :op-1 :fn op-fn :params {}
                 :mode :table :config {}}
                {:type :sink :id :result :output-binding :result}]
        :edges [{:from :input-src :to :op-1 :type :data-flow}
                {:from :op-1 :to :result :type :data-flow}]
        :config {:platform "javascript"}})

     (defn- result-rows [result]
       (if (array? result) result (dataset/rows result)))

     (defn ^:async run-per-geometry! []
       (await (dataset/init-arquero!))
       (register-probe-ops!)
       (let [fc {:type "FeatureCollection"
                 :features [{:type "Feature" :properties {:n 1}
                             :geometry {:type "Point" :coordinates [0 0]}}
                            {:type "Feature" :properties {:n 2}
                             :geometry {:type "Point" :coordinates [1 1]}}
                            {:type "Feature" :properties {:n 3}
                             :geometry {:type "Point" :coordinates [2 2]}}]}
             d (dataset/geojson->dataset (js/JSON.stringify fc))
             before @geometry-op-calls
             result (await (run/run-model (table-model "cg.probe/identity-geom")
                                          {:input d} {:timeout-ms 10000}))
             rows (result-rows result)]
         (is (= 3 (count rows)) "all 3 rows pass through")
         (is (= 3 (- @geometry-op-calls before)) "the op runs once per geometry")
         (is (every? (fn [r] (some? (:geometry r))) rows)
             "every row keeps its geometry")))

     (defn ^:async run-per-row! []
       (await (dataset/init-arquero!))
       (register-probe-ops!)
       (let [d (dataset/->dataset {:n [1 2 3 4]})
             result (await (run/run-model (table-model "cg.probe/double-n")
                                          {:input d} {:timeout-ms 10000}))]
         (check-doubled-rows! (result-rows result))))

     (deftest per-row-op-runs-in-table-mode
       (testing "a registered per-row op sees each row as a map"
         (run-per-row!)))

     (deftest per-geometry-op-maps-over-the-geometry-column
       (testing "a registered per-geometry op maps over the geometry column"
         (run-per-geometry!)))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.run.per-row-dispatch-test")))))
