;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.per-row-dispatch-test
  "End-to-end tests for per-row operations in :table-level mode.

   Verifies the dispatch path that unwraps a batch, iterates its rows,
   applies a per-row operation to each row-map, and rebuilds the dataset.

   ONE .cljc suite covers both runtimes, but each runtime registers its
   operation its own way. The JVM registers a host defn with ^:op through
   run/register-op!, as eval-cg does for a .cg op. cljs uses the public
   extension API: feature/register-operation! for the shape, then
   run/register-namespace! for the code. The dispatch under test, and the
   assertions on the rows, are the same. The per-geometry shape has no JVM
   twin here, so it runs on cljs only."
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [cg.run :as run]
                    [cg.run.fusion :as fusion]
                    [cg.trace :as trace]
                    [cg.feature :as feature]
                    [cg.macros :refer [table->]]
                    [clojure.core.async :as a]
                    [tech.v3.dataset :as ds])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["../../../src/cg/run.mjs" :as run]
                     ["../../../src/cg/run/fusion.mjs" :as fusion]
                     ["../../../src/cg/dataset.mjs" :as dataset]
                     ["../../../src/cg/feature.mjs" :as feature]
                     ["../../../src/cg/trace_js.mjs" :as trace]
                     ["../wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- check-doubled-rows! [rows]
  (is (= [2 4 6 8] (vec (mapv :doubled rows))))
  (is (= [1 2 3 4] (vec (mapv :n rows)))))

(defn- check-row-errors!
  "The flow-complete event counts the rows that a throwing per-row op left
   with :error, because the run goes on and gives no other sign of them."
  [events]
  (let [done (first (filter (fn [e] (= "flow-complete" (:kind e))) events))]
    (is (= 2 (:failed-rows done)))
    (is (= 4 (:total-rows done)))
    (is (= "even n" (:first-error done)))))

;; On JS, a symbol or a keyword compiles to a string, and squint keyword? is
;; true for each string.
(deftest node->op-key-gives-the-registry-key-of-each-op-ref
  (feature/register-bare-op-namespace! "key-probe" "cg.probe")
  (doseq [op-ref ['cg.probe/key-probe :cg.probe/key-probe "cg.probe/key-probe" "key-probe"]]
    (is (= "cg.probe/key-probe" (fusion/node->op-key {:fn op-ref})) (pr-str op-ref))))

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

     (defn ^:op tdt-fail-even
       {:row-reads #{:n} :row-writes #{:doubled}}
       [^:row parcel-row]
       (if (even? (:n parcel-row))
         (throw (ex-info "even n" {}))
         (assoc! parcel-row :doubled (:n parcel-row))))
     (register-op! #'tdt-fail-even)

     (deftest a-run-over-no-rows-ends-at-once
       (let [result (run/run-model (table-> :input (tdt-double-n))
                                   {:input (ds/->>dataset [])} {:timeout-ms 3000})]
         (is (zero? (ds/row-count result)))))

     (deftest a-run-counts-the-rows-that-failed
       (let [ch (a/chan 1024)
             d (ds/->>dataset [{:n 1} {:n 2} {:n 3} {:n 4}])]
         (trace/tap-trace ch)
         (try
           (run/run-model (table-> :input (tdt-fail-even)) {:input d} {:timeout-ms 10000})
           (Thread/sleep 150)
           (check-row-errors! (loop [acc []]
                                (if-let [e (a/poll! ch)] (recur (conj acc e)) acc)))
           (finally (trace/untap-trace ch)))))

     (deftest per-row-op-runs-in-table-mode
       (let [d (ds/->>dataset [{:n 1} {:n 2} {:n 3} {:n 4}])
             model (table-> :input (tdt-double-n))
             result (run/run-model model {:input d} {:timeout-ms 10000})]
         (check-doubled-rows! (ds/rows result :as-maps))))

     (deftest per-row-op-chain-fuses-in-table-mode
       (let [d (ds/->>dataset [{:n 1} {:n 2} {:n 3} {:n 4}])
             model (table-> :input (tdt-double-n) (tdt-incr-doubled))
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
         (is (= [3 5 7 9] (mapv :doubled rows))
             ":doubled = 2n + 1 after both ops apply")))))

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
       (feature/register-operation! "cg.probe/fail-even"
                                    {:type "transformer" :row-shape "per-row"})
       (run/register-namespace!
        "cg.probe"
        #js {:identity_geom (fn [g] (swap! geometry-op-calls inc) g)
             :double_n (fn [row] (aset row "doubled" (* 2 (:n row))) row)
             :fail_even (fn [row]
                          (if (even? (:n row))
                            (throw (js/Error. "even n"))
                            (do (aset row "doubled" (:n row)) row)))}))

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
         (is (= 3 (count rows)))
         (is (= 3 (- @geometry-op-calls before)) "the op runs once per geometry")
         (is (every? (fn [r] (some? (:geometry r))) rows))))

     (defn ^:async run-per-row! []
       (register-probe-ops!)
       (let [d (dataset/->dataset {:n [1 2 3 4]})
             result (await (run/run-model (table-model "cg.probe/double-n")
                                          {:input d} {:timeout-ms 10000}))]
         (check-doubled-rows! (result-rows result))))

     (defn ^:async run-row-errors! []
       (register-probe-ops!)
       (let [events #js []
             l (trace/add-trace-listener (fn [e] (.push events e)))]
         (try
           (await (run/run-model (table-model "cg.probe/fail-even")
                                 {:input (dataset/->dataset {:n [1 2 3 4]})}
                                 {:timeout-ms 10000}))
           (finally (trace/remove-trace-listener l)))
         (check-row-errors! (vec events))))

     (deftest per-row-op-runs-in-table-mode
       (run-per-row!))

     (deftest a-run-counts-the-rows-that-failed
       (run-row-errors!))

     (defn ^:async run-no-rows! []
       (register-probe-ops!)
       (let [result (await (run/run-model (table-model "cg.probe/double-n")
                                          {:input (dataset/->dataset [])}
                                          {:timeout-ms 3000}))]
         (is (zero? (count (result-rows result))))))

     (deftest a-run-over-no-rows-ends-at-once
       (run-no-rows!))

     (deftest per-geometry-op-maps-over-the-geometry-column
       (run-per-geometry!))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (dataset/init-arquero!)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.run.per-row-dispatch-test")))))
