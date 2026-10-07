;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.def-events-test
  "run-file emits def-start, then def-complete or def-error, for each
   top-level def of the file. The pid is binding-<name>, the id of the loader
   card of the def. Other evaluations emit no def events."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [clojure.core.async :as a]
                    [cg.run :as run]
                    [cg.trace :as trace])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/trace_js.mjs" :as trace]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private two-defs
  "(require '[cg.feature :as f])
(require '[cg.geo :as geo])
(require '[cg.macros :refer [item->]])
(def pts [(f/point 0 0)])
(def buffered (item-> :pts (geo/buffer 1)))")

(def ^:private two-defs-in-a-module
  "(ns def-events-module
  (:require [cg.feature :as f]
            [cg.geo :as geo]
            [cg.macros :refer [item->]]))

(def pts [(f/point 0 0)])
(def buffered (item-> :pts (geo/buffer 1)))")

(def ^:private failing-def
  "(def ok 1)
(def bad (throw (ex-info \"no data\" {})))")

(def ^:private two-defs-events
  [["def-start" "binding-pts"] ["def-complete" "binding-pts"]
   ["def-start" "binding-buffered"] ["def-complete" "binding-buffered"]])

(defn- def-events
  "[kind pid] of each def event, in order."
  [events]
  (->> events
       (filter (fn [e] (contains? #{"def-start" "def-complete" "def-error"} (:kind e))))
       (mapv (fn [e] [(:kind e) (:pid e)]))))

#?(:clj
   (defn- collect-events
     "Run f with a tap on the trace stream, then drain what it emitted."
     [f]
     (let [ch (a/chan 1024)]
       (trace/tap-trace ch)
       (try
         (try (f) (catch Exception _ nil))
         (Thread/sleep 150)
         (loop [acc []]
           (if-let [e (a/poll! ch)] (recur (conj acc e)) acc))
         (finally
           (trace/untap-trace ch))))))

#?(:cljs
   (defn ^:async collect-events
     "Resolve to the events that the Promise of (f) emitted. A rejection
      counts as an end."
     [f]
     (let [events (atom [])
           listener (trace/add-trace-listener (fn [e] (swap! events conj e)))]
       (try
         (await (.catch (f) (fn [_] nil)))
         @events
         (finally
           (trace/remove-trace-listener listener))))))

#?(:cljs
   (do
     (defn ^:async run-two-defs! []
       (is (= two-defs-events
              (def-events (await (collect-events #(run/run-file two-defs {:model "buffered"})))))))

     (defn ^:async run-module! []
       (is (= two-defs-events
              (def-events (await (collect-events
                                  #(run/run-file two-defs-in-a-module {:model "buffered"})))))))

     (defn ^:async run-failing! []
       (is (= [["def-start" "binding-ok"] ["def-complete" "binding-ok"]
               ["def-start" "binding-bad"] ["def-error" "binding-bad"]]
              (def-events (await (collect-events #(run/run-file failing-def)))))))

     (defn ^:async run-eval-only! []
       (is (= [] (def-events (await (collect-events
                                     #(js/Promise.resolve (run/eval-cg "(def z 1)"))))))))))

(deftest each-def-of-a-run-emits-start-and-complete
  #?(:clj (is (= two-defs-events
                 (def-events (collect-events #(run/run-file two-defs {:model "buffered"})))))
     :cljs (run-two-defs!)))

(deftest the-defs-of-a-module-emit-their-events
  #?(:clj (is (= two-defs-events
                 (def-events (collect-events
                              #(run/run-file two-defs-in-a-module {:model "buffered"})))))
     :cljs (run-module!)))

(deftest a-failed-def-emits-def-error
  #?(:clj (is (= [["def-start" "binding-ok"] ["def-complete" "binding-ok"]
                  ["def-start" "binding-bad"] ["def-error" "binding-bad"]]
                 (def-events (collect-events #(run/run-file failing-def)))))
     :cljs (run-failing!)))

(deftest eval-cg-emits-no-def-events
  (testing "only the run of a file lights the loader cards"
    #?(:clj (is (= [] (def-events (collect-events #(run/eval-cg "(def z 1)")))))
       :cljs (run-eval-only!))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.def-events-test")))))
