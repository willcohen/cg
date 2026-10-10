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
                    [cg.run :as run]
                    [cg.test-runner :as runner :refer [after]])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/trace_js.mjs" :as trace]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_ after]])))

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
     "cg.test-runner/collect-events, but a throw of `f` counts as an end."
     [f]
     (runner/collect-events #(try (f) (catch Exception _ nil)))))

#?(:cljs
   (defn ^:async collect-events
     "Resolve to the events that (f) emitted. f can give a Promise. A throw
      or a rejection counts as an end."
     [f]
     (let [events (atom [])
           listener (trace/add-trace-listener (fn [e] (swap! events conj e)))]
       (try
         (try (await (f)) (catch :default _ nil))
         @events
         (finally
           (trace/remove-trace-listener listener))))))

(deftest each-def-of-a-run-emits-start-and-complete
  (after (collect-events #(run/run-file two-defs {:model "buffered"}))
         #(is (= two-defs-events (def-events %)))))

(deftest the-defs-of-a-module-emit-their-events
  (after (collect-events #(run/run-file two-defs-in-a-module {:model "buffered"}))
         #(is (= two-defs-events (def-events %)))))

(deftest a-failed-def-emits-def-error
  (after (collect-events #(run/run-file failing-def))
         #(is (= [["def-start" "binding-ok"] ["def-complete" "binding-ok"]
                  ["def-start" "binding-bad"] ["def-error" "binding-bad"]]
                 (def-events %)))))

;; A line of a string that starts with ( is not a form. The file has no
;; model, so run-file throws after the eval.
(deftest a-string-line-that-starts-with-a-paren-keeps-its-text
  (after (collect-events #(run/run-file "(def wkt \"A\n(B)\")"))
         (fn [events]
           (is (= [["def-start" "binding-wkt"] ["def-complete" "binding-wkt"]]
                  (def-events events)))
           (is (= "A\n(B)" (run/eval-cg "wkt"))))))

(deftest eval-cg-emits-no-def-events
  (testing "only the run of a file lights the loader cards"
    (after (collect-events #(run/eval-cg "(def z 1)"))
           #(is (= [] (def-events %))))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.def-events-test")))))
