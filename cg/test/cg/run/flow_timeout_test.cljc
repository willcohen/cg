;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.flow-timeout-test
  "A run-flow that times out stops its flow, so that its procs do not run on
   after the caller gets the error."
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [cg.run.flow :as cg-flow]
                    [cg.test-runner :refer [after]])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["../../../src/cg/run/flow.mjs" :as cg-flow]
                     ["../test_runner.mjs" :refer [run_tests_and_exit_BANG_ after]])))

#?(:clj (set! *warn-on-reflection* true))

(defn- recording-step
  "A step with the describe map `desc` that never sends a message. On the
   stop transition, it adds `pid` to the map in `stopped`."
  [desc stopped pid]
  (fn
    ([] desc)
    ([_args] {})
    ([state transition]
     (when (= transition :clojure.core.async.flow/stop)
       (swap! stopped assoc pid true))
     state)
    ([state _in _msg] [state {}])))

(defn- never-done-flow
  "A flow-def for run-flow whose source sends nothing, so that the flow
   never completes."
  [stopped]
  {:config {:procs {:src {:proc (cg-flow/make-process
                                 (recording-step {:outs {:out "Nothing"}} stopped :src))
                          :args {}}
                    :sink {:proc (cg-flow/make-process
                                  (recording-step {:ins {:in "Nothing"}} stopped :sink))
                           :args {}}}
            :conns [[[:src :out] [:sink :in]]]}
   :result-atom (atom nil)
   :completed (atom false)})

(def ^:private stop-wait-ms 2000)

(defn- check-timeout-stop [e stopped]
  (is (= :timeout (:type (ex-data e))) "the caller gets the timeout error")
  (is (contains? stopped :src) "the source got the stop transition")
  (is (contains? stopped :sink) "the sink got the stop transition"))

#?(:clj
   (defn- wait-for-stops [stopped]
     (let [deadline (+ (System/currentTimeMillis) stop-wait-ms)]
       (loop []
         (when (and (< (count @stopped) 2)
                    (< (System/currentTimeMillis) deadline))
           (Thread/sleep 20)
           (recur))))))

#?(:cljs
   (do
     (defn- sleep [ms]
       (js/Promise. (fn [resolve] (js/setTimeout resolve ms))))

     (defn ^:async wait-for-stops [stopped]
       (let [deadline (+ (js/Date.now) stop-wait-ms)]
         (while (and (< (count @stopped) 2)
                     (< (js/Date.now) deadline))
           (await (sleep 20)))))))

(defn- timeout-error [stopped]
  #?(:clj (try (cg-flow/run-flow (never-done-flow stopped) {:timeout-ms 200}) nil
               (catch clojure.lang.ExceptionInfo e e))
     :cljs (.then (cg-flow/run-flow (never-done-flow stopped) {:timeout-ms 200})
                  (fn [_] nil) identity)))

(deftest a-flow-that-times-out-stops
  (let [stopped (atom {})]
    (after (timeout-error stopped)
           (fn [e]
             (after (wait-for-stops stopped) (fn [_] (check-timeout-stop e @stopped)))))))

#?(:cljs (run_tests_and_exit_BANG_ "cg.run.flow-timeout-test"))
