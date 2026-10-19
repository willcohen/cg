;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; A run-model call emits the same lifecycle events, with the same wire
;; fields, through cg.trace (a channel) and cg.trace-js (listeners).
(ns cg.trace-test
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [cg.trace :as trace]
                    [cg.run :as run]
                    [cg.feature :as feature]
                    [cg.test-runner :refer [after collect-events]])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["../../src/cg/trace_js.mjs" :as trace]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_ after]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private buffer-model
  {:nodes [{:type :source :id :data-src :data-ref :data}
           {:type :operation :id :buffer-1 :fn "cg.geo/buffer"
            :params {:distance 5} :execution :item-level}
           {:type :sink :id :result :output-binding :result}]
   :edges [{:from :data-src :to :buffer-1 :type :data-flow}
           {:from :buffer-1 :to :result :type :data-flow}]
   :config {}})

(defn- two-points [] [(feature/point 0 0) (feature/point 10 10)])

#?(:cljs
   (defn ^:async collect-events [f]
     (let [events #js []
           l (trace/add-trace-listener (fn [e] (.push events e)))]
       (try
         (await (f))
         (finally
           (trace/remove-trace-listener l)))
       events)))

(defn- of-kind [events k]
  (vec (filter (fn [e] (= k (:kind e))) events)))

(defn- check-lifecycle! [events]
  (let [kinds (mapv :kind events)]
    (is (some? (first (filter (fn [k] (= "proc-start" k)) kinds)))
        "proc-start is emitted")
    (is (= "flow-complete" (last kinds)) "flow-complete is the last event")))

(defn- check-pids! [events]
  (let [flow-start (nth (of-kind events "flow-start") 0)
        proc-starts (of-kind events "proc-start")]
    (is (= "traced" (:flow flow-start)) "opts :name becomes the flow name")
    (is (= ["traced__buffer-1"] (:pids flow-start))
        "flow-start lists the operation pid")
    ;; The canvas expects <model>__<node-id>. A pid that loses its prefix
    ;; cannot be matched to a node on the canvas.
    (is (every? (fn [e] (= "traced__buffer-1" (:pid e))) proc-starts)
        "each proc pid follows the <model>__<node-id> convention")
    (is (= (count proc-starts) (count (of-kind events "proc-complete")))
        "each proc-start has a proc-complete")))

(defn- check-wire-shape! [events]
  (is (every? #(= 1 (:v %)) events))
  (is (every? #(number? (:seq %)) events))
  (is (every? #(number? (:ts %)) events))
  (is (every? #(string? (:kind %)) events))
  (is (every? #(string? (:worker-id %)) events))
  (let [seqs (mapv :seq events)]
    (is (every? (fn [i] (or (= 0 i) (> (nth seqs i) (nth seqs (dec i)))))
                (range (count seqs)))
        "seq increases with every event")))

(deftest run-model-emits-the-flow-lifecycle
  (after (collect-events #(run/run-model buffer-model {:data (two-points)}
                                         {:name "traced" :timeout-ms 5000}))
         (fn [events]
           (check-lifecycle! events)
           (check-pids! events)
           (check-wire-shape! events))))

(deftest run-model-without-a-name-emits-anon-pids
  (after (collect-events #(run/run-model buffer-model {:data [(feature/point 0 0)]}
                                         {:timeout-ms 5000}))
         (fn [events]
           (let [proc-start (nth (of-kind events "proc-start") 0)]
             (is (= "anon__buffer-1" (:pid proc-start))
                 "a run without a name gets the anon prefix")))))

#?(:cljs
   (deftest listeners-are-isolated-and-removable
     ;; cljs only: the JVM producer has no listener registry. Its analogue is
     ;; untap-trace, which cg.test-runner/collect-events uses.
     (let [events #js []
           bad (trace/add-trace-listener (fn [] (throw (js/Error. "bad listener"))))
           good (trace/add-trace-listener (fn [e] (.push events e)))]
       (trace/emit! "probe" {})
       (trace/remove-trace-listener bad)
       (trace/remove-trace-listener good)
       (is (< 0 (count events)) "a throwing listener does not stop the others")
       (let [before (count events)]
         (trace/emit! "post-remove" {})
         (is (= before (count events)) "a removed listener receives nothing")))))

#?(:clj
   (deftest emit-includes-worker-id
     (let [evt (first (collect-events #(trace/emit! "test-worker-id" {:hello "world"})))]
       (is (= "test-worker-id" (:kind evt)))
       (is (= (.getName (Thread/currentThread)) (:worker-id evt))
           "worker-id matches the thread that called emit!"))))

#?(:clj
   (deftest worker-busy-start-end
     (let [result (atom nil)
           evts  (collect-events
                  #(reset! result (trace/with-worker-busy {:task "test" :seq 42}
                                    (Thread/sleep 5)
                                    :work-done)))
           start (first (of-kind evts "worker-busy-start"))
           end   (first (of-kind evts "worker-busy-end"))]
       (is (= :work-done @result) "with-worker-busy returns the body's value")
       (is (= 42 (:seq start)) "tag :seq carried through to start")
       (is (= "test" (:task start)))
       (is (= "test" (:task end)))
       (is (>= (- (:ts end) (:ts start)) 5)
           "end timestamp is at least 5ms after start (sleep)"))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.trace-test")))))
