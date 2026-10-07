;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.trace-state-test
  "The state and the progress that a trace event gives a card of the canvas.
   A def event lights the loader card of the def, and its rows have no total."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_gui/trace_state.mjs" :as ts]))

(deftest a-def-event-sets-the-state-of-its-card
  (is (= "running" (ts/node-state "def-start")))
  (is (= "complete" (ts/node-state "def-complete")))
  (is (= "error" (ts/node-state "def-error"))))

(deftest a-proc-event-sets-the-state-of-its-card
  (is (= "running" (ts/node-state "proc-start")))
  (is (= "complete" (ts/node-state "proc-complete")))
  (is (= "error" (ts/node-state "proc-error")))
  (is (nil? (ts/node-state "flow-start"))))

(deftest def-progress-gives-a-row-count-with-no-total
  (let [p (ts/node-progress #js {:kind "def-progress" :pid "binding-parcels" :done 2000})]
    (is (= 2000 (.-done p)))
    (is (nil? (.-total p)))
    (is (= "2,000 rows" (ts/progress-text p)))
    (is (nil? (ts/progress-pct p)) "no bar without a total")))

(deftest rows-progress-gives-done-of-total
  (let [p (ts/node-progress #js {:kind "rows-progress" :done 250 :total 1000})]
    (is (= "250 / 1,000 rows" (ts/progress-text p)))
    (is (= "25%" (ts/progress-pct p)))))

(deftest another-event-gives-no-progress
  (is (nil? (ts/node-progress #js {:kind "proc-start" :pid "m__a-1"}))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
