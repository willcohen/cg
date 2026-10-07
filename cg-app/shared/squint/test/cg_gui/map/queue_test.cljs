;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.map.queue-test
  "The queue of the map page: one job at a time, and a wait stage for a run
   that must wait."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/map/queue.mjs" :as queue]))

(defn- states [posted]
  (mapv (fn [m] (aget (aget m "event") "state")) posted))

(defn- tick [] (js/Promise. (fn [res _] (js/setTimeout res 0))))

(deftest ^:async a-run-behind-a-busy-queue-posts-a-wait-stage
  (let [posted #js []
        q (queue/make (js/Promise.resolve "ctx") (fn [m] (.push posted m)))
        !release (atom nil)
        hold (fn [_ _] (js/Promise. (fn [res _] (reset! !release res))))
        seen #js []
        job (fn [ctx m] (.push seen #js [ctx m (.-length posted)]) (js/Promise.resolve nil))]
    (queue/enqueue! q hold "a" true)
    (is (= [] (states posted)) "an idle queue posts no wait")
    (queue/enqueue! q job "form" false)
    (let [second-run (queue/enqueue! q job "b" true)]
      (is (= ["start"] (states posted)) "a run that waits posts, a REPL form does not")
      (is (= "wait in the queue" (aget (aget (aget posted 0) "event") "label")))
      (await (tick))
      (@!release nil)
      (await second-run))
    (is (= ["start" "done"] (states posted)))
    (is (= [["ctx" "form" 1] ["ctx" "b" 2]] (mapv vec seen))
        "the jobs run in order, and the wait ends before the run")
    (let [n (.-length posted)]
      (await (queue/enqueue! q job "c" true))
      (is (= n (.-length posted)) "an idle queue again posts no wait"))))

(deftest ^:async a-failed-job-does-not-stop-the-queue
  (let [q (queue/make (js/Promise.resolve "ctx") (fn [_] nil))
        seen #js []]
    (queue/enqueue! q (fn [_ _] (js/Promise.reject (js/Error. "boom"))) "a" true)
    (await (queue/enqueue! q (fn [_ m] (.push seen m) (js/Promise.resolve nil)) "b" true))
    (is (= ["b"] (vec seen)))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
