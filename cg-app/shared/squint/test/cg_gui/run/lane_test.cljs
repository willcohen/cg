;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run.lane-test
  "The pure backend decisions: runtime resolution and the Run route. A
   runtime choice is never rerouted in silence."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/run/lane.mjs" :as lane]))

(deftest resolve-explicit-wins
  (is (= "jvm" (lane/resolve-runtime "jvm" false)))
  (is (= "js"  (lane/resolve-runtime "js" true))))

(deftest resolve-auto-follows-reachability
  (is (= "jvm" (lane/resolve-runtime "auto" true)))
  (is (= "js"  (lane/resolve-runtime "auto" false))))

(deftest execute-route-refuses-explicit-jvm-when-unreachable
  (let [r (lane/execute-route "jvm" false)]
    (is (true? (:refused r)))
    (is (string? (:reason r))))
  (let [r (lane/execute-route "jvm" true)]
    (is (false? (:refused r)))
    (is (= "jvm" (:runtime r))))
  (let [r (lane/execute-route "auto" false)]
    (is (false? (:refused r)))
    (is (= "js" (:runtime r))))
  (let [r (lane/execute-route "js" false)]
    (is (false? (:refused r)))
    (is (= "js" (:runtime r)))))

(deftest chip-text-names-an-explicit-choice-and-the-auto-resolution
  (is (= "CG: jvm" (lane/chip-text "jvm" "js")))
  (is (= "CG: js" (lane/chip-text "js" "js")))
  (is (= "CG: auto->jvm" (lane/chip-text "auto" "jvm")))
  (is (= "CG: auto->js" (lane/chip-text "auto" "js")))
  (is (= "CG: auto" (lane/chip-text "auto" nil))))

;; run-tests.mjs reads the exit code.
(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
