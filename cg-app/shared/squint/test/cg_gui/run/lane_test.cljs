;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run.lane-test
  "The pure backend decisions: runtime resolution, the web Run route and the
   CG Map route. A runtime choice is never rerouted in silence."
  (:require [cljs.test :as t :refer [deftest is testing]]
            ["../../../src/cg_gui/run/lane.mjs" :as lane]))

(defn- route [needs-jvm? backend-reachable? resolved-runtime]
  (lane/route-map-run {:needs-jvm? needs-jvm?
                          :backend-reachable? backend-reachable?
                          :resolved-runtime resolved-runtime}))

(deftest route-decision-table
  (testing "self-contained .cg follows the resolved runtime"
    (is (= :js  (route false true  :js)))
    (is (= :js  (route false false :js)))
    (is (= :jvm (route false true  :jvm))))
  (testing "explicit jvm with the backend down refuses loudly (ExplicitChoiceWins)"
    (is (= :refused (route false false :jvm))))
  (testing "needs-jvm overrides the toggle — reachable backend routes to jvm"
    (is (= :jvm (route true true :js)))
    (is (= :jvm (route true true :jvm))))
  (testing "needs-jvm with the backend down is unsupported (guard message)"
    (is (= :unsupported (route true false :js)))
    (is (= :unsupported (route true false :jvm)))))

(deftest runtime-choice-never-silently-rerouted
  (doseq [nj [true false]
          br [true false]
          rr [:jvm :js]]
    (let [lane (route nj br rr)]
      (when nj
        (is (not= :js lane)
            (str "a needs-jvm file must never run on the JS backend: " [nj br rr] " -> " lane)))
      (when (= :jvm rr)
        (is (not= :js lane)
            (str "an explicit jvm choice must never silently fall back to js: " [nj br rr] " -> " lane))))))

(deftest flow-needs-jvm-predicate
  (is (true?  (lane/flow-needs-jvm? {:loaded-modules ["parcel"]})))
  (is (true?  (lane/flow-needs-jvm? {:loaded-modules ["a" "b"]})))
  (is (false? (lane/flow-needs-jvm? {:loaded-modules []})))
  (is (false? (lane/flow-needs-jvm? {})))
  (is (false? (lane/flow-needs-jvm? {:bindings [{:name "m"}]}))))

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
