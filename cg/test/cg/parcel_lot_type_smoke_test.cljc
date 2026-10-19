;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parcel-lot-type-smoke-test
  "Runs test/fixtures/parcel-lot-type-smoke.cg: the checks of the lot-type
   rules (corner, through, mid-block, end-cap, frontage groups, the
   pointed-lot Rear) on synthetic edges. The program throws when a check
   fails and prints one PASS or FAIL line for each check."
  #?(:clj (:require [clojure.string :as str]
                    [clojure.test :refer [deftest is]]
                    [cg.run :as run])
     :cljs (:require [clojure.string :as str]
                     [cljs.test :refer [deftest is]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/dataset.mjs" :as ds]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

;; On JS the println of CG code writes with console.log.
(defn- program-output []
  (let [run! #(run/eval-cg-file "test/fixtures/parcel-lot-type-smoke.cg")]
    #?(:clj (with-out-str (run!))
       :cljs (let [lines (atom [])
                   log (.-log js/console)]
               (set! (.-log js/console) (fn [& args] (swap! lines conj (str/join " " args))))
               (try (run!) (finally (set! (.-log js/console) log)))
               (str/join "\n" @lines)))))

(deftest parcel-lot-type-smoke
  (let [out     (program-output)
        summary (last (str/split-lines out))]
    (is (re-find #"parcel lot-type smoke: \d+ passed, 0 failed" summary) summary)
    (is (<= 80 (count (re-seq #"(?m)^PASS - " out)))
        "the program still runs every check")))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (ds/init-arquero!)))
             (.then (fn [_] (run/load-module "src/cg/parcel.cg")))
             (.then (fn [_] (run/load-module "src/cg/zoning.cg")))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.parcel-lot-type-smoke-test")))))
