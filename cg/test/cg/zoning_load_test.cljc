;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.zoning-load-test
  "zoning.cg loads parcel.cg, and parcel.cg does not load zoning.cg. Because
   of this, a file whose only load is zoning.cg runs on both runtimes. Each cljs suite is
   its own node process, therefore the load here is the first one on the JS runtime;
   the JVM resets the context first."
  #?(:clj (:require [clojure.test :refer [deftest is]]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is]]
                     ["../../src/cg/run.mjs" :as run]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private zoning "src/cg/zoning.cg")

(def ^:private both-src
  "(require '[cg.zoning :as zoning] '[cg.parcel :as parcel])
   [(fn? zoning/score-zoning-intersections) (fn? parcel/max-by-stable)]")

(defn- check-both! [result]
  (is (= [true true] (vec result))
      "a first load of zoning.cg defines zoning.cg and parcel.cg"))

#?(:cljs
   (defn ^:async run-zoning-first! []
     (await (run/load-module zoning))
     (check-both! (run/eval-cg both-src))))

(deftest zoning-loads-first
  #?(:clj (do (run/reset-context!)
              (run/load-module zoning)
              (check-both! (run/eval-cg both-src)))
     :cljs (run-zoning-first!)))

#?(:clj
   (deftest parcel-does-not-load-zoning
     (run/reset-context!)
     (run/load-module "src/cg/parcel.cg")
     (is (nil? (run/eval-cg "(find-ns 'cg.zoning)")))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.zoning-load-test")))))
