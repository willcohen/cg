;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parcel-js-test
  "parcel.cg on both runtimes, for the code that the JS runtime runs another
   way: the zoning chain runs in the browser too. squint cannot call
   a keyword, and parcel.cg and zoning.cg give max-by-stable the key-fn
   :intersection-area."
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.run :as run])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private parcel "src/cg/parcel.cg")

;; b and c tie on the key; the tiebreak selects the smaller [code].
(def ^:private max-by-keyword-src
  "(require '[cg.parcel :as parcel])
   (:code (parcel/max-by-stable :intersection-area
                                (fn [z] [(:code z)])
                                [{:code \"a\" :intersection-area 1.0}
                                 {:code \"c\" :intersection-area 3.0}
                                 {:code \"b\" :intersection-area 3.0}]))")

#?(:cljs
   (defn ^:async run-max-by-keyword! []
     (await (run/load-module parcel))
     (is (= "b" (run/eval-cg max-by-keyword-src)))))

(deftest max-by-stable-takes-a-keyword-key-fn
  (testing "the largest :intersection-area, ties broken by code"
    #?(:clj (do (run/load-module parcel)
                (is (= "b" (run/eval-cg max-by-keyword-src))))
       :cljs (run-max-by-keyword!))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.parcel-js-test")))))
