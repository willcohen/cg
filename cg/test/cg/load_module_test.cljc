;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; run/load-module on both runtimes, with the shape of parcel.cg and
;; zoning.cg: a load cycle, an alias that a require makes after the
;; load-module, a def that reads the other module while it loads, a helper
;; name that both modules define, an op, and clojure.string. On the JS
;; runtime load-module is async, and it registers the op from the parser decl.
;; The module's op also runs as a rung under its module's alias, as
;; parcel.cg's ops do in the zoning chain.
(ns cg.load-module-test
  #?(:clj (:require [clojure.test :refer [deftest is testing]]
                    [cg.run :as run]
                    [cg.geo :as geo]
                    [cg.feature :as feature]
                    [cg.test-runner :refer [after]])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/parser/core.mjs" :as parser]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_ after]])))

#?(:clj (set! *warn-on-reflection* true))

(def ^:private mod-a "test/fixtures/modules/mod_a.cg")

(def ^:private pair-src
  "(require '[test.mod-a :as a]) (a/pair)")

(def ^:private area-src
  "(require '[test.mod-a :as a]) (require '[cg.feature :as f])
   (a/area-of (f/polygon [[0 0] [10 0] [10 10] [0 10] [0 0]]))")

;; The rung that zoning chain rungs look like: a module op under the
;; module's alias.
(def ^:private rung-src
  "(require '[cg.macros :refer [item->]]) (require '[test.mod-a :as a])
   (item-> :squares (a/area-of))")

(defn- square []
  (feature/polygon [[0 0] [10 0] [10 10] [0 10] [0 0]]))

(defn- check-pair! [pair]
  (is (= ["a" "b" "A" "HELLO"] (vec pair))
      "each module keeps its own helper, and cross-module calls resolve"))

(defn- check-area! [area]
  (is (= (geo/area (feature/polygon [[0 0] [10 0] [10 10] [0 10] [0 0]])) area)))

(defn- check-rung! [result]
  (is (= [(geo/area (square))] (vec result))))

(deftest load-module-with-a-cycle
  (testing "mod_a loads mod_b, which loads mod_a"
    (after (run/load-module mod-a)
           (fn [info]
             (is (= "test.mod-a" (str (:ns-name info))))
             (check-pair! (run/eval-cg pair-src))
             (check-area! (run/eval-cg area-src))))))

(deftest module-op-as-an-item-rung
  (after (run/load-module mod-a)
         (fn [_]
           (after (run/run-model (run/eval-cg rung-src) {:squares [(square)]} {})
                  check-rung!))))

;; A module's rung must get the param names of an op of a module that it
;; loads. On the JS runtime the module compiles before its own load-module
;; calls run, as boston.cg did under a file that loaded it.
(def ^:private rung-user "test/fixtures/modules/mod_rung_user.cg")

(def ^:private scale-src
  "(require '[test.mod-rung-user :as u]) (u/scale-model 3 {:factor 2})")

(deftest a-module-rung-binds-the-params-of-an-op-that-it-loads
  (after (run/load-module rung-user)
         (fn [_]
           (after (run/run-model (run/eval-cg scale-src) {:xs [1 2]} {})
                  #(is (= [6 12] (vec %)))))))

;; cg.cache/version-hash runs on the JS runtime too, because a flow file
;; computes the key of a checkpoint when it loads. The hash differs between
;; the runtimes.
(def ^:private cache-hash-src
  "(require '[cg.cache :as cache]) (cache/version-hash [\"0a1b2c3d\" \"2026-07-31\"])")

(defn- check-hash! [h]
  (is (and (string? h) (= 8 (count h))) "eight hex digits"))

(deftest cache-version-hash-runs-on-both-runtimes
  (after (run/load-module "src/cg/cache.cg")
         (fn [_]
           (let [h (run/eval-cg cache-hash-src)]
             (check-hash! h)
             #?(:clj (is (= "214a0400" h) "the JVM hash of this key"))))))

;; A module's ns name may carry metadata; the name is the symbol after
;; it.
(def ^:private marked "test/fixtures/modules/mod_marked.cg")

(def ^:private twice-src
  "(require '[test.mod-marked :as mm]) (mm/twice 21)")

(deftest a-marked-module-loads-under-its-name
  (after (run/load-module marked)
         (fn [info]
           (is (= "test.mod-marked" (str (:ns-name info))))
           (is (= 42 (run/eval-cg twice-src))))))

;; A consumer of the library runs in a folder that has no src/cg/. The
;; JVM then reads a library module from the classpath. test/ is a classpath
;; root, and this path is not a file from the working folder.
#?(:clj
   (deftest a-path-that-is-not-a-file-loads-from-the-classpath
     (is (= "test.mod-res" (str (:ns-name (run/load-module "fixtures/modules/mod_res.cg")))))
     (is (= "resource" (run/eval-cg "(require '[test.mod-res :as res]) res/origin")))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (parser/init! nil)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.load-module-test")))))
