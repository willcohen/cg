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
                    [cg.util :as util]
                    [clojure.string :as str]
                    [cg.test-runner :refer [after error-message]])
     :cljs (:require [cljs.test :refer [deftest is testing]]
                     ["../../src/cg/run.mjs" :as run]
                     ["../../src/cg/geo.mjs" :as geo]
                     ["../../src/cg/feature.mjs" :as feature]
                     ["../../src/cg/util.mjs" :as util]
                     ["../../src/cg/parser/core.mjs" :as parser]
                     [clojure.string :as str]
                     ["node:fs" :as fs]
                     ["node:os" :as os]
                     ["node:path" :as node-path]
                     ["./wasmts_setup.mjs" :refer [init_wasmts_BANG_]]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_ after error-message]]))
  #?(:clj (:import (java.io File)
                   (java.nio.file Files)
                   (java.nio.file.attribute FileAttribute))))

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

(defn- temp-dir
  "A new folder. Its path has forward slashes, because a test puts it in a
   CG string, where a backslash of win32 is an escape."
  []
  (str/replace #?(:clj (str (Files/createTempDirectory "cg-load-module" (make-array FileAttribute 0)))
                  :cljs (fs/mkdtempSync (node-path/join (os/tmpdir) "cg-load-module")))
               "\\" "/"))

(defn- delete-dir! [dir]
  #?(:clj (doseq [^File f (reverse (file-seq (File. ^String dir)))] (.delete f))
     :cljs (fs/rmSync dir #js {:recursive true :force true})))

;; A module that fails to load must run again on the next load: when its
;; dep is missing, and when its text does not compile.
(deftest a-module-that-fails-on-a-missing-dep-loads-again
  (let [dir (temp-dir)
        user (str dir "/user.cg")
        dep (str dir "/dep.cg")]
    (util/write-file! user (str "(ns test.mod-late-user (:require [cg.run :as run]))\n"
                                "(run/load-module \"" dep "\")\n"
                                "(def ran 1)\n"))
    (after (error-message #(run/load-module user))
           (fn [msg]
             (is (some? msg))
             (util/write-file! dep "(ns test.mod-late-dep)\n")
             (after (run/load-module user)
                    (fn [_]
                      (is (= 1 (run/eval-cg "(require '[test.mod-late-user :as u]) u/ran")))
                      (delete-dir! dir)))))))

(deftest a-module-that-does-not-compile-loads-again
  (let [dir (temp-dir)
        bad (str dir "/bad.cg")]
    (util/write-file! bad "(ns test.mod-bad)\n(def fixed 1\n")
    (after (error-message #(run/load-module bad))
           (fn [msg]
             (is (some? msg))
             (util/write-file! bad "(ns test.mod-bad)\n(def fixed 2)\n")
             (after (run/load-module bad)
                    (fn [_]
                      (is (= 2 (run/eval-cg "(require '[test.mod-bad :as b]) b/fixed")))
                      (delete-dir! dir)))))))

#?(:cljs (-> (init_wasmts_BANG_)
             (.then (fn [_] (parser/init! nil)))
             (.then (fn [_] (run_tests_and_exit_BANG_ "cg.load-module-test")))))
