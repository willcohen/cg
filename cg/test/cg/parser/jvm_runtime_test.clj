;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.jvm-runtime-test
  "Tests of the JVM backend: the optimizing Truffle runtime, a failed init,
   and calls from many threads.

   cg.parser.jvm hosts web-tree-sitter on a GraalVM polyglot Context.
   With org.graalvm.truffle/truffle-runtime on the path and a GraalVM CE
   JDK (see flake.nix jvmJdk), the guest JIT-compiles via libgraal; without
   either it silently falls back to the interpreter (~6x slower). Stock
   OpenJDK 25 cannot load the optimizing runtime cleanly (JDK-8364936). This
   test fails loudly if that regresses. JVM-only."
  (:require [cg.parser.core :as parser]
            [cg.parser.jvm]
            [clojure.test :refer [deftest is testing]])
  (:import [com.oracle.truffle.api Truffle]))

(set! *warn-on-reflection* true)

(deftest optimizing-runtime-active
  (testing "Truffle selects an optimizing (guest-JIT) runtime, not the fallback"
    ;; Parse once so a Context is created and the runtime is resolved.
    (parser/parse "(ns example) (def x (item-> :parcels (geo/buffer 10)))")
    (let [runtime (.getName (Truffle/getRuntime))]
      (is (not= "Interpreted" runtime)
          (str "Truffle runtime is '" runtime "' (interpreted fallback). "
               "Run on GraalVM CE 25 with org.graalvm.truffle/truffle-runtime "
               "on the path; a stock OpenJDK falls back (JDK-8364936).")))))

(deftest a-failed-init-throws-its-ex-info
  (with-redefs [cg.parser.jvm/state (atom nil)
                cg.parser.jvm/boot-source
                (constantly "(function () { return function (r, g, ok, err) { err('boom'); }; })()")]
    (is (= "tree-sitter init failed: boom"
           (try (#'cg.parser.jvm/init!) nil (catch Exception e (ex-message e)))))))

(deftest calls-from-many-threads-share-one-context
  (let [src (slurp "examples/simple.cg")
        fs (mapv (fn [_] (future (dotimes [_ 10] (parser/extract-models src)) :ok)) (range 8))]
    (is (= (repeat 8 :ok) (mapv deref fs)))))
