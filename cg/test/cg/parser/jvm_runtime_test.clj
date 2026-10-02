;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.jvm-runtime-test
  "Regression guard for the optimizing Truffle runtime.

   cg.parser.jvm hosts web-tree-sitter on a GraalVM polyglot Context.
   With org.graalvm.truffle/truffle-runtime on the path and a GraalVM CE
   JDK (see flake.nix jvmJdk), the guest JIT-compiles via libgraal; without
   either it silently falls back to the interpreter (~6x slower). Stock
   OpenJDK 25 cannot load the optimizing runtime cleanly (JDK-8364936). This
   test fails loudly if that regresses. JVM-only.

   Run with: clojure -M:test-parser"
  (:require [cg.parser.core :as parser]
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
