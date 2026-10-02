;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.test-runner-test
  "The cljs test runner fails a suite that ends before its tests ran.
   Without that, a suite whose setup never settles exits 0 with no Ran
   line, and bb test:cljs goes on as if it passed. cljs only."
  #?(:cljs (:require [cljs.test :refer [deftest is]]
                     ["node:child_process" :as child-process]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

#?(:cljs
   (deftest a-suite-that-never-reaches-the-runner-fails
     (let [r (child-process/spawnSync (.-execPath js/process)
                                      #js ["test/fixtures/never_settles.mjs"]
                                      #js {:encoding "utf8"})]
       (is (= 1 (.-status r))
           (str "the suite ends before its tests ran; stdout: " (.-stdout r))))))

#?(:cljs (run_tests_and_exit_BANG_ "cg.test-runner-test"))
