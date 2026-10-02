;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.test-runner
  "Cljs.test runner footer for cg's dual-runtime .cljc test suites.

   squint + node keeps the process alive in some suites (wasmts timers,
   worker pools), so a bare `(run-tests)` footer can hang on success. This
   helper always exits explicitly: 0 on green, 1 on any failure or error.
   JVM consumers ignore this ns entirely -- the clojure.test runner handles
   process exit.

   The optional leading teardown fn is for suites whose runtime state
   outlives run-tests resolving (a worker pool, a PROJ context). Squint's
   ^:async metadata does not propagate to inline fns in argument position,
   so the teardown must be a top-level ^:async defn (or any fn returning a
   Promise) for its inner await to compile.

   Usage at the bottom of a cljs.test mirror:

     (ns my-test
       (:require #?(:cljs [cljs.test :as t])
                 #?(:cljs [cg.test-runner :as tr])))

     #?(:cljs (tr/run-tests-and-exit! \"my-test\"))"
  #?(:cljs (:require [cljs.test :as cljs-test])))

#?(:clj (set! *warn-on-reflection* true))

;; run-tests-and-exit! ends each suite with process.exit,
;; which emits no beforeExit. So a beforeExit means that the event loop
;; drained first: a setup or teardown Promise that never settled, as when
;; every pool worker exits during init. Without this, node exits 0 with no
;; Ran line.
#?(:cljs
   (.once js/process "beforeExit"
          (fn [_]
            (println "test suite: the event loop drained before the suite exited; its tests did not finish")
            (.exit js/process 1))))

#?(:cljs
   (defn run-tests-and-exit!
     "Invoke cljs.test/run-tests for the given namespace name strings
      (zero args = all registered tests), await any returned Promise, and
      process.exit with 0 on green, 1 on any failure or error.

      An optional leading teardown fn (zero-arg; sync or Promise-returning)
      runs between run-tests resolving and process.exit."
     [& args]
     (let [first-arg (first args)
           teardown  (when (fn? first-arg) first-arg)
           ns-names  (if teardown (rest args) args)]
       (.then (js/Promise.resolve (apply cljs-test/run-tests ns-names))
              (fn [results]
                (let [fail      (or (get results "fail") 0)
                      err       (or (get results "error") 0)
                      exit-code (if (pos? (+ fail err)) 1 0)]
                  (if teardown
                    (.then (js/Promise.resolve (teardown))
                           (fn [_] (.exit js/process exit-code)))
                    (.exit js/process exit-code))))))))
