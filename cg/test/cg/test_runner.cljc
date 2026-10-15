;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.test-runner
  "Cljs.test runner footer for cg's dual-runtime .cljc test suites, and the
   helpers `after` and `error-message`, which a test body uses on both
   runtimes.

   squint + node keeps the process alive in some suites (wasmts timers,
   worker pools), so a bare `(run-tests)` footer can hang on success.
   `run-tests-and-exit!` always exits explicitly: 0 on green, 1 on any
   failure or error. On the JVM, the clojure.test runner ends the process.

   The optional leading teardown fn is for suites whose runtime state
   outlives run-tests resolving (a worker pool, a PROJ context)."
  #?(:clj (:require [clojure.core.async :as a]
                    [cg.trace :as trace])
     :cljs (:require [cljs.test :as cljs-test])))

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

;; Not `then`: import() treats a module that exports `then` as a thenable.
(defn after
  "Calls `f` with `x`. On cljs, `x` can be a Promise, and the result is a
   Promise. A deftest must return it, because cljs.test waits only for a
   Promise that the test body returns."
  [x f]
  #?(:clj (f x) :cljs (.then (js/Promise.resolve x) f)))

(defn error-message
  "The message of the error that `(thunk)` throws or rejects with, or nil."
  [thunk]
  #?(:clj (try (thunk) nil (catch Exception e (ex-message e)))
     :cljs (-> (js/Promise.resolve nil)
               (.then (fn [_] (thunk)))
               (.then (fn [_] nil) ex-message))))

#?(:clj
   (defn poll
     "Call `f` until `done?` is true of its value, or `tries` more times, with
      50 ms between the calls, and give the last value. Use it for a result
      that a go block gives later."
     [f done? tries]
     (loop [i 0]
       (let [v (f)]
         (if (or (done? v) (>= i tries))
           v
           (do (Thread/sleep 50) (recur (inc i))))))))

#?(:clj
   (defn collect-events
     "Call `f` with a tap on the cg.trace stream, and give the events that `f`
      emitted, in order. A throw of `f` goes to the caller. The tap gets each
      event later, from a go block. Thus an end event follows `f`, and the
      drain stops at that event, or after 10 s."
     [f]
     (let [ch (a/chan 1024)]
       (trace/tap-trace ch)
       (try
         (f)
         (let [end (trace/emit! "collect-events-end" {})
               deadline (a/timeout 10000)]
           (loop [acc []]
             (let [[e _] (a/alts!! [ch deadline])]
               (if (or (nil? e) (= end e))
                 acc
                 (recur (conj acc e))))))
         (finally
           (trace/untap-trace ch))))))

#?(:clj
   (defn with-system-property
     "Call `f` with the system property `k` set to `v` (nil clears it), then
      put the old value back."
     [k v f]
     (let [old (System/getProperty k)]
       (try
         (if v (System/setProperty k v) (System/clearProperty k))
         (f)
         (finally
           (if old (System/setProperty k old) (System/clearProperty k)))))))
