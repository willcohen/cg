;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.run.flow-error-test
  "flow-error-ex: the ex-info built from a flow error-channel report must
   name the failing proc and the underlying exception in its MESSAGE and
   carry that exception as the CAUSE — a bare \"Flow execution error\"
   hides which step died."
  (:require [clojure.test :refer [deftest is testing]]
            [cg.run]))

(set! *warn-on-reflection* true)

(def ^:private flow-error-ex #'cg.run/flow-error-ex)

(deftest message-names-proc-op-count-and-exception
  (let [inner (java.util.concurrent.TimeoutException.)
        e (flow-error-ex {:clojure.core.async.flow/pid :transform-coords-56567
                          :clojure.core.async.flow/op :step
                          :clojure.core.async.flow/count 3
                          :clojure.core.async.flow/ex inner})]
    (testing "message carries proc, op, message count, and exception class"
      (is (= (str "Flow execution error in :transform-coords-56567 during "
                  ":step (message 3): java.util.concurrent.TimeoutException")
             (ex-message e))))
    (testing "underlying exception is the cause"
      (is (identical? inner (ex-cause e))))
    (testing "full report stays in ex-data"
      (is (= :flow-error (:type (ex-data e))))
      (is (= :transform-coords-56567
             (get-in (ex-data e) [:error :clojure.core.async.flow/pid]))))))

(deftest inner-message-appended-when-present
  (let [e (flow-error-ex {:clojure.core.async.flow/pid :p1
                          :clojure.core.async.flow/ex (ex-info "boom" {})})]
    (is (= "Flow execution error in :p1: clojure.lang.ExceptionInfo - boom"
           (ex-message e)))))

(deftest empty-report-degrades-to-generic-message
  (let [e (flow-error-ex {})]
    (is (= "Flow execution error" (ex-message e)))
    (is (nil? (ex-cause e)))))
