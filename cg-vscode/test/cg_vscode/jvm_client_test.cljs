;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.jvm-client-test
  "The SSE frame parser of the JVM client. No backend.
   Runs standalone: node test/cg_vscode/jvm_client_test.mjs."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/runtime/jvm_client.mjs" :as jvm]))

(defn- kinds [events] (mapv (fn [e] (aget e "kind")) events))

(deftest complete-frames-give-events-and-the-tail-waits
  (let [[events tail] (jvm/sse-events "data: {\"kind\":\"a\"}\n\ndata: {\"kind\":\"b\"}\n\ndata: {\"ki")]
    (is (= ["a" "b"] (kinds events)))
    (is (= "data: {\"ki" tail) "a chunk can end in the middle of a frame")))

(deftest a-frame-that-does-not-parse-is-skipped
  (let [[events tail] (jvm/sse-events "data: {bad\n\n: comment\n\ndata: {\"kind\":\"c\"}\n\n")]
    (is (= ["c"] (kinds events)))
    (is (= "" tail))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err (or (get results "error") 0)]
           (js/process.exit (if (pos? (+ fail err)) 1 0)))))
