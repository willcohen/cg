;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.run-relay-test
  "The canvas messages of a JS run in the CG Map tab.
   Runs standalone: node test/cg_vscode/run_relay_test.mjs."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/run_relay.mjs" :as relay]))

(def ^:private file "/w/cg/examples/parcel.cg")

(defn- types [msgs] (mapv (fn [m] (aget m "type")) msgs))

(deftest a-run-start-and-its-trace-go-to-the-canvas
  (let [evt #js {:kind "proc-start" :pid "drawn__map-layer-1"}
        [started] (relay/canvas-messages #js {:type "cg/run-started" :model "drawn"} file)
        [trace] (relay/canvas-messages #js {:type "cg/trace-event" :event evt} file)]
    (is (= "cg/run-started" (aget started "type")))
    (is (= file (aget started "file")))
    (is (= "cg/trace-event" (aget trace "type")))
    (is (identical? evt (aget trace "event")))))

(deftest a-run-with-a-layer-ends-with-a-line-and-a-success
  (let [msgs (relay/canvas-messages #js {:type "cg/run-result" :ok true :count 2
                                         :mapped true :layer "drawn"} file)]
    (is (= ["cg/run-output" "cg/run-ended"] (types msgs)))
    (is (= "ran on the JS backend: 2 results, map layer cgdata-drawn in the CG Map tab\n"
           (aget (first msgs) "text")))
    (is (= "success" (aget (second msgs) "outcome")))
    (is (= 2 (aget (second msgs) "count")))))

(deftest a-run-with-no-layer-ends-with-a-success
  (let [msgs (relay/canvas-messages #js {:type "cg/run-result" :ok true :count 1 :mapped false} file)]
    (is (= ["cg/run-output" "cg/run-ended"] (types msgs)))
    (is (= "ran on the JS backend: 1 result, no map layer\n" (aget (first msgs) "text")))))

(deftest a-failed-run-gives-the-message-of-the-error
  (let [[m] (relay/canvas-messages #js {:type "cg/run-error" :error "boom"} file)]
    (is (= "cg/run-error" (aget m "type")))
    (is (= "boom" (aget m "message")))))

(deftest the-other-map-messages-give-nothing
  (doseq [t ["cg/ready" "cg/map-ready" "cg/layers-rendered" "cg/viewport-settled" "cg/map-error"
             "cg/csp-violation"]]
    (is (= [] (relay/canvas-messages #js {:type t} file)) t)))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err (or (get results "error") 0)]
           (js/process.exit (if (pos? (+ fail err)) 1 0)))))
