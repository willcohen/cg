;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.run-relay-test
  "The canvas messages of a run in the CG Map tab."
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
    (is (= "JS" (aget started "runtime")) "the page runs only on JS")
    (is (= "cg/trace-event" (aget trace "type")))
    (is (identical? evt (aget trace "event")))))

(deftest a-page-error-is-a-run-output-line
  (let [m (relay/page-error-output #js {:type "cg/map-error" :level "warn" :error "no pool"})]
    (is (= "cg/run-output" (aget m "type")))
    (is (= "stderr" (aget m "stream")))
    (is (= "page warn: no pool\n" (aget m "text"))))
  (is (= "page error: boom\n"
         (aget (relay/page-error-output #js {:type "cg/map-error" :error "boom"}) "text"))
      "an error with no level is an error")
  (let [m (relay/page-error-output #js {:type "cg/map-error" :level "log" :error "sum: 12"})]
    (is (= "sum: 12\n" (aget m "text")) "the printed text of a run has no prefix")
    (is (= "stdout" (aget m "stream")))))

(deftest a-run-with-a-layer-ends-with-a-line-and-a-success
  (let [msgs (relay/canvas-messages #js {:type "cg/run-ended" :outcome "success" :count 2
                                         :layer "drawn"} file)]
    (is (= ["cg/run-output" "cg/run-ended"] (types msgs)))
    (is (= "ran on the JS backend: 2 results, map layer cgdata-drawn in the CG Map tab\n"
           (aget (first msgs) "text")))
    (is (= "success" (aget (second msgs) "outcome")))
    (is (= 2 (aget (second msgs) "count")))))

(deftest a-run-with-no-layer-ends-with-a-success
  (let [msgs (relay/canvas-messages #js {:type "cg/run-ended" :outcome "success" :count 1} file)]
    (is (= ["cg/run-output" "cg/run-ended"] (types msgs)))
    (is (= "ran on the JS backend: 1 result, no map layer\n" (aget (first msgs) "text")))))

(deftest a-jvm-run-names-the-jvm-backend
  (let [msgs (relay/canvas-messages #js {:type "cg/run-ended" :outcome "success" :count 2
                                         :layer "dots" :runtime "JVM"} file)]
    (is (= "ran on the JVM backend: 2 results, map layer cgdata-dots in the CG Map tab\n"
           (aget (first msgs) "text")))))

(deftest a-failed-run-gives-the-message-of-the-error
  (let [[m] (relay/canvas-messages #js {:type "cg/run-error" :message "boom"} file)]
    (is (= "cg/run-error" (aget m "type")))
    (is (= "boom" (aget m "message")))))

(deftest the-other-map-messages-give-nothing
  (doseq [t ["cg/ready" "cg/viewport-settled" "cg/map-error"]]
    (is (= [] (relay/canvas-messages #js {:type t} file)) t)))

(.then (js/Promise.resolve (t/run-tests)) #(js/process.exit (if (t/successful? %) 0 1)))
