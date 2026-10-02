;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.run-relay
  "The canvas messages for a run in the CG Map tab. No vscode dependency.")

(defn- result-line [msg]
  (let [n (aget msg "count")
        layer (aget msg "layer")]
    (str "ran on the JS backend: " n " result" (when (not= 1 n) "s")
         (if layer
           (str ", map layer cgdata-" layer " in the CG Map tab")
           ", no map layer")
         "\n")))

(defn canvas-messages
  "The messages for the canvas that a message `msg` of the map webview gives,
   as a vector. `file` is the path of the .cg of the run."
  [msg file]
  (let [t (aget msg "type")]
    (cond
      (= t "cg/run-started")
      [#js {:type "cg/run-started" :file file}]

      (= t "cg/trace-event")
      [#js {:type "cg/trace-event" :event (aget msg "event")}]

      (= t "cg/run-result")
      [#js {:type "cg/run-output" :stream "stdout" :text (result-line msg)}
       #js {:type "cg/run-ended" :outcome "success" :count (aget msg "count")}]

      (= t "cg/run-error")
      [#js {:type "cg/run-error" :message (aget msg "error")}]

      :else [])))
