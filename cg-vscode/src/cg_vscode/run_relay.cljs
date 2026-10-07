;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.run-relay
  "The canvas messages for a run in the CG Map tab. No vscode dependency.")

(defn- result-line
  "The console line of the end of a run. `msg` names its :runtime, JS when it
   has none, because the page posts the end of a JS run."
  [msg]
  (let [n (aget msg "count")
        layer (aget msg "layer")]
    (str "ran on the " (or (aget msg "runtime") "JS") " backend: " n " result" (when (not= 1 n) "s")
         (if layer
           (str ", map layer cgdata-" layer " in the CG Map tab")
           ", no map layer")
         "\n")))

(defn canvas-messages
  "The messages for the canvas that a message `msg` of the map webview gives,
   as a vector. `file` is the path of the .cg of the run."
  [msg file]
  (case (aget msg "type")
    "cg/run-started" [#js {:type "cg/run-started" :file file}]
    "cg/trace-event" [msg]
    "cg/run-ended"   [#js {:type "cg/run-output" :stream "stdout" :text (result-line msg)} msg]
    "cg/run-error"   [msg]
    []))
