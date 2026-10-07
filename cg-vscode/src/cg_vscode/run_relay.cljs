;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.run-relay
  "The canvas messages for a run in the CG Map tab. No vscode dependency.")

(defn- result-line
  "`msg` names its :runtime, JS when it has none, because the page posts the
   end of a JS run."
  [msg]
  (let [n (aget msg "count")
        layer (aget msg "layer")]
    (str "ran on the " (or (aget msg "runtime") "JS") " backend: " n " result" (when (not= 1 n) "s")
         (if layer
           (str ", map layer cgdata-" layer " in the CG Map tab")
           ", no map layer")
         "\n")))

(defn page-error-output
  "The console.log text of a run (level log) is the output of the run."
  [msg]
  (let [level (or (aget msg "level") "error")]
    (if (= "log" level)
      #js {:type "cg/run-output" :stream "stdout" :text (str (aget msg "error") "\n")}
      #js {:type "cg/run-output" :stream "stderr"
           :text (str "page " level ": " (aget msg "error") "\n")})))

(defn canvas-messages [msg file]
  (case (aget msg "type")
    "cg/run-started" [#js {:type "cg/run-started" :file file :runtime "JS"}]
    "cg/trace-event" [msg]
    "cg/run-ended"   [#js {:type "cg/run-output" :stream "stdout" :text (result-line msg)} msg]
    "cg/run-error"   [msg]
    []))
