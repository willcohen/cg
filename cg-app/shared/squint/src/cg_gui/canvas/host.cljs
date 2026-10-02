;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.host
  "The send side of the canvas bridge: the VS Code API handle in a webview, else
   window.parent. acquireVsCodeApi works one time per webview, `vscode` keeps it.")

(def ^:private vscode
  (when (exists? js/acquireVsCodeApi)
    (js/acquireVsCodeApi)))

(defn post!
  "Send a JS-object message to the extension host."
  [msg]
  (if vscode
    (.postMessage vscode msg)
    (.postMessage (.-parent js/window) msg "*")))

(defn ui-state
  "The saved UI state of the webview (vscode getState), or nil where the host
   keeps none (an iframe, or a probe stub)."
  []
  (when (and vscode (fn? (.-getState vscode)))
    (.getState vscode)))

(defn merge-ui-state!
  "Merges UI keys (console width, hidden state) into the webview state. Does
   nothing where the host has no state."
  [kvs]
  (when (and vscode (fn? (.-setState vscode)))
    (.setState vscode (js/Object.assign #js {} (ui-state) kvs))))

(defn reveal-location!
  "Asks the host to reveal a 1-indexed parser span. `path` is the file of the
   span, or nil for the document of the canvas."
  [loc path]
  (when loc
    (let [sl (aget loc "start-line") sc (aget loc "start-column")
          el (aget loc "end-line")   ec (aget loc "end-column")]
      (when (and sl sc el ec)
        (post! #js {:type "cg/open-defop"
                    :path (or path nil)
                    :location #js {:startLine sl :startColumn sc
                                   :endLine el :endColumn ec}})))))
