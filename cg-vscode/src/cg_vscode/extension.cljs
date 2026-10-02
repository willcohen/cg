;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.extension
  (:require ["vscode" :as vscode]
            [cg-vscode.extension.lifecycle :as lc]
            [cg-vscode.runtime :as runtime]
            [cg-vscode.editor :as editor]
            [cg-vscode.exec-webview :as exec-webview]
            [cg-vscode.run-status :as run-status]))

(defonce !app-db
  (atom {:extension/context       nil
         :extension/disposables   []
         :runtime/choice          "auto"
         :runtime/resolved        nil
         :runtime/jvm-reachable   false
         :runtime/status-bar      nil
         :runtime/output-channel  nil
         :editor/flow-editor-uris #{}
         :editor/panels           {}}))

(defn- create-run-status-bar!
  "The status bar item of the run status. It stays hidden until a run starts."
  [!state]
  (let [^js bar (vscode/window.createStatusBarItem vscode/StatusBarAlignment.Left 50)]
    (set! (.-name bar) "CG Run")
    (set! (.-command bar) "cg.showRunStatus")
    (lc/push-disposable! !state bar)
    (swap! !state assoc :run-status/bar bar)))

(defn- show-run-status!
  "Opens the CG Run Status tab beside the active editor."
  [!state]
  (if-let [^js panel (:run-status/panel @!state)]
    (.reveal panel js/undefined true)
    (let [^js panel (vscode/window.createWebviewPanel
                     "cg.runStatus" "CG Run Status"
                     #js {:viewColumn vscode/ViewColumn.Beside :preserveFocus true}
                     #js {:enableScripts false})]
      (.onDidDispose panel (fn [] (swap! !state dissoc :run-status/panel)))
      (swap! !state assoc :run-status/panel panel :run-status/nonce (lc/make-nonce))
      (run-status/draw! !state (js/Date.now)))))

(defn ^:export activate [context]
  (when context
    (swap! !app-db assoc :extension/context context))

  (let [!state !app-db
        ^js channel (vscode/window.createOutputChannel "CG")]
    (lc/push-disposable! !state channel)
    (swap! !state assoc :runtime/output-channel channel)
    (.appendLine channel "CG: activating...")

    (runtime/init-runtime! !state)
    (create-run-status-bar! !state)
    (runtime/register-runtime-commands! !state)
    (lc/register-command! !state "cg.openMap"
                          exec-webview/open-map-webview!)
    (exec-webview/register-run-commands! !state)
    (lc/register-command! !state "cg.showRunStatus" show-run-status!)

    (editor/init-editor! !state)
    (editor/setup-split-view! !state)

    (lc/set-when-context! "cg:active" true)

    (.appendLine channel "CG: active")

    #js {:v1 {}}))

(defn ^:export deactivate []
  (runtime/stop-backend!)
  (run-status/stop! !app-db)
  (lc/cleanup! !app-db))
