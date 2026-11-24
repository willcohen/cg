;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.doc
  "Host-side helpers for finding the .cg document the user is looking at."
  (:require ["vscode" :as vscode]))

(defn cg-uri? [^js u]
  (boolean (some-> u .-fsPath (.endsWith ".cg"))))

(defn active-cg-uri
  "The Uri of the .cg document in view, or nil. It also looks at the tabs,
   because `activeTextEditor` is nil while a webview has focus."
  []
  (let [cg-only (fn [^js u] (when (cg-uri? u) u))]
    (or (cg-only (some-> vscode/window.activeTextEditor .-document .-uri))
        (cg-only (some-> vscode/window.tabGroups.activeTabGroup .-activeTab .-input .-uri))
        (some (fn [^js ed] (cg-only (some-> ed .-document .-uri))) vscode/window.visibleTextEditors)
        (some (fn [^js grp] (some (fn [^js tab] (cg-only (some-> tab .-input .-uri))) (.-tabs grp)))
              vscode/window.tabGroups.all))))
