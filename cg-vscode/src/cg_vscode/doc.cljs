;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.doc
  "Host-side helpers for finding the .cg document the user is looking at."
  (:require ["vscode" :as vscode]))

(defn cg-uri?
  "True when `u` is a Uri pointing at a .cg file."
  [^js u]
  (boolean (and u
                (let [p (.-fsPath u)]
                  (and (string? p) (.endsWith p ".cg"))))))

(defn active-cg-uri
  "The Uri of the .cg document in view, or nil. It also looks at the tabs,
   because `activeTextEditor` is nil while a webview has focus."
  []
  (or
   (let [^js u (some-> vscode/window.activeTextEditor .-document .-uri)]
     (when (cg-uri? u) u))
   (let [^js u (some-> vscode/window.tabGroups.activeTabGroup
                       .-activeTab .-input .-uri)]
     (when (cg-uri? u) u))
   (some (fn [^js ed]
           (let [^js u (some-> ed .-document .-uri)]
             (when (cg-uri? u) u)))
         (or vscode/window.visibleTextEditors #js []))
   (some (fn [^js grp]
           (some (fn [^js tab]
                   (let [^js u (some-> tab .-input .-uri)]
                     (when (cg-uri? u) u)))
                 (or (.-tabs grp) #js [])))
         (or (some-> vscode/window.tabGroups .-all) #js []))))
