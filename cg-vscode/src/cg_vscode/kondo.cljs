;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.kondo
  "Runs clj-kondo on a .cg buffer into its own DiagnosticCollection."
  (:require ["vscode" :as vscode]
            ["node:child_process" :as cp]
            [cg-vscode.diagnostics :as diag]
            [cg-vscode.diagnostics.kondo :as kdata]))

(defn- binary-path
  "The `cg.cljKondoPath` setting, else `clj-kondo` on PATH. VS Code can start
   without the PATH of the dev shell."
  []
  (let [^js cfg (.getConfiguration vscode/workspace "cg")
        p (.get cfg "cljKondoPath")]
    (if (and p (pos? (count (str p))))
      (str p)
      "clj-kondo")))

(defn- workspace-dir
  "The workspace folder of the document. clj-kondo finds its config from
   there up."
  [^js document]
  (when-let [^js folder (.getWorkspaceFolder vscode/workspace (.-uri document))]
    (.. folder -uri -fsPath)))

(defn- run-kondo!
  "Resolves to the stdout of `bin` with `text` on stdin, or to nil when the
   command cannot start."
  [bin args cwd text]
  (js/Promise.
   (fn [resolve _reject]
     (try
       (let [^js child (cp/spawn bin (into-array args) #js {:cwd cwd})
             out (atom "")]
         (.on (.-stdout child) "data" (fn [d] (swap! out str (str d))))
         (.on child "error" (fn [_err] (resolve nil)))
         (.on child "close" (fn [_code] (resolve @out)))
         (.write (.-stdin child) text)
         (.end (.-stdin child)))
       (catch :default _ (resolve nil))))))

(defn lint-document!
  "Lints the text of one .cg buffer, saved or not, into `collection`. Returns
   a Promise that always resolves."
  [^js collection ^js document]
  (let [uri (.-uri document)
        path (.-fsPath uri)
        text (.getText document)]
    (-> (run-kondo! (binary-path) (kdata/argv path) (workspace-dir document) text)
        (.then (fn [out]
                 (when (some? out)
                   (diag/set-diagnostic-data! collection uri
                                              (kdata/parse-output out)))))
        (.catch (fn [_] nil)))))
