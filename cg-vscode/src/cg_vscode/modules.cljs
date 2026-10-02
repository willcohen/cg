;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.modules
  "Resolves and reads the module files that a .cg loads with run/load-module."
  (:require ["vscode" :as vscode]
            ["fs" :as fs]
            [cg-vscode.extract :as extract]
            ["../../../cg-app/shared/squint/src/cg_gui/doc/modules.mjs" :as doc-modules]))

(defn bb-root-for-document
  "The nearest directory with a bb.edn above the document, else the workspace
   folder. `bb run` starts there, and module paths resolve against it."
  [^js document]
  (let [start-path (.-fsPath (.-uri document))
        node-path (js/require "path")]
    (loop [dir (.dirname node-path start-path)]
      (cond
        (.existsSync fs (.join node-path dir "bb.edn"))
        dir
        (or (= dir "/") (= dir (.dirname node-path dir)))
        (or (when-let [^js wf (vscode/workspace.getWorkspaceFolder (.-uri document))]
              (.. wf -uri -fsPath))
            (.dirname node-path start-path))
        :else
        (recur (.dirname node-path dir))))))

(defn resolve-module-path
  "The absolute path of a module path under the bb root of the document, or
   nil when no file is there."
  [^js document path-str]
  (let [^js node-path (js/require "path")
        root (or (try (bb-root-for-document document) (catch :default _ nil))
                 (.dirname node-path (.-fsPath (.-uri document))))
        candidate (.resolve node-path root path-str)]
    (when (.existsSync fs candidate) candidate)))

(defn read-text-safe!
  "Reads `path` as UTF-8 text. Returns nil on failure, because a module that
   cannot be read must not stop the render of the main flow."
  [path]
  (try (.toString (.readFileSync fs path) "utf8")
       (catch :default _ nil)))

(defn follow-loaded-modules!
  "Promise of the operations of every module that `cg-flow` loads. Each has a
   :source-module with the absolute :path of its file."
  [^js document cg-flow]
  (doc-modules/follow-loaded-modules!
   cg-flow
   {:resolve (fn [p] (resolve-module-path document p))
    :read    (fn [abs] (js/Promise.resolve (read-text-safe! abs)))
    :parse   extract/extract-models}))
