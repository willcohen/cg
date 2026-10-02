;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.doc.diagnostics
  "Converts CgFlow :diagnostics to plain data, with no vscode dependency.
   cg-vscode.diagnostics builds the vscode Diagnostic objects.")

(def ^:private typecheck-codes
  "Codes emitted by cg.parser.types (signature lookup + type checking).
   Everything else falls under cg-parser (extract + validate)."
  #{:unresolved-op :type-mismatch})

(defn- code->source [code]
  (if (contains? typecheck-codes code) "cg-typecheck" "cg-parser"))

(def ^:private residue-codes
  "Codes of a form that the parser does not accept at the top level. The file
   still runs, because a .cg program is Clojure, but the canvas cannot draw it."
  #{:forbidden-form :unclassified-top-level-form})

(def ^:private advisory-codes
  "Codes of a form that the parser cannot resolve or advises to move. The form
   gets a mark, but the file is not wrong."
  #{:unresolved-op :model-inside-defn})

(defn- editor-severity
  "The editor severity: residue codes become :info and advisory codes :warning,
   because the parser marks each one an error. Other codes keep their severity."
  [code severity]
  (cond
    (contains? residue-codes code) :info
    (contains? advisory-codes code) :warning
    :else severity))

(defn cg-flow->diagnostic-data
  "The :diagnostics of a CgFlow as a vector of {:range {:start [l c] :end [l c]}
   :code :severity :message :source}, with 0-indexed positions for VS Code."
  [{:keys [diagnostics]}]
  (mapv (fn [{:keys [code severity message location]}]
          (let [{:keys [start-line start-column end-line end-column]} location]
            {:range {:start [(dec start-line) (dec start-column)]
                     :end   [(dec end-line) (dec end-column)]}
             :code (name code)
             :severity (editor-severity code severity)
             :message message
             :source (code->source code)}))
        diagnostics))
