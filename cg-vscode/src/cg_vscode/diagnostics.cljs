;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.diagnostics
  "Converts the :diagnostics of a CgFlow into vscode/Diagnostic objects and
   manages DiagnosticCollections."
  (:require ["vscode" :as vscode]
            ["../../../cg-app/shared/squint/src/cg_gui/doc/diagnostics.mjs" :as data]))

(def ^:private severity->vscode
  {:error vscode/DiagnosticSeverity.Error
   :warning vscode/DiagnosticSeverity.Warning
   :info vscode/DiagnosticSeverity.Information})

(defn- ^js data->vscode
  [{:keys [range message code severity source]}]
  (let [[sl sc] (:start range)
        [el ec] (:end range)
        d (vscode/Diagnostic. (vscode/Range. sl sc el ec) message
                              (get severity->vscode severity
                                   vscode/DiagnosticSeverity.Error))]
    (set! (.-code d) code)
    (set! (.-source d) source)
    d))

(defn ^js create-collection!
  "Creates a DiagnosticCollection that the caller disposes. Each source needs
   its own, because a `set` replaces the findings of the whole collection."
  [name]
  (.createDiagnosticCollection vscode/languages name))

(defn set-diagnostic-data! [^js collection uri diagnostic-data]
  (.set collection uri (into-array (map data->vscode diagnostic-data))))

(defn set-diagnostics! [^js collection uri cg-flow]
  (set-diagnostic-data! collection uri (data/cg-flow->diagnostic-data cg-flow)))

(defn clear-diagnostics! [^js collection uri]
  (.delete collection uri))
