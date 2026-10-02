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
  (let [r (vscode/Range. (vscode/Position. (first (:start range))
                                           (second (:start range)))
                         (vscode/Position. (first (:end range))
                                           (second (:end range))))
        d (vscode/Diagnostic. r message
                              (get severity->vscode severity
                                   vscode/DiagnosticSeverity.Error))]
    (set! (.-code d) code)
    (set! (.-source d) source)
    d))

(defn cg-flow->vscode-diagnostics
  "Convert a CgFlow's :diagnostics list into a JS array of vscode/Diagnostic."
  [cg-flow]
  (into-array (map data->vscode (data/cg-flow->diagnostic-data cg-flow))))

(defn ^js create-collection!
  "Creates a DiagnosticCollection that the caller disposes. Each source needs
   its own, because a `set` replaces the findings of the whole collection."
  ([] (create-collection! "cg"))
  ([name] (.createDiagnosticCollection vscode/languages name)))

(defn set-diagnostic-data!
  "Replaces the diagnostics for `uri` with those built from diagnostic maps."
  [^js collection uri diagnostic-data]
  (.set collection uri (into-array (map data->vscode diagnostic-data))))

(defn set-diagnostics!
  "Replace the diagnostics for `uri` with those derived from `cg-flow`."
  [^js collection uri cg-flow]
  (.set collection uri (cg-flow->vscode-diagnostics cg-flow)))

(defn clear-diagnostics!
  "Drop any diagnostics for `uri` (use on document close)."
  [^js collection uri]
  (.delete collection uri))
