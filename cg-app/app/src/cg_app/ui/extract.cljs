;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.extract
  "Parses a flow with the tree-sitter parser and follows its loaded modules.
   The overlay read is in cg-gui.doc.overlay."
  (:require ["../../../../../cg/src/cg/parser/core.mjs" :as parser]
            ["../../../../shared/squint/src/cg_gui/doc/modules.mjs" :as doc-modules]
            [cg-app.ui.loader :as loader]))

(defn ^:async extract-flow
  "Promise<CgFlow>. opts must carry :grammar-wasm (path or URL) on first call."
  [text opts]
  (await (parser/extract-models text opts)))

(defn ^:async follow-loaded-modules!
  "A new flow with the operations of each module that `flow` loads. The flow
   comes back unchanged when no module resolves."
  [flow opts]
  (let [extra (await (doc-modules/follow-loaded-modules!
                      flow
                      {:resolve identity
                       :read    loader/fetch-module-text
                       :parse   (fn [text] (extract-flow text opts))}))]
    (if (zero? (count extra))
      flow
      ;; recheck removes the diagnostics from before the follow (the false
      ;; :unresolved-op marks for ops in other files) and resolves against the
      ;; merged registry.
      (parser/recheck
       (js/Object.assign #js {} flow
                         #js {:operations
                              (.concat (or (aget flow "operations") #js [])
                                       (into-array extra))})))))
