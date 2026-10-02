;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.codeview
  "The content of each code panel tab: dsl is the .cg text with an overlay
   from the positions, and flow is the merged CgFlow."
  (:require ["../../../../shared/squint/src/cg_gui/doc/overlay.mjs" :as overlay]
            [cg-app.ui.extract :as extract]))

(defn content-for [fmt text flow positions]
  (cond
    (nil? flow) ";; Load a .cg file"
    (= fmt "dsl") (overlay/update-visual-metadata text flow positions)
    (= fmt "flow") (js/JSON.stringify
                    (extract/merge-positions-into-flow flow positions) nil 2)
    :else ";; unknown format"))
