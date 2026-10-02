;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.carbon
  "The subset of @carbon/react that the app-shell panels use. crs_picker uses
   ComposedModal and Search directly."
  (:require ["@carbon/react" :as Carbon]
            ["@carbon/react/icons" :as Icons]))

(def Button Carbon/Button)
(def Toggle Carbon/Toggle)
(def InlineLoading Carbon/InlineLoading)
(def ProgressBar Carbon/ProgressBar)
(def Tag Carbon/Tag)
(def Accordion Carbon/Accordion)
(def AccordionItem Carbon/AccordionItem)
(def CloseIcon Icons/Close)
(def CheckmarkIcon Icons/CheckmarkFilled)
(def ErrorIcon Icons/ErrorFilled)
