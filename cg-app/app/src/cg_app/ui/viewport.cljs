;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.viewport
  "The viewport breakpoint hook: under 768 px is phone, under 1024 px is
   tablet, else desktop."
  (:require ["react" :refer [useSyncExternalStore]]))

(def phone-max 768)
(def tablet-max 1024)

(defn breakpoint
  "innerWidth -> \"phone\" | \"tablet\" | \"desktop\"."
  [width]
  (cond
    (< width phone-max) "phone"
    (< width tablet-max) "tablet"
    :else "desktop"))

(defn phone? [bp] (= "phone" bp))

(defn- subscribe [cb]
  (.addEventListener js/window "resize" cb)
  (fn [] (.removeEventListener js/window "resize" cb)))

(defn- snapshot []
  (breakpoint (.-innerWidth js/window)))

(defn use-breakpoint
  "React hook: the current breakpoint string, live across window resizes."
  []
  (useSyncExternalStore subscribe snapshot))
