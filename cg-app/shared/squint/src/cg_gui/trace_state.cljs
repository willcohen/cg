;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.trace-state
  "The state and the progress that a trace event gives the card with the id
   of its pid. A proc event lights the card of a step. A def event lights the
   loader card of a def, binding-<name>. No React.")

(defn node-state
  "The card state for the event `kind`: \"running\", \"complete\", \"error\",
   or nil for an event that changes no state."
  [kind]
  (case kind
    ("proc-start" "def-start") "running"
    ("proc-complete" "def-complete") "complete"
    ("proc-error" "def-error") "error"
    nil))

(defn node-progress
  "#js {:done :total} for a rows-progress or a def-progress event, else nil. A
   def counts the rows of its fetches, and has no total."
  [^js evt]
  (case (aget evt "kind")
    "rows-progress" #js {:done (aget evt "done") :total (aget evt "total")}
    "def-progress" #js {:done (aget evt "done") :total nil}
    nil))

(defn- num-text [n] (.toLocaleString n "en-US"))

(defn progress-text
  "The stat line of a card: \"<done> / <total> rows\", or \"<done> rows\" with
   no total."
  [^js p]
  (let [total (aget p "total")]
    (if (some? total)
      (str (num-text (aget p "done")) " / " (num-text total) " rows")
      (str (num-text (aget p "done")) " rows"))))

(defn progress-pct
  "The width of the progress bar of a card, or nil with no total."
  [^js p]
  (let [total (aget p "total")]
    (when (some? total)
      (str (if (> total 0)
             (js/Math.round (* 100 (/ (aget p "done") total)))
             0)
           "%"))))
