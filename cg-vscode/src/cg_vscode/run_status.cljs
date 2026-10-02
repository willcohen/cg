;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.run-status
  "The run status of the canvas runs, on the status bar and in the CG Run
   Status tab. No vscode require: the host puts the bar and tab in the state."
  (:require [cg-vscode.status-html :as status-html]
            ["../../../cg-app/shared/squint/src/cg_gui/run_status.mjs" :as rs]))

(def ^:private draw-gap-ms 150)

(defn bar-text
  "The status bar text of a run-view, with a codicon for its kind."
  [^js view]
  (let [title (aget view "title")]
    (case (aget view "kind")
      "running" (str "$(sync~spin) " title " · " (aget view "clock"))
      "ok"      (str "$(check) " title)
      "error"   (str "$(error) " title)
      title)))

(defn bar-tooltip
  "The hover text of a run-view: its detail, and the label of the flow of the
   run."
  [run ^js view]
  (let [parts (.filter #js [(aget view "detail") (rs/current-flow-label run)]
                       (fn [p] (and (some? p) (not= "" p))))]
    (if (pos? (.-length parts)) (.join parts "\n") (aget view "title"))))

(defn draw!
  "Shows the run status on the status bar item and in the tab, when it is
   open. Hides the item when there is no run to show."
  [!state now]
  (let [st @!state
        run (or (:run-status/run st) rs/idle-run)
        view (rs/run-view run nil now)]
    (when-let [^js bar (:run-status/bar st)]
      (if view
        (do (set! (.-text bar) (bar-text view))
            (set! (.-tooltip bar) (bar-tooltip run view))
            (.show bar))
        (.hide bar)))
    (when-let [^js panel (:run-status/panel st)]
      (set! (.. panel -webview -html)
            (status-html/build-html run now (:run-status/nonce st))))))

(defn- stop-clock! [!state]
  (when-let [t (:run-status/clock @!state)]
    (js/clearInterval t)
    (swap! !state dissoc :run-status/clock)))

(defn- start-clock!
  "Draws each second while a run is on, for the clock of the status bar."
  [!state]
  (when-not (:run-status/clock @!state)
    (swap! !state assoc :run-status/clock
           (js/setInterval (fn [] (draw! !state (js/Date.now))) 1000))))

(defn- draw-soon!
  "Draws at most one time in each draw-gap-ms, because a run sends many trace
   events each second."
  [!state]
  (when-not (:run-status/timer @!state)
    (swap! !state assoc :run-status/timer
           (js/setTimeout (fn []
                            (swap! !state dissoc :run-status/timer)
                            (draw! !state (js/Date.now)))
                          draw-gap-ms))))

(defn note!
  "Folds the run message `msg` into the run status. A trace event draws soon,
   and a message that changes nothing does not draw."
  [!state ^js msg]
  (let [now (js/Date.now)
        prev (or (:run-status/run @!state) rs/idle-run)
        run (rs/run-reducer prev msg now)]
    (when-not (identical? run prev)
      (swap! !state assoc :run-status/run run)
      (if (= "running" (:phase run))
        (do (start-clock! !state)
            (if (= "cg/trace-event" (aget msg "type"))
              (draw-soon! !state)
              (draw! !state now)))
        (do (stop-clock! !state)
            (draw! !state now))))))

(defn post-to!
  "Posts the run message `msg` to the canvas `panel` and notes it."
  [!state ^js panel ^js msg]
  (when panel (.. panel -webview (postMessage msg)))
  (note! !state msg))

(defn post!
  "Posts the run message `msg` to the canvas of the document `uri-str`, when
   that canvas is open, and notes it."
  [!state uri-str ^js msg]
  (post-to! !state (get-in @!state [:editor/panels uri-str]) msg))

(defn stop!
  "Stops the timers of the run status."
  [!state]
  (stop-clock! !state)
  (when-let [t (:run-status/timer @!state)]
    (js/clearTimeout t)
    (swap! !state dissoc :run-status/timer)))
