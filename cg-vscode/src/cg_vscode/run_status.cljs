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

(defn bar-text [^js view]
  (let [title (aget view "title")]
    (case (aget view "kind")
      "running" (str "$(sync~spin) " title " · " (aget view "clock"))
      "ok"      (str "$(check) " title)
      "error"   (str "$(error) " title)
      title)))

(defn bar-tooltip [run ^js view]
  (let [parts (.filter #js [(aget view "detail") (rs/current-flow-label run)]
                       (fn [p] (and (some? p) (not= "" p))))]
    (if (pos? (.-length parts)) (.join parts "\n") (aget view "title"))))

(defn draw! [!state now]
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

(defn note! [!state ^js msg]
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

(defn- clock [now]
  (.slice (.toTimeString (js/Date. now)) 0 8))

(defn- file-name [path]
  (some-> path (.split #"[\\/]") last))

(defn- stage-text
  "A progress event gives no line, because a fetch sends one for each page."
  [^js evt]
  (let [label (aget evt "label")
        rows  (fn [n] (when (number? n) (str " (" n " rows)")))]
    (case (aget evt "state")
      "start" (str "  " label (rows (aget evt "total")) "\n")
      "done"  (str "  " label ": done" (rows (aget evt "n")) "\n")
      nil)))

(defn channel-text
  "The text that the run message `msg` adds to the CG Run output channel, or
   nil. Of the trace events, only a stage, a flow with failed rows and the
   printed output of a run add text, because a run sends many events."
  [^js msg now]
  (case (aget msg "type")
    "cg/run-started" (str "[" (clock now) "] started " (file-name (aget msg "file"))
                          (when-let [rt (aget msg "runtime")] (str " on " rt)) "\n")
    "cg/trace-event" (let [evt (aget msg "event")
                           kind (when evt (aget evt "kind"))]
                       (cond
                         (= "stage" kind) (stage-text evt)
                         (= "output" kind) (aget evt "text")
                         (and (= "flow-complete" kind) (aget evt "failed-rows"))
                         (str "  " (aget evt "flow") ": " (aget evt "failed-rows") " of "
                              (aget evt "total-rows") " rows failed: "
                              (aget evt "first-error") "\n")))
    "cg/run-output" (aget msg "text")
    "cg/run-error" (str "failed: " (aget msg "message") "\n")
    nil))

(defn- append-to-channel!
  "A run start shows the channel, with no change of focus."
  [!state ^js msg]
  (when-let [^js ch (:run-status/channel @!state)]
    (when-let [text (channel-text msg (js/Date.now))]
      (when (= "cg/run-started" (aget msg "type")) (.show ch true))
      (.append ch text))))

(defn post-to! [!state ^js panel ^js msg]
  (when panel (.. panel -webview (postMessage msg)))
  (note! !state msg)
  (append-to-channel! !state msg))

(defn post! [!state uri-str ^js msg]
  (post-to! !state (get-in @!state [:editor/panels uri-str]) msg))

(defn requested!
  "A request with no line after it stopped before its run started."
  [!state uri-str path runtime]
  (when-let [^js ch (:run-status/channel @!state)] (.show ch true))
  (post! !state uri-str
         #js {:type "cg/run-output" :stream "stdout"
              :text (str "[" (clock (js/Date.now)) "] run " (file-name path) " on " runtime "\n")}))

(defn stop! [!state]
  (stop-clock! !state)
  (when-let [t (:run-status/timer @!state)]
    (js/clearTimeout t)
    (swap! !state dissoc :run-status/timer)))
