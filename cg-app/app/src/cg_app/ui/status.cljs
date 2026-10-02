;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.status
  "The file status line of the header and the run-status store. Bus messages
   go through the shared run-reducer into run-state."
  (:require ["../../../../shared/squint/src/cg_gui/run_status.mjs" :as rs]))

;; A warning is no error state: the parser warns on a form that the canvas
;; cannot model, and the file still runs.
(defn- error? [d] (= "error" (aget d "severity")))

(defn diagnostic-counts
  "#js {:errors :warnings} of parser diagnostics. Each severity other than
   \"error\" counts as a warning."
  [diags]
  (let [ds (or diags #js [])
        errors (.-length (.filter ds error?))]
    #js {:errors errors :warnings (- (.-length ds) errors)}))

(defn- plural [n word]
  (str n " " word (when (not= 1 n) "s")))

(defn counts-text
  "\"1 error, 2 warnings\", \"2 warnings\", or nil when both counts are 0."
  [counts]
  (let [e (aget counts "errors")
        w (aget counts "warnings")
        parts (.filter #js [(when (pos? e) (plural e "error"))
                            (when (pos? w) (plural w "warning"))]
                       some?)]
    (when (pos? (.-length parts)) (.join parts ", "))))

(defn- diagnostic-line [d]
  (str (aget d "severity") ": " (aget d "message")))

(defn status-line
  "#js {:text :kind :title} for the header. :kind is \"error\", \"warning\",
   \"ok\" or \"empty\". :title lists the diagnostics for a hover."
  [load-error file-name diags]
  (let [counts (diagnostic-counts diags)
        summary (counts-text counts)]
    (cond
      load-error #js {:text (str "error: " load-error) :kind "error" :title load-error}
      (nil? file-name) #js {:text "no file loaded" :kind "empty" :title nil}
      :else #js {:text (if summary (str file-name " · " summary) file-name)
                 :kind (cond
                         (pos? (aget counts "errors")) "error"
                         (pos? (aget counts "warnings")) "warning"
                         :else "ok")
                 :title (when summary
                          (.join (.map (or diags #js []) diagnostic-line) "\n"))})))

;; `live` takes each event. `run-state` is what the views read.
(defonce ^:private live (atom rs/idle-run))
(defonce run-state (atom rs/idle-run))
(defonce ^:private publish (atom {:at 0 :timer nil}))

(def ^:private publish-gap-ms 150)

;; A run sends these kinds many times each second.
(def ^:private frequent-kinds
  #js ["rows-progress" "worker-busy-start" "worker-busy-end" "flow-snapshot"])

(defn- publish! []
  (when-let [timer (:timer @publish)] (js/clearTimeout timer))
  (reset! publish {:at (js/Date.now) :timer nil})
  (reset! run-state @live))

(defn- frequent? [m]
  (let [evt (aget m "event")]
    (and evt (.includes frequent-kinds (aget evt "kind")))))

(defn- publish-soon!
  "Publishes now, or at most one time in each publish-gap-ms for a frequent
   event. A step start or a stage publishes now: the run can hold the thread."
  [m]
  (let [wait (- publish-gap-ms (- (js/Date.now) (:at @publish)))]
    (cond
      (or (not (frequent? m)) (<= wait 0)) (publish!)
      (nil? (:timer @publish)) (swap! publish assoc :timer (js/setTimeout publish! wait)))))

(def ^:private turn-ms
  "How long the status bar shows one of the items that run at the same time.
   Two seconds reads a short title and its bar, and three items come around
   in six seconds."
  2000)

(defonce ^:private turn-timer (atom nil))

(defn- next-turn! []
  (swap! live rs/show-next)
  (publish!))

(defn- keep-turns!
  "Runs the turn timer while more than one item of the run shows."
  []
  (let [on? (rs/taking-turns? @live)
        timer @turn-timer]
    (cond
      (and on? (nil? timer)) (reset! turn-timer (js/setInterval next-turn! turn-ms))
      (and (not on?) timer) (do (js/clearInterval timer)
                                (reset! turn-timer nil))
      :else nil)))

(defn reset-run! []
  (reset! live rs/idle-run)
  (keep-turns!)
  (publish!))

(defn subscribe-run
  "Registers `cb` for each change of the run status. Returns the unsubscribe fn."
  [cb]
  (let [k (str "run-sub-" (js/Math.random))]
    (add-watch run-state k (fn [_ _ _ _] (cb)))
    (fn [] (remove-watch run-state k))))

(defn- on-bus-message [^js e]
  (let [m (.-data e)]
    (when (and m (aget m "type"))
      (let [prev @live
            nxt (rs/run-reducer prev m (js/Date.now))]
        (when-not (identical? nxt prev)
          (reset! live nxt)
          (keep-turns!)
          (publish-soon! m))))))

(defn listen!
  "Folds the messages of the window bus into run-state."
  []
  (.addEventListener js/window "message" on-bus-message))
