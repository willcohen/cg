;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.status-view
  "The run status in two views of one state: the bar under the header and
   the Status panel."
  (:require ["react" :refer [useEffect useState useSyncExternalStore]]
            [cg-app.ui.status :as status]
            ["../../../../shared/squint/src/cg_gui/run_status.mjs" :as rs]
            [cg-app.ui.carbon :as c]))

(defn- use-run-state []
  (useSyncExternalStore status/subscribe-run (fn [] @status/run-state)))

(defn- use-tick!
  "Renders the caller again each second while `on?`."
  [on?]
  (let [[_ set-n] (useState 0)]
    (useEffect
     (fn []
       (if on?
         (let [id (js/setInterval (fn [] (set-n (fn [n] (inc n)))) 1000)]
           (fn [] (js/clearInterval id)))
         js/undefined))
     #js [on?])))

(defn- Mark [^js props]
  (case (.-kind props)
    "running" #jsx [c/InlineLoading {:className "cg-run-mark" :status "active"
                                     :description "" :iconDescription "Running"}]
    "ok"      #jsx [c/CheckmarkIcon {:className "cg-run-mark" :size 16}]
    #jsx [c/ErrorIcon {:className "cg-run-mark" :size 16}]))

(defn- Bar [^js props]
  #jsx [:div {:className "cg-run-bar" :data-testid "run-progress"}
        [c/ProgressBar {:label (.-label props) :hideLabel true :size "small"
                        :value (.-n props) :max (.-total props)}]])

(defn- Position
  "\"(2/3)\": the place of the shown item when more than one runs."
  [^js props]
  (when-let [text (.-text props)]
    #jsx [:span {:className "cg-run-position" :data-testid (.-testid props)} text]))

(defn- ShowButton [^js props]
  #jsx [c/Button {:kind "ghost" :size "sm" :className "cg-run-action"
                  :data-testid (.-testid props)
                  :onClick (.-onClick props)}
        (.-label props)])

(defn RunStatus
  "props: {exec (the :exec map of app state), outputs (#js {:map :results},
   the hidden panels with run output), onShow (fn [panel-key]), onDismiss}."
  [^js props]
  (let [run  (use-run-state)
        exec (.-exec props)
        v    (rs/run-view run exec (js/Date.now))
        kind (when v (.-kind v))
        outs (or (.-outputs props) #js {})]
    (use-tick! (= "running" kind))
    (when v
      #jsx [:div {:className "cg-run-status" :role "status"
                  :data-testid "exec-status"
                  :data-exec-status (or (:status exec) kind)
                  :data-run-kind kind}
            [Mark {:kind kind}]
            [:span {:className "cg-run-title" :data-testid "run-title"} (.-title v)]
            [Position {:text (.-position v) :testid "run-position"}]
            (when (some? (.-total v))
              #jsx [Bar {:label (.-title v) :n (.-n v) :total (.-total v)}])
            (when (.-detail v)
              #jsx [:span {:className "cg-run-detail" :data-testid "run-detail"
                           :title (.-detail v)}
                    (.-detail v)])
            (when (and (= "ok" kind) (aget outs "map"))
              #jsx [ShowButton {:label "Show the map" :testid "run-show-map"
                                :onClick (fn [_] ((.-onShow props) "map"))}])
            (when (and (= "ok" kind) (aget outs "results"))
              #jsx [ShowButton {:label "Show the results" :testid "run-show-results"
                                :onClick (fn [_] ((.-onShow props) "results"))}])
            [:span {:className "cg-run-spacer"}]
            (when (.-clock v)
              #jsx [:span {:className "cg-run-elapsed"}
                    "Elapsed "
                    [:span {:className "cg-run-clock" :data-testid "run-clock"}
                     (.-clock v)]])
            (when-not (= "running" kind)
              #jsx [:button {:className "cg-run-dismiss" :aria-label "Dismiss"
                             :title "Dismiss" :data-testid "run-dismiss"
                             :onClick (fn [_] ((.-onDismiss props)))}
                    [c/CloseIcon {:size 16}]])])))

(defn- NowBlock [^js props]
  (let [v (.-view props)
        kind (.-kind v)]
    #jsx [:section {:className "cg-runpane-now" :data-run-kind kind :data-testid "run-now"}
          [:div {:className "cg-runpane-now-head"}
           [Mark {:kind kind}]
           [:span {:className "cg-run-title"} (.-title v)]
           [Position {:text (.-position v)}]
           [:span {:className "cg-run-spacer"}]
           (when (.-clock v)
             #jsx [:span {:className "cg-run-elapsed"}
                   "Elapsed "
                   [:span {:className "cg-run-clock"} (.-clock v)]])]
          (when (some? (.-total v))
            #jsx [Bar {:label (.-title v) :n (.-n v) :total (.-total v)}])
          (when (.-detail v)
            #jsx [:p {:className "cg-runpane-detail"} (.-detail v)])]))

(defn- StepRow [^js props]
  (let [r (.-row props)]
    #jsx [:li {:className "cg-step" :data-state (.-state r) :data-testid "run-step"}
          [:span {:className "cg-step-dot" :aria-hidden "true"}]
          [:span {:className "cg-step-label"} (.-label r)]
          [:span {:className "cg-step-state"} (if (= "error" (.-state r)) "failed" (.-state r))]
          [:span {:className "cg-step-time"} (.-time r)]
          (when (or (.-count r) (.-rows r) (.-queued r))
            #jsx [:span {:className "cg-step-more"}
                  (.join (.filter #js [(.-count r) (.-rows r) (.-queued r)] some?) " · ")])
          (when (some? (.-total r))
            #jsx [:span {:className "cg-step-bar"}
                  [c/ProgressBar {:label (.-label r) :hideLabel true :size "small"
                                  :value (or (.-done r) 0) :max (.-total r)}]])]))

(defn- WorkerCell [^js props]
  (let [w (.-cell props)]
    #jsx [:li {:className "cg-worker" :data-state (.-state w) :data-testid "run-worker"}
          [:div {:className "cg-worker-head"}
           [:span {:className "cg-step-dot" :aria-hidden "true"}]
           [:span {:className "cg-worker-name"} (.-name w)]
           (when (and (.-handler w) (not= "main" (.-handler w)))
             #jsx [c/Tag {:size "sm" :type "cool-gray" :className "cg-worker-tag"} (.-handler w)])
           [:span {:className "cg-step-state"} (.-state w)]]
          [:div {:className "cg-worker-work"}
           (if (= "busy" (.-state w))
             (.-label w)
             (str (.-jobs w) (if (= 1 (.-jobs w)) " job done" " jobs done")))]
          (when (and (= "busy" (.-state w)) (.-step w))
            #jsx [:div {:className "cg-worker-step"} (str "for step " (.-step w))])]))

(defn- FlowRow [^js props]
  (let [f (.-row props)]
    #jsx [:li {:className "cg-step" :data-state (.-state f) :data-testid "run-flow"}
          [:span {:className "cg-step-dot" :aria-hidden "true"}]
          [:span {:className "cg-step-label"} (.-label f)]
          [:span {:className "cg-step-state"} (if (= "error" (.-state f)) "failed" "done")]
          [:span {:className "cg-step-time"} (.-time f)]
          [:span {:className "cg-step-more"} (.-detail f)]]))

(defn RunPane
  "The Status panel. props: {exec <the :exec map of the app state>}."
  [^js props]
  (let [run     (use-run-state)
        now     (js/Date.now)
        v       (rs/run-view run (.-exec props) now)
        running (and v (= "running" (.-kind v)))
        steps   (rs/step-rows run now)
        workers (rs/worker-cells run)
        flows   (rs/flow-rows run)]
    (use-tick! running)
    #jsx [:div {:className "cg-runpane" :data-testid "run-pane"}
          (if v
            #jsx [NowBlock {:view v}]
            #jsx [:p {:className "cg-runpane-empty" :data-testid "run-pane-empty"}
                  "Nothing runs. Click Run, and this panel shows what the run does: its stage, the steps of each flow, and the workers."])
          (when (or running (pos? (.-length steps)))
            #jsx [:section {:data-testid "run-steps"}
                  [:h3 {:className "cg-runpane-h"}
                   "Steps"
                   (when-let [label (rs/current-flow-label run)]
                     #jsx [:span {:className "cg-runpane-h-of"} (str " of " label)])]
                  (if (pos? (.-length steps))
                    #jsx [:ol {:className "cg-steps"}
                          (.map steps (fn [r i] #jsx [StepRow {:key (str i "-" (.-label r)) :row r}]))]
                    #jsx [:p {:className "cg-runpane-note"} "No flow runs at this moment."])])
          (when (pos? (.-length workers))
            #jsx [:section {:data-testid "run-workers"}
                  [:h3 {:className "cg-runpane-h"} "Workers"]
                  [:ul {:className "cg-workers"}
                   (.map workers (fn [w] #jsx [WorkerCell {:key (.-id w) :cell w}]))]])
          (when (pos? (.-length flows))
            #jsx [:section {:data-testid "run-flows"}
                  [:h3 {:className "cg-runpane-h"} "Flows that ended"]
                  [:ol {:className "cg-steps"}
                   (.map flows (fn [f i] #jsx [FlowRow {:key (str i) :row f}]))]])]))
