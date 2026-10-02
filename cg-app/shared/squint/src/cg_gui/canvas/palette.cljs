;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.palette
  "The op search overlay: `/` opens it, Escape closes it, and Enter or a click
   drops a ghost. The query matches as a case-insensitive subsequence."
  (:require ["react" :refer [useState useEffect useRef]]
            [cg-gui.canvas.op-catalog :as catalog]))

(def ^:private overlay-bg "rgba(0,0,0,0.55)")
(def ^:private card-bg "var(--cg-surface, var(--vscode-editor-background, #1e1e1e))")
(def ^:private card-fg "var(--cg-text, var(--vscode-foreground, #d4d4d4))")
(def ^:private card-line "var(--cg-line-soft, var(--vscode-panel-border, #444))")
(def ^:private card-active "var(--cg-surface-raised, var(--vscode-list-activeSelectionBackground, #094771))")
(def ^:private mono "var(--cg-font-mono, monospace)")

(defn- all-ops
  "The built-in ops of all tabs plus the loaded user ops of the CgFlow, each as
   #js {:name :desc?}."
  [operations]
  (let [acc #js []]
    (.forEach catalog/builtins
              (fn [tab] (.forEach (aget tab "ops") (fn [op] (.push acc op)))))
    (.forEach (or operations #js [])
              (fn [op]
                (.push acc #js {:name (aget op "name")
                                :desc (str (.-length (or (aget op "inputs") #js [])) " in / "
                                           (.-length (or (aget op "outputs") #js [])) " out")})))
    acc))

(defn- subseq-match?
  "True when each char of `query` is in `nm` in order, case-insensitive. An
   empty query matches all."
  [query nm]
  (if (= "" query)
    true
    (let [q (.toLowerCase query) n (.toLowerCase nm)
          qlen (.-length q) nlen (.-length n)]
      (loop [qi 0 ni 0]
        (cond
          (= qi qlen) true
          (= ni nlen) false
          (= (.charAt q qi) (.charAt n ni)) (recur (inc qi) (inc ni))
          :else (recur qi (inc ni)))))))

(defn- jitter [] (+ 200 (js/Math.floor (* (js/Math.random) 120))))

(defn Palette
  "props: {operations, onAddGhost (fn [name x y]), onClose (fn [])}."
  [^js props]
  (let [operations   (aget props "operations")
        on-add-ghost (aget props "onAddGhost")
        on-close     (aget props "onClose")
        [query set-query]       (useState "")
        [selected set-selected] (useState 0)
        input-ref    (useRef nil)
        results      (.filter (all-ops operations)
                              (fn [op] (subseq-match? query (aget op "name"))))
        n-results    (.-length results)
        insert! (fn [op]
                  (when op (on-add-ghost (aget op "name") (jitter) (jitter)))
                  (on-close))]
    (useEffect (fn [] (when-let [el (.-current input-ref)] (.focus el)) js/undefined) #js [])
    (useEffect (fn [] (when (>= selected n-results) (set-selected 0)) js/undefined)
               #js [n-results selected])
    #jsx [:div {:style #js {:position "fixed" :top 0 :left 0 :right 0 :bottom 0
                            :background overlay-bg :zIndex 1000 :display "flex"
                            :alignItems "flex-start" :justifyContent "center" :paddingTop "80px"}
                :onClick (fn [_] (on-close))}
          [:div {:onClick (fn [^js e] (.stopPropagation e))
                 :style #js {:width "520px" :maxHeight "60vh" :background card-bg :color card-fg
                             :border (str "1px solid " card-line) :borderRadius "4px"
                             :boxShadow "0 8px 24px rgba(0,0,0,0.4)" :display "flex"
                             :flexDirection "column" :overflow "hidden"}}
           [:input {:ref input-ref :value query :placeholder "Search ops…"
                    :onChange (fn [^js e] (set-query (.. e -target -value)) (set-selected 0))
                    :onKeyDown
                    (fn [^js e]
                      (let [k (.-key e)]
                        (cond
                          (= "Escape" k) (do (.preventDefault e) (on-close))
                          (= "Enter" k)  (do (.preventDefault e) (insert! (aget results selected)))
                          (= "ArrowDown" k)
                          (do (.preventDefault e)
                              (set-selected (fn [i] (if (> n-results 0) (mod (inc i) n-results) 0))))
                          (= "ArrowUp" k)
                          (do (.preventDefault e)
                              (set-selected (fn [i] (if (> n-results 0) (mod (+ i (dec n-results)) n-results) 0)))))))
                    :style #js {:padding "10px 14px" :background card-bg :color card-fg
                                :border "none" :borderBottom (str "1px solid " card-line)
                                :fontFamily mono :fontSize "13px" :outline "none"}}]
           [:div {:style #js {:overflowY "auto" :maxHeight "calc(60vh - 44px)"
                              :fontFamily mono :fontSize "12px"}}
            (if (= 0 n-results)
              #jsx [:div {:style #js {:padding "12px 14px" :opacity 0.6}} "No matches"]
              (.map results
                    (fn [op i]
                      #jsx [:div {:key (aget op "name")
                                  :onClick (fn [_] (insert! op))
                                  :onMouseEnter (fn [_] (set-selected i))
                                  :style #js {:padding "5px 14px"
                                              :background (when (= i selected) card-active)
                                              :cursor "pointer" :display "flex"
                                              :justifyContent "space-between" :gap "12px"}}
                            [:span (aget op "name")]
                            (when (aget op "desc")
                              #jsx [:span {:style #js {:opacity 0.55}} (aget op "desc")])])))]]]))
