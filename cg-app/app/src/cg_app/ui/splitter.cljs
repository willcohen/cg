;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.splitter
  "The Carbon Labs Resizer between the side-by-side panels. Each set of shown
   panels keeps its own widths, in memory and in localStorage."
  (:require ["react" :refer [useLayoutEffect useRef]]
            ["@carbon-labs/react-resizer" :refer [Resizer]]))

(def min-width 200)
(def ^:private storage-key "cg-panel-sizes")

(defn set-key
  "The storage key of the shown panels, a JS array in panel order."
  [shown]
  (.join shown "+"))

(defn next-shown
  "The shown panel to the right of panel `k`, or nil."
  [shown k]
  (let [i (.indexOf shown k)]
    (when (and (>= i 0) (< (inc i) (.-length shown)))
      (aget shown (inc i)))))

(defn- take-room!
  "Takes up to `want` px from the panels `ks` of `out`, nearest first, down to
   min-width each. Returns the px taken."
  [out ks want]
  (loop [i 0 taken 0]
    (if (or (>= i (.-length ks)) (>= taken want))
      taken
      (let [k (aget ks i)
            w (aget out k)
            give (max 0 (min (- want taken) (- w min-width)))]
        (aset out k (- w give))
        (recur (inc i) (+ taken give))))))

(defn moved
  "A copy of the widths `base` after the border to the right of panel `left`
   moves `dx` px. The panels on the far side shrink, nearest first, because
   with five panels on one neighbor has little room to give."
  [base shown left dx]
  (let [i (.indexOf shown left)
        out (js/Object.assign #js {} base)
        before (.reverse (.slice shown 0 (inc i)))
        after (.slice shown (inc i))]
    (if (pos? dx)
      (let [got (take-room! out after dx)]
        (aset out left (+ (aget out left) got)))
      (let [got (take-room! out before (- dx))
            right (aget after 0)]
        (aset out right (+ (aget out right) got))))
    out))

(defn percent-of-pair
  "The share of `wa` in the pair, as a whole percent."
  [wa wb]
  (if (pos? (+ wa wb))
    (js/Math.round (* 100 (/ wa (+ wa wb))))
    50))

(defn panel-style
  "The inline style of panel `k`: a flex grow equal to its stored width, or
   nil to keep the CSS share. Grow from a zero basis keeps the proportions
   when the window changes width."
  [sizes k]
  (when-let [w (when sizes (aget sizes k))]
    #js {:flex (str w " 1 0px")}))

(defn- storage [] (aget js/globalThis "localStorage"))

(defn- read-stored []
  (try
    (let [v (some-> (storage) (.getItem storage-key) (js/JSON.parse))]
      (if (and v (= "object" (js* "typeof ~{}" v)) (not (array? v))) v #js {}))
    (catch :default _ #js {})))

(defonce ^:private !sizes (atom nil))

(defn- all-sizes []
  ;; reset! returns undefined in squint, not the new value.
  (when (nil? @!sizes) (reset! !sizes (read-stored)))
  @!sizes)

(defn sizes-for
  "The widths that the reader gave the panels of set `k`, or nil."
  [k]
  (aget (all-sizes) k))

(defn- persist! []
  (try (some-> (storage) (.setItem storage-key (js/JSON.stringify (all-sizes))))
       (catch :default _ nil)))

(defn set-sizes!
  "Keeps `sizes` for set `k`, and nil gives back the default shares.
   `persist?` also writes localStorage, which a drag does only at its end."
  [k sizes persist?]
  (let [all (js/Object.assign #js {} (all-sizes))]
    (if sizes (aset all k sizes) (js-delete all k))
    (reset! !sizes all)
    (when persist? (persist!))
    nil))

(defn forget-cache!
  "Drops the widths in memory, and the next read uses localStorage."
  []
  (reset! !sizes nil)
  nil)

(defn- panel-el [k]
  (.getElementById js/document (str "cg-panel-" k)))

(defn- width-of [k]
  (if-let [el (panel-el k)]
    (.-width (.getBoundingClientRect el))
    0))

(defn- measured
  "The widths in px of the `shown` panels as they are on the screen."
  [shown]
  (let [out #js {}]
    (doseq [k shown] (aset out k (width-of k)))
    out))

(defn- commit! [^js props sizes persist?]
  (set-sizes! (set-key (.-shown props)) sizes persist?)
  ((.-onChange props)))

(defn- key-delta
  "The move of a key press. The Resizer moves Home and End by the width of a
   DOM sibling, and a hidden panel can be that sibling."
  [^js e delta]
  (case (.-key e)
    "Home" -100000
    "End" 100000
    delta))

(defn- on-resize!
  "A mouse delta counts from the press and applies to the widths of that
   moment. A key delta is one step and applies to the widths now."
  [^js props ^js drag ^js e delta]
  (let [shown (.-shown props)
        left (.-left props)]
    (if (= "keydown" (.-type e))
      (commit! props (moved (measured shown) shown left (key-delta e delta)) false)
      (when-let [base (.-current drag)]
        (commit! props (moved base shown left delta) false)))))

(defn- on-resize-end!
  "Stores the widths. The Resizer reports the end at the release of the mouse
   and 100 ms after the last key."
  [^js drag]
  (set! (.-current drag) nil)
  (.remove (.-classList (.-documentElement js/document)) "cg-resizing")
  (persist!))

(defn- on-press!
  "Keeps the widths at the press of the main button. The Resizer reports
   later moves as a delta from the press."
  [^js drag ^js props ^js e]
  (when (zero? (.-button e))
    (set! (.-current drag) (measured (.-shown props)))
    ;; A drag over the editor must not select its text.
    (.add (.-classList (.-documentElement js/document)) "cg-resizing")))

(defn- sync-aria!
  "Writes the share of the left panel on the border. The widths exist only
   after the layout, and React does not own these attributes."
  [^js el ^js props]
  (let [wa (width-of (.-left props))
        wb (width-of (.-right props))
        pct (percent-of-pair wa wb)]
    (.setAttribute el "aria-valuenow" (str pct))
    (.setAttribute el "aria-valuetext"
                   (str (.-leftLabel props) " " pct "%, "
                        (.-rightLabel props) " " (- 100 pct) "%"))))

(defn Splitter
  "The border between panel `left` and the next shown panel `right`."
  [^js props]
  (let [ref (useRef nil)
        drag (useRef nil)]
    (useLayoutEffect
     (fn []
       (when-let [el (.-current ref)] (sync-aria! el props))
       js/undefined))
    #jsx [Resizer {:ref ref
                   :orientation "vertical"
                   :className "cg-splitter"
                   :aria-controls (str "cg-panel-" (.-left props))
                   :aria-valuemin 0
                   :aria-valuemax 100
                   :aria-label (str "Border between " (.-leftLabel props)
                                    " and " (.-rightLabel props))
                   :title "Drag to change the widths. Double-click for the default widths."
                   :data-testid (str "splitter-" (.-left props) "-" (.-right props))
                   :onMouseDownCapture (fn [^js e] (on-press! drag props e))
                   :onResize (fn [^js e delta] (on-resize! props drag e delta))
                   :onResizeEnd (fn [_ _] (on-resize-end! drag))
                   :onDoubleClick (fn [_] (commit! props nil true))}]))
