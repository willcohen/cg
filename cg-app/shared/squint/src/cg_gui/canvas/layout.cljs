;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.layout
  "ELK places the top-level cards by their edges. The steps of a thread frame
   are stacked here, and other child nodes keep their positions from convert."
  (:require ["elkjs/lib/elk.bundled.js$default" :as ELK]))

(def ^:private elk (ELK.))

;; The box for a node with no :style size. ELK needs only approximate boxes to
;; prevent overlap. React Flow measures the real size after render.
(def ^:private default-w 240)
(def ^:private default-h 52)

(defn- layout-opts
  "The ELK options for a direction (\"DOWN\" or \"RIGHT\"); `compact?` makes the
   layer gap smaller. The gap is graph level, because elkjs ignores it per node."
  [direction compact?]
  #js {"elk.algorithm" "layered"
       "elk.direction" (or direction "DOWN")
       "elk.layered.spacing.nodeNodeBetweenLayers" (if compact? "28" "56")
       "elk.spacing.nodeNode" "50"
       "elk.layered.spacing.edgeNodeBetweenLayers" "30"
       ;; ELK lays out each connected component by itself and packs the
       ;; components into rows of about this aspect ratio, the component of
       ;; the tallest thread first. One layered graph for the whole file is
       ;; several screens wide, and its fitted view is mostly empty.
       "elk.separateConnectedComponents" "true"
       "elk.aspectRatio" "1.4"
       "elk.spacing.componentComponent" "48"})

(defn- top-level?
  "ELK positions only top-level nodes; body subnodes (parentId) stay relative."
  [n]
  (not (aget n "parentId")))

(defn- pinned?
  "True for a node with an overlay position (:pinned? in :data, from convert).
   ELK must not move it, and its edges do not go into the layout."
  [n]
  (let [d (aget n "data")]
    (boolean (and d (aget d "pinned?")))))

(defn- node-size
  "#js [w h] for ELK. Call-sites carry a body-sized :style; everything else gets
   the default box."
  [n]
  (let [st (aget n "style")
        w  (or (aget n "width") (and st (aget st "width")) default-w)
        h  (or (and st (aget st "height")) default-h)]
    #js [w h]))

(defn- collapsed?
  "canvas.cljs drops the :style of a closed container before the layout."
  [n]
  (let [d (aget n "data")]
    (boolean (and d (aget d "has-body?") (not (aget n "style"))))))

(defn- with-collapsed-width
  "On the node and not in :style, because canvas.cljs drops the :style of a
   closed container on each render."
  [n]
  (let [w (when (collapsed? n) (aget (aget n "data") "collapsed-width"))]
    (if w
      (js/Object.assign #js {} n #js {:width w})
      n)))

(def ^:private frame-pad 16)
(def ^:private frame-head 48)
(def ^:private port-height 24)
(def ^:private thread-gap 28)
(def ^:private band-inset 10)
(def ^:private band-head 22)

(defn- frame?
  [n]
  (let [d (aget n "data")]
    (boolean (and d (aget d "thread-frame?")))))

(defn- step-box
  [n]
  (let [d  (aget n "data")
        st (aget n "style")]
    (if (collapsed? n)
      #js [(aget d "collapsed-width") (aget d "collapsed-height")]
      #js [(aget st "width") (aget st "height")])))

(defn- per-row-runs
  "Index ranges #js [first last] of each run of two or more per-row steps."
  [steps]
  (let [runs  #js []
        start (atom nil)
        close (fn [i]
                (when (and (some? @start) (> (- i @start) 1))
                  (.push runs #js [@start (dec i)]))
                (reset! start nil))]
    (.forEach steps
              (fn [n i]
                (if (aget (aget n "data") "per-row?")
                  (when (nil? @start) (reset! start i))
                  (close i))))
    (close (.-length steps))
    runs))

(defn- band-rects
  [steps ys boxes x0 col-w]
  (.map (per-row-runs steps)
        (fn [r]
          (let [a   (aget r 0)
                b   (aget r 1)
                top (- (aget ys a) band-head)
                bot (+ (aget ys b) (aget (aget boxes b) 1) band-inset)]
            #js {:x (- x0 band-inset) :y top :w (+ col-w (* 2 band-inset)) :h (- bot top)}))))

(defn- stack-thread
  "Puts the steps of a frame on one vertical line in source order, and sizes
   the frame. Returns #js [frame steps]."
  [frame steps]
  (let [d      (aget frame "data")
        x0     (+ frame-pad (or (aget d "inputs-width") 0))
        boxes  (.map steps step-box)
        col-w  (.reduce boxes (fn [m b] (max m (aget b 0))) (or (aget d "column-width") 0))
        cx     (+ x0 (/ col-w 2))
        src-y  (+ frame-head 4)
        ys     #js []
        res-y  (.reduce boxes
                        (fn [y b] (.push ys y) (+ y (aget b 1) thread-gap))
                        (+ src-y port-height thread-gap))
        table? (= "table" (aget d "mode"))
        bands  (if table? (band-rects steps ys boxes x0 col-w) #js [])
        placed (.map steps
                     (fn [n i]
                       (let [b   (aget boxes i)
                             pos (if (aget (aget n "data") "pinned?")
                                   (aget n "position")
                                   #js {:x (- cx (/ (aget b 0) 2)) :y (aget ys i)})
                             ;; A side input wire runs from the left edge of
                             ;; the frame to the card.
                             data (js/Object.assign #js {} (aget n "data")
                                                    #js {:wire-reach (aget pos "x")})
                             extra (if (collapsed? n)
                                     #js {:position pos :data data
                                          :width (aget b 0) :height (aget b 1)}
                                     #js {:position pos :data data})]
                         (js/Object.assign #js {} n extra))))
        reach  (.reduce placed
                        (fn [acc n i]
                          (let [p (aget n "position")
                                b (aget boxes i)]
                            #js [(max (aget acc 0) (+ (aget p "x") (aget b 0)))
                                 (max (aget acc 1) (+ (aget p "y") (aget b 1)))]))
                        #js [0 0])
        w      (max (+ x0 col-w frame-pad (if (> (.-length bands) 0) band-inset 0))
                    (or (aget d "head-width") 0)
                    (+ (aget reach 0) frame-pad))
        h      (max (+ res-y port-height frame-pad)
                    (+ (aget reach 1) frame-pad))
        geom   #js {:cx cx :src-y src-y :res-y res-y :port-h port-height :bands bands}]
    #js [(js/Object.assign #js {} frame
                           #js {:style #js {:width w :height h}
                                :data (js/Object.assign #js {} d #js {:geom geom})})
         placed]))

(defn- by-thread-index
  [a b]
  (- (aget (aget a "data") "thread-index") (aget (aget b "data") "thread-index")))

(defn stack-threads
  "Each thread frame of `nodes` stacked and sized; all other nodes pass
   through."
  [nodes]
  (let [kids #js {}]
    (.forEach nodes
              (fn [n]
                (let [p (aget n "parentId")
                      d (aget n "data")]
                  (when (and p d (aget d "in-thread?"))
                    (when-not (aget kids p) (aset kids p #js []))
                    (.push (aget kids p) n)))))
    (let [done #js {}]
      (.forEach nodes
                (fn [n]
                  (when (frame? n)
                    (let [steps (.sort (or (aget kids (aget n "id")) #js []) by-thread-index)
                          r     (stack-thread n steps)]
                      (aset done (aget n "id") (aget r 0))
                      (.forEach (aget r 1) (fn [s] (aset done (aget s "id") s)))))))
      (.map nodes (fn [n] (or (aget done (aget n "id")) n))))))

(defn- top-of
  "The top-level ancestor of node `id`. An edge into a step counts as an edge
   into its frame, because ELK lays out only top-level nodes."
  [parents id]
  (loop [i id]
    (let [p (aget parents i)]
      (if p (recur p) i))))

(defn ^:async layout!
  "Positions the canvas nodes with ELK. Returns a Promise of the nodes, with a new
   :position on each unpinned top-level node, or of the input nodes on an error."
  [in-nodes edges direction]
  (let [nodes    (stack-threads (.map in-nodes with-collapsed-width))
        parents  (let [m #js {}]
                   (.forEach nodes (fn [n] (when-let [p (aget n "parentId")]
                                             (aset m (aget n "id") p))))
                   m)
        tops     (.filter nodes (fn [n] (and (top-level? n) (not (pinned? n)))))
        chips    (.filter tops (fn [n]
                                 (let [d (aget n "data")]
                                   (boolean (and d (aget d "chip?"))))))
        compact? (and (> (.-length tops) 0)
                      (>= (* 2 (.-length chips)) (.-length tops)))
        top-ids  (let [s #js {}]
                   (.forEach tops (fn [n] (aset s (aget n "id") true)))
                   s)
        elk-edges (.filter
                   (.map edges
                         (fn [e]
                           #js {:id (or (aget e "id")
                                        (str (aget e "source") "->" (aget e "target")))
                                :sources #js [(top-of parents (aget e "source"))]
                                :targets #js [(top-of parents (aget e "target"))]}))
                   (fn [e]
                     (let [s (aget (aget e "sources") 0)
                           t (aget (aget e "targets") 0)]
                       (and (not= s t) (aget top-ids s) (aget top-ids t)))))
        ;; The component of a thread comes first: the first view of a large file
        ;; must show a thread.
        children (.map tops
                       (fn [n]
                         (let [wh (node-size n)]
                           #js {:id (aget n "id")
                                :width (aget wh 0)
                                :height (aget wh 1)
                                :layoutOptions
                                #js {"elk.priority" (str (if (frame? n) (aget wh 1) 0))}})))
        graph #js {:id "root"
                   :layoutOptions (layout-opts direction compact?)
                   :children children
                   :edges elk-edges}]
    (try
      (let [laid (await (.layout elk graph))
            pos  (let [m #js {}]
                   (.forEach (aget laid "children")
                             (fn [c]
                               (aset m (aget c "id") #js {:x (aget c "x") :y (aget c "y")})))
                   m)]
        (.map nodes
              (fn [n]
                (let [p (aget pos (aget n "id"))]
                  (if p
                    (js/Object.assign #js {} n #js {:position p})
                    n)))))
      (catch :default _e
        nodes))))
