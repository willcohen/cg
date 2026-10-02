;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.canvas
  "The @xyflow/react canvas for the CgFlow in a `cg/flow` message. The host
   loads the CSS with <link> tags, because the .mjs must stay pure JS for the CSP."
  (:require ["react" :refer [useState useEffect useRef useMemo]]
            ["react-dom/client" :refer [createRoot]]
            ["@xyflow/react" :refer [ReactFlow Background Controls MiniMap Panel applyNodeChanges]]
            ["@carbon/react" :refer [Search]]
            [cg-gui.canvas.convert :as convert]
            [cg-gui.canvas.layout :as layout]
            ;; The alias is `cn`, not `nodes`: App binds a local `nodes`, and
            ;; squint resolves `nodes/x` against the local.
            [cg-gui.canvas.nodes :as cn]
            [cg-gui.canvas.toolbar :as toolbar]
            [cg-gui.canvas.palette :as palette]
            [cg-gui.canvas.console :as console]
            [cg-gui.canvas.host :as host]))

(defn- emit-vars
  "`{key value}` JS object -> `    --<prefix><key>: <value>;` lines. \"\" for nil."
  [prefix obj]
  (if obj
    (.join (.map (js/Object.entries obj)
                 (fn [pair] (str "    --" prefix (aget pair 0) ": " (aget pair 1) ";")))
           "\n")
    ""))

(defn- apply-theme!
  "Writes the #cg-tokens <style> for `mode` from the token globals that the host
   inlined, and sets <html data-theme>."
  [mode]
  (let [tk     (aget js/window "__CG_TOKENS__")
        ck     (aget js/window "__CG_CODE_TOKENS__")
        tokens (when tk (aget tk mode))
        codes  (when ck (aget ck mode))
        body   (str (emit-vars "cg-" tokens) "\n" (emit-vars "code-" codes))
        el     (.getElementById js/document "cg-tokens")]
    (when el
      (set! (.-innerHTML el) (str ":root {\n" body "\n}\n")))
    (.setAttribute (.-documentElement js/document) "data-theme" mode)))

(def ^:private pulse-period-ms 1400)
(def ^:private pulse-max-running 12)

(defn- set-root-var! [name value]
  (.setProperty (.. js/document -documentElement -style) name (str value)))

(defn- pulse-loop
  "Writes one --cg-pulse-opacity on <html> each frame, for all running nodes to
   pulse in one phase. Returns the stop fn."
  [t0]
  (let [tick  (atom nil)
        live? (atom true)]
    (.add (.. js/document -documentElement -classList) "cg-pulse-locked")
    (reset! tick
            (fn rec [ts]
              (when @live?
                (let [n (.-length (.querySelectorAll js/document ".cg-node.running"))]
                  (if (and (pos? n) (<= n pulse-max-running))
                    (let [phase (/ (mod (- ts t0) pulse-period-ms) pulse-period-ms)
                          o     (+ 0.5 (* 0.5 (js/Math.cos (* phase 2 js/Math.PI))))]
                      (set-root-var! "--cg-pulse-opacity" o))
                    (set-root-var! "--cg-pulse-opacity" 1)))
                (js/requestAnimationFrame rec))))
    (js/requestAnimationFrame @tick)
    (fn stop! []
      (reset! live? false)
      (.remove (.. js/document -documentElement -classList) "cg-pulse-locked")
      (.removeProperty (.. js/document -documentElement -style) "--cg-pulse-opacity"))))

(defn- use-phase-lock-pulse []
  (useEffect
   (fn []
     (let [stop! (pulse-loop (.now js/performance))]
       (fn cleanup [] (stop!))))
   #js []))

(defn- ghost-id?
  "True for a ghost: a webview-local node dropped from the toolbar, with id
   `ghost-N`. The host does not persist a ghost."
  [id]
  (.startsWith (str id) "ghost-"))

(defn- reveal!
  "Reveals the source span of a node, in its module file when it has
   :source-path. Data keys keep their hyphens, because the data is clj->js output."
  [^js node]
  (let [d (.-data node)]
    (host/reveal-location! (aget d "call-site-location")
                           (aget d "source-path"))))

(defn on-node-click
  "A plain click selects the node and makes its model of `flow` the one Run runs.
   Cmd or Ctrl click reveals the source span, which moves the editor selection."
  [^js e ^js node ^js flow]
  (if (or (.-metaKey e) (.-ctrlKey e))
    (reveal! node)
    (let [d (.-data node)
          m (when d (aget d "model-name"))]
      ;; Ghosts, loader cards and op body steps carry a model-name too, and
      ;; the cg-app host does not check the name it gets.
      (when (and m
                 (some (fn [b] (and (= "threading_macro" (aget b "body-shape"))
                                    (= m (aget b "name"))))
                       (or (and flow (aget flow "bindings")) #js [])))
        (host/post! #js {:type "cg/select-model" :model m})))))

(defn on-node-context-menu
  "Stops the default webview menu on right-click."
  [^js e _node]
  (.preventDefault e))

(defn on-node-double-click
  "Double-click calls the :on-dive callback of the node. apply-collapse gives
   that callback only to a divable node."
  [_e ^js node]
  (let [d (.-data node)
        f (when d (aget d "on-dive"))]
    (when f (f))))

(defn- span-covers?
  [loc line]
  (and loc
       (<= (aget loc "start-line") line)
       (>= (aget loc "end-line") line)))

(defn- node-for-line
  "The node whose source span covers `line`. The smallest span wins, to pick a
   body row over its block. Skips a node with :source-path (a loaded module)."
  [nodes line]
  (.reduce nodes
           (fn [best ^js n]
             (let [d   (.-data n)
                   loc (when d (aget d "call-site-location"))]
               (if (and (span-covers? loc line) (not (aget d "source-path")))
                 (let [span (- (aget loc "end-line") (aget loc "start-line"))]
                   (if (or (not best) (< span (aget best "span")))
                     #js {:id (aget n "id") :parent (aget n "parentId") :span span}
                     best))
                 best)))
           nil))

(defn- select-only
  "Nodes with only `id` selected. Returns the same array when the selection
   already matches, because a caret move in one span must not make a new array."
  [nodes id]
  (if (.every nodes (fn [^js n] (= (if (.-selected n) true false)
                                   (= id (aget n "id")))))
    nodes
    (.map nodes (fn [^js n]
                  (js/Object.assign #js {} n #js {:selected (= id (aget n "id"))})))))

(defn- apply-reveal!
  "Selects the node that covers `line`, expands its container, and pans to it
   after 400 ms for the new ELK layout. Returns the new node array."
  [prev line set-expanded-overrides rf]
  (let [hit (node-for-line prev line)]
    (if-not hit
      prev
      (let [id  (aget hit "id")
            pid (aget hit "parent")]
        (when pid
          (set-expanded-overrides
           (fn [ov]
             (if (aget ov pid)
               ov
               (let [next (js/Object.assign #js {} ov)]
                 (aset next pid true)
                 next)))))
        (js/setTimeout
         (fn []
           (when-let [inst (.-current rf)]
             (.fitView inst #js {:nodes #js [#js {:id id}]
                                 :maxZoom 1 :padding 0.35 :duration 250})))
         400)
        (select-only prev id)))))

(defn on-node-drag-stop
  "Posts a cg/edit DragNode message with the new position of the dragged node.
   The host writes the position into the ^:cgproj overlay of the .cg file."
  [_e ^js node]
  (let [d          (.-data node)
        id         (.-id node)
        model-name (when d (aget d "model-name"))]
    ;; A ghost is not in the .cg file. The position of a body step is relative
    ;; to its parent, because the step has extent="parent".
    (when-not (ghost-id? id)
      (host/post! #js {:type "cg/edit"
                       :editType "DragNode"
                       :model model-name
                       :payload #js {:nodeId id
                                     :position #js {:x (.. node -position -x)
                                                    :y (.. node -position -y)}}}))))

(defn- patch-node-states
  "Sets :state (\"idle\" with no trace entry), :progress and :run-model? in each
   node's :data, by node id. An unchanged node stays the same object for React Flow."
  [nodes trace-state progress-state run-model]
  (.map nodes
        (fn [n]
          (let [id (aget n "id")
                st (or (aget trace-state id) "idle")
                pr (aget progress-state id)
                d  (.-data n)
                rm (and (some? run-model) (= run-model (aget d "model-name")))]
            (if (and (= st (aget d "state"))
                     (identical? pr (aget d "progress"))
                     (= rm (true? (aget d "run-model?"))))
              n
              (js/Object.assign #js {} n
                                #js {:data (js/Object.assign
                                            #js {} d
                                            #js {:state st :progress pr
                                                 :run-model? rm})}))))))

(defn- expanded-now?
  "The chevron state for a call-site id. With no entry in `overrides`, a parsed
   call-site is collapsed and a ghost is expanded."
  [overrides id]
  (let [v (aget overrides id)]
    (if (some? v) v (ghost-id? id))))

(defn- apply-collapse
  "The nodes and edges that React Flow renders. Hides collapsed bodies and the
   edges to them (React Flow does not), and adds chevron and dive callbacks."
  [nodes edges overrides toggle dive]
  (let [collapsed #js {}
        hidden    #js {}]
    (.forEach nodes
              (fn [n]
                (let [d (.-data n)]
                  (when (and d (aget d "has-body?"))
                    (let [id (aget n "id")]
                      (when-not (expanded-now? overrides id)
                        (aset collapsed id true)))))))
    (.forEach nodes
              (fn [n]
                (let [p (aget n "parentId")]
                  (when (and p (aget collapsed p))
                    (aset hidden (aget n "id") true)))))
    #js {:nodes
         (.map nodes
               (fn [n]
                 (let [id (aget n "id")
                       d  (.-data n)]
                   (cond
                     (and d (aget d "has-body?"))
                     (let [coll? (aget collapsed id)
                           nd    (js/Object.assign
                                  #js {} n
                                  #js {:data (js/Object.assign
                                              #js {} d
                                              #js {:expanded? (not coll?)
                                                   :on-toggle-expand (fn [] (toggle id))})})]
                       (when coll? (aset nd "style" nil))
                       (when (aget hidden id) (aset nd "hidden" true))
                       nd)

                     (and d (aget d "divable?"))
                     (let [nd (js/Object.assign
                               #js {} n
                               #js {:data (js/Object.assign
                                           #js {} d
                                           ;; The name is an argument, because a
                                           ;; step with branches names one
                                           ;; op for each branch.
                                           #js {:on-dive
                                                (fn [nm]
                                                  (dive (or nm
                                                            (aget (or (aget d "dive-names")
                                                                      #js [])
                                                                  0))))})})]
                       (when (aget hidden id) (aset nd "hidden" true))
                       nd)

                     (aget hidden id)
                     (js/Object.assign #js {} n #js {:hidden true})

                     :else n))))
         :edges
         (.map edges
               (fn [e]
                 (if (or (aget hidden (aget e "source"))
                         (aget hidden (aget e "target")))
                   (js/Object.assign #js {} e #js {:hidden true})
                   e)))}))

(defn- size-for-layout
  "Removes the :style of each collapsed call-site before ELK layout. Without
   this, ELK reserves room for the expanded body."
  [flow-nodes overrides]
  (.map flow-nodes
        (fn [n]
          (let [d (.-data n)]
            (if (and d (aget d "has-body?")
                     (not (expanded-now? overrides (aget n "id"))))
              (let [nn (js/Object.assign #js {} n)]
                (aset nn "style" nil)
                nn)
              n)))))

(def ^:private fit-opts #js {:padding 0.15 :minZoom 0.05 :maxZoom 1.5})

(def ^:private legible-px 8)

(defn- legible-zoom
  "The zoom at which a card title is legible-px tall."
  [^js pane]
  (let [head (when pane (.querySelector pane ".react-flow__node .cg-head"))
        fs   (if head (js/parseFloat (.-fontSize (js/getComputedStyle head))) 13)]
    (min 1 (/ legible-px (if (pos? fs) fs 13)))))

(defn- fit-zoom
  "The zoom at which `bounds` fits the pane with fit-opts padding."
  [^js bounds ^js pane-rect]
  (let [k (+ 1 (* 2 (.-padding fit-opts)))]
    (min (.-maxZoom fit-opts)
         (/ (.-width pane-rect) (* k (max 1 (.-width bounds))))
         (/ (.-height pane-rect) (* k (max 1 (.-height bounds)))))))

(defn- fit-legibly!
  "Fits the graph. A graph too big for legible titles shows its top-left at
   the legible zoom instead; the minimap and the search reach the rest."
  [^js inst ^js pane]
  (let [shown (.filter (.getNodes inst) (fn [^js n] (not (.-hidden n))))]
    (if (or (nil? pane) (zero? (.-length shown)))
      (.fitView inst fit-opts)
      (let [b  (.getNodesBounds inst shown)
            lz (legible-zoom pane)]
        (if (>= (fit-zoom b (.getBoundingClientRect pane)) lz)
          (.fitView inst fit-opts)
          ;; The offset keeps the top row clear of the toolbar.
          (.setViewport inst #js {:x (- 24 (* lz (.-x b))) :y (- 64 (* lz (.-y b))) :zoom lz}))))))

(defn- node-title [^js n]
  (let [d (.-data n)]
    (.toLowerCase (str (when d (aget d "label")) " " (when d (aget d "head"))))))

(defn- find-nodes
  "The shown nodes whose title has `q`, ignoring case; a title that starts
   with `q` comes first."
  [nodes q]
  (let [lq (.toLowerCase (.trim (str q)))]
    (if (= "" lq)
      #js []
      (let [rank (fn [n] (if (.startsWith (node-title n) lq) 0 1))]
        (.sort (.filter nodes (fn [^js n] (and (not (.-hidden n)) (.includes (node-title n) lq))))
               (fn [a b] (- (rank a) (rank b))))))))

(defn- NodeSearch
  "A Carbon Search that centers the canvas on the first card with the typed
   name. Enter goes to the next one."
  [^js props]
  (let [[q set-q] (useState "")
        [i set-i] (useState 0)
        nodes (.-nodes props)
        hits  (find-nodes nodes q)
        n     (.-length hits)
        go!   (fn [hs k] (when (pos? (.-length hs))
                           ((.-onPick props) (aget hs (mod k (.-length hs))))))]
    #jsx [:div {:className "cg-search" :data-testid "node-search"
                :style #js {:display "flex" :alignItems "center" :gap "6px"
                            :width "220px" :marginRight "6px"}}
          [Search {:size "sm"
                   :labelText "Find a card by name"
                   :placeholder "Find a card"
                   :closeButtonLabelText "Clear"
                   :value q
                   :onChange (fn [^js e]
                               (let [v (.. e -target -value)]
                                 (set-q v)
                                 (set-i 0)
                                 (go! (find-nodes nodes v) 0)))
                   :onKeyDown (fn [^js e]
                                (when (= "Enter" (.-key e))
                                  (set-i (inc i))
                                  (go! hits (inc i))))}]
          (when-not (= "" q)
            #jsx [:span {:data-testid "node-search-count"
                         :style #js {:fontSize "11px" :whiteSpace "nowrap"
                                     :color "var(--cds-text-secondary, var(--cg-ink-muted, #888))"}}
                  (if (pos? n) (str (inc (mod i n)) "/" n) "none")])]))

(defn- minimap-color
  "The minimap fill of a node, by its run state."
  [^js node]
  (case (when (.-data node) (aget (.-data node) "state"))
    "running"  "var(--cg-run, #0066cc)"
    "complete" "var(--cg-complete, #137a3d)"
    "error"    "var(--cg-error, #b42318)"
    "var(--cg-line, #9aa5b1)"))

(def ^:private search-min-px 640)
(def ^:private minimap-min-px 480)

;; A plain ghost's size before React Flow measures it, and its gap to a card.
(def ^:private ghost-w 240)
(def ^:private ghost-h 90)
(def ^:private ghost-gap 24)

(defn- node-rect
  "#js {:x :y :w :h} of a top-level node in flow coordinates."
  [^js n ^js internal]
  (let [m (or (and internal (.-measured internal)) (.-measured n))
        s (.-style n)
        p (.-position n)]
    #js {:x (.-x p) :y (.-y p)
         :w (or (and m (.-width m)) (and s (.-width s)) ghost-w)
         :h (or (and m (.-height m)) (and s (.-height s)) ghost-h)}))

(defn- top-level-rects
  "The rects of the shown nodes with no parent; a body step is in its parent's."
  [nodes ^js inst]
  (.map (.filter nodes (fn [^js n] (and (not (.-parentId n)) (not (.-hidden n)))))
        (fn [^js n] (node-rect n (when inst (.getInternalNode inst (.-id n)))))))

(defn- overlaps?
  [^js a ^js b]
  (and (< (.-x a) (+ (.-x b) (.-w b) ghost-gap))
       (< (.-x b) (+ (.-x a) (.-w a) ghost-gap))
       (< (.-y a) (+ (.-y b) (.-h b) ghost-gap))
       (< (.-y b) (+ (.-y a) (.-h a) ghost-gap))))

(defn- free? [rects box] (not (.some rects (fn [r] (overlaps? box r)))))

(defn- scan-area
  "The first free w x h box in `area`, row by row from its top-left, or nil."
  [rects w h ^js area]
  (let [step (max ghost-gap (/ (.-w area) 80))
        x-end (- (+ (.-x area) (.-w area)) w)
        y-end (- (+ (.-y area) (.-h area)) h)]
    (loop [y (.-y area) x (.-x area)]
      (cond
        (> y y-end) nil
        (> x x-end) (recur (+ y step) (.-x area))
        :else (let [box #js {:x x :y y :w w :h h}]
                (if (free? rects box) box (recur y (+ x step))))))))

(defn- right-of-all
  "A free w x h box in a column to the right of every rect."
  [rects w h]
  (let [x (+ ghost-gap (.reduce rects (fn [mx r] (max mx (+ (.-x r) (.-w r)))) 0))
        y0 (.reduce rects (fn [mn r] (min mn (.-y r))) (if (pos? (.-length rects)) js/Infinity 0))]
    (loop [y y0]
      (let [box #js {:x x :y y :w w :h h}]
        (if (free? rects box) box (recur (+ y ghost-gap)))))))

(defn free-spot
  "A free w x h box: the first in `area` clear of `cards` and `panels`, else
   one right of all cards."
  [cards panels w h area]
  (or (when area (scan-area (.concat cards panels) w h area))
      (right-of-all cards w h)))

(defn- screen->flow-rect
  "A client rect as #js {:x :y :w :h} in flow coordinates."
  [^js inst left top right bottom]
  (let [tl (.screenToFlowPosition inst #js {:x left :y top})
        br (.screenToFlowPosition inst #js {:x right :y bottom})]
    #js {:x (.-x tl) :y (.-y tl) :w (- (.-x br) (.-x tl)) :h (- (.-y br) (.-y tl))}))

(defn- visible-area
  "The visible part of the canvas in flow coordinates, or nil before React
   Flow is up."
  [^js inst ^js pane]
  (when (and inst pane)
    (let [r (.getBoundingClientRect pane)
          inset 16]
      (screen->flow-rect inst (+ (.-left r) inset) (+ (.-top r) inset)
                         (- (.-right r) inset) (- (.-bottom r) inset)))))

(defn- panel-rects
  "The rects of the panels over the pane, in flow coordinates."
  [^js inst ^js pane]
  (if (and inst pane)
    (.map (js/Array.from (.querySelectorAll pane ".react-flow__panel"))
          (fn [^js el]
            (let [r (.getBoundingClientRect el)]
              (screen->flow-rect inst (.-left r) (.-top r) (.-right r) (.-bottom r)))))
    #js []))

(defn- inside? [^js box ^js area]
  (and area
       (>= (.-x box) (.-x area)) (>= (.-y box) (.-y area))
       (<= (+ (.-x box) (.-w box)) (+ (.-x area) (.-w area)))
       (<= (+ (.-y box) (.-h box)) (+ (.-y area) (.-h area)))))

(defn- ghost-size
  "#js [w h] of the head node of a ghost piece."
  [^js head]
  (let [s (.-style head)]
    #js [(or (and s (.-width s)) ghost-w) (or (and s (.-height s)) ghost-h)]))

(defn- new-load?
  "True for the first flow of a new :loadId, and records it in the ref
   `last`. cg-vscode sends no :loadId: it posts a flow on each edit."
  [^js last ^js flow]
  (let [id (when flow (aget flow "loadId"))]
    (when (and (some? id) (not= id (.-current last)))
      (set! (.-current last) id)
      true)))

(defn- runnable?
  "True when the flow has a binding to run."
  [^js flow]
  (boolean (and flow (pos? (.-length (or (aget flow "bindings") #js []))))))

(defn- run-end-status
  "The console status for a cg/run-ended message. The bb run subprocess
   reports an exit code; a backend run reports a result count."
  [^js m]
  (let [code (aget m "exitCode")
        n    (aget m "count")]
    (cond
      (some? code)                 (str "exited " code)
      (= "error" (aget m "outcome")) "failed"
      (some? n)                    (str "ok, " n " result(s)")
      :else                        "ok")))

(defn App []
  (let [[received set-received]     (useState nil)
        [trace-state set-trace-state] (useState #js {})
        [progress-state set-progress-state] (useState #js {})
        ;; React Flow must own a mutable array, because a drag moves a node
        ;; with applyNodeChanges.
        [nodes set-nodes] (useState #js [])
        [palette-open set-palette-open] (useState false)
        ;; The body edges of ghost ops. The parsed edges come from convert.
        [ghost-edges set-ghost-edges] (useState #js [])
        ;; Chevron overrides, id -> boolean. An override survives a reseed,
        ;; because node ids are stable across flows.
        [expanded-overrides set-expanded-overrides] (useState #js {})
        [direction set-direction] (useState "DOWN")
        ;; The console splits :text into lines at render, because a chunk
        ;; boundary is not a line boundary.
        [run set-run] (useState #js {:text "" :status "idle" :file nil})
        [run-model set-run-model] (useState nil)
        [pane-w set-pane-w] (useState 1200)
        roomy?  (>= pane-w search-min-px)
        minimap? (>= pane-w minimap-min-px)
        ;; The op names the reader entered, outermost first. Empty means the
        ;; canvas of the file.
        [dive set-dive] (useState #js [])
        dive-name (when (> (.-length dive) 0) (aget dive (dec (.-length dive))))
        dive-into (fn [nm]
                    (when (and nm (not= nm ""))
                      (set-dive (fn [prev] (.concat prev #js [nm])))))
        dive-up-to (fn [n] (set-dive (fn [prev] (.slice prev 0 n))))
        toggle-expand (fn [id]
                        (set-expanded-overrides
                         (fn [prev]
                           (let [nxt (js/Object.assign #js {} prev)]
                             (aset nxt id (not (expanded-now? prev id)))
                             nxt))))
        rf (useRef nil)
        ;; A cg/reveal-line that arrives before the nodes exist puts its line
        ;; here. The reseed effect uses it.
        pending-reveal (useRef nil)
        ;; A ref, not state, because a new ghost id must not cause a render.
        ghost-counter (useRef 0)
        load-id (useRef nil)
        ;; True while the view is the last fit; a pan or zoom by the reader
        ;; clears it, and then no resize or re-post moves the view.
        auto-fit (useRef true)
        refit-pending (useRef false)
        pane-ref (useRef nil)
        fit-now! (fn []
                   (when-let [inst (.-current rf)]
                     (fit-legibly! inst (.-current pane-ref))
                     (set! (.-current auto-fit) true)))
        center-on (fn [^js node]
                    (when-let [inst (.-current rf)]
                      (let [inode (.getInternalNode inst (.-id node))
                            p  (.. inode -internals -positionAbsolute)
                            m  (or (.-measured inode) #js {})
                            z  (min (.-maxZoom fit-opts)
                                    (max (.getZoom inst) (legible-zoom (.-current pane-ref))))]
                        (set! (.-current auto-fit) false)
                        (.setCenter inst
                                    (+ (.-x p) (/ (or (.-width m) 0) 2))
                                    (+ (.-y p) (/ (or (.-height m) 0) 2))
                                    #js {:zoom z :duration 300})
                        (set-nodes (fn [prev] (select-only prev (.-id node)))))))
        ;; A dive parks the ghosts here, because the reseed for the dived body
        ;; drops them.
        parked-ghosts (useRef #js [])
        add-piece (fn [fn-name place]
                    (let [n     (inc (.-current ghost-counter))
                          gid   (str "ghost-" n)
                          ops   (when received (aget received "operations"))
                          piece (convert/ghost-piece gid fn-name 0 0 ops)
                          head  (aget (aget piece "nodes") 0)]
                      (set! (.-position head) (place head))
                      (set! (.-current ghost-counter) n)
                      (set-nodes (fn [prev] (.concat prev (aget piece "nodes"))))
                      (set-ghost-edges (fn [prev] (.concat prev (aget piece "edges"))))))
        add-ghost (fn [fn-name x y]
                    (add-piece fn-name (fn [_] #js {:x x :y y})))
        place-ghost (fn [fn-name]
                      (let [inst (.-current rf)
                            pane (.-current pane-ref)
                            area (visible-area inst pane)]
                        (add-piece fn-name
                                   (fn [head]
                                     (let [wh  (ghost-size head)
                                           box (free-spot (top-level-rects nodes inst)
                                                          (panel-rects inst pane)
                                                          (aget wh 0) (aget wh 1) area)]
                                       (when (and inst (not (inside? box area)))
                                         (set! (.-current auto-fit) false)
                                         (.setCenter inst
                                                     (+ (.-x box) (/ (.-w box) 2))
                                                     (+ (.-y box) (/ (.-h box) 2))
                                                     #js {:zoom (.getZoom inst) :duration 250}))
                                       #js {:x (.-x box) :y (.-y box)})))))]
    (use-phase-lock-pulse)
    ;; :fitView fits only on init, when the graph is empty. Fit again on a new
    ;; flow, after a delay in which React Flow measures the new nodes.
    (useEffect
     (fn []
       (when received
         (js/setTimeout
          (fn []
            (when (or (.-current refit-pending) (.-current auto-fit))
              (set! (.-current refit-pending) false)
              (fit-now!)))
          250))
       js/undefined)
     #js [received])
    (useEffect
     (fn []
       (when received (js/setTimeout fit-now! 250))
       js/undefined)
     #js [dive-name])
    (useEffect
     (fn []
       (let [pane  (.-current pane-ref)
             timer (atom nil)
             ro    (when (and pane (aget js/globalThis "ResizeObserver"))
                     (js/ResizeObserver.
                      (fn [entries]
                        (set-pane-w (.. (aget entries 0) -contentRect -width))
                        (js/clearTimeout @timer)
                        (reset! timer (js/setTimeout
                                       (fn [] (when (.-current auto-fit) (fit-now!)))
                                       150)))))]
         (when ro (.observe ro pane))
         (fn []
           (js/clearTimeout @timer)
           (when ro (.disconnect ro)))))
     #js [])
    ;; cg/ready tells the host to post the flow that it has.
    (useEffect
     (fn []
       (let [on-msg
             (fn [^js e]
               (let [m (.-data e)
                     t (when m (aget m "type"))]
                 (cond
                   (= t "cg/flow")
                   (let [f (aget m "flow")]
                     (when (new-load? load-id f)
                       (set-trace-state #js {})
                       (set-progress-state #js {})
                       (set-run #js {:text "" :status "idle" :file nil})
                       (set! (.-current refit-pending) true))
                     (when-not (and f (some? (aget f "loadId")))
                       (set! (.-current refit-pending) true))
                     (set-received f))

                   (= t "cg/run-model")
                   (set-run-model (aget m "model"))

                   (= t "cg/run-started")
                   (do (set-trace-state #js {})
                       (set-progress-state #js {})
                       (set-run #js {:text "" :status "running" :file (aget m "file")}))

                   (= t "cg/run-output")
                   (let [chunk (or (aget m "text") "")]
                     (set-run (fn [prev]
                                (js/Object.assign
                                 #js {} prev
                                 #js {:text (str (aget prev "text") chunk)}))))

                   ;; The flow-* rows are aggregate and have no node.
                   (= t "cg/trace-event")
                   (let [evt  (aget m "event")
                         kind (when evt (aget evt "kind"))
                         pid  (when evt (aget evt "pid"))
                         st   (cond
                                (= kind "proc-start")    "running"
                                (= kind "proc-complete") "complete"
                                (= kind "proc-error")    "error"
                                :else nil)]
                     (when (and st pid)
                       (set-trace-state
                        (fn [prev]
                          (let [next (js/Object.assign #js {} prev)]
                            (aset next pid st)
                            next))))
                     ;; Only the JVM backend sends rows-progress events.
                     (when (and (= kind "rows-progress") pid)
                       (set-progress-state
                        (fn [prev]
                          (let [next (js/Object.assign #js {} prev)]
                            (aset next pid #js {:done (aget evt "done")
                                                :total (aget evt "total")})
                            next)))))

                   ;; A hit in a collapsed container selects the container and
                   ;; does not expand it, because an expand on each caret move
                   ;; fills the canvas with open blocks. set-expanded-overrides
                   ;; only reads the overrides here: it returns them unchanged.
                   (= t "cg/cursor")
                   (let [line (aget m "line")]
                     (when line
                       (set-expanded-overrides
                        (fn [ov]
                          (set-nodes
                           (fn [prev]
                             (let [hit (node-for-line prev line)
                                   pid (and hit (aget hit "parent"))
                                   target (cond
                                            (and pid (not (expanded-now? ov pid))) pid
                                            hit (aget hit "id")
                                            :else nil)]
                               (select-only prev target))))
                          ov))))

                   ;; Unlike cg/cursor, this can expand the container. With no
                   ;; nodes yet, the line waits in pending-reveal.
                   (= t "cg/reveal-line")
                   (let [line (aget m "line")]
                     (when line
                       (set-nodes
                        (fn [prev]
                          (if (= 0 (.-length prev))
                            (do (set! (.-current pending-reveal) line)
                                prev)
                            (apply-reveal! prev line
                                           set-expanded-overrides rf))))))

                   ;; The host renders the first theme. This message arrives only
                   ;; on a theme change.
                   (= t "cg/theme-kind")
                   (apply-theme! (aget m "mode"))

                   (= t "cg/run-ended")
                   (set-run (fn [prev]
                              (js/Object.assign
                               #js {} prev
                               #js {:status (run-end-status m)})))

                   (= t "cg/run-error")
                   (set-run (fn [prev]
                              (js/Object.assign
                               #js {} prev
                               #js {:status (str "failed: " (aget m "message"))})))

                   :else nil)))]
         (.addEventListener js/window "message" on-msg)
         (host/post! #js {:type "cg/ready"})
         (fn [] (.removeEventListener js/window "message" on-msg))))
     #js [])
    (useEffect
     (fn []
       (let [on-key
             (fn [^js e]
               (when (= "/" (.-key e))
                 (let [ae  (.-activeElement js/document)
                       tag (when ae (.-tagName ae))
                       editable? (or (= tag "INPUT") (= tag "TEXTAREA")
                                     (and ae (.-isContentEditable ae)))]
                   (when-not editable?
                     (.preventDefault e)
                     (set-palette-open true)))))]
         (.addEventListener js/window "keydown" on-key)
         (fn [] (.removeEventListener js/window "keydown" on-key))))
     #js [])
    (useEffect
     (fn []
       (let [on-key
             (fn [^js e]
               (when (and (= "Escape" (.-key e)) (not palette-open))
                 (set-dive (fn [prev]
                             (if (> (.-length prev) 0)
                               (.slice prev 0 (dec (.-length prev)))
                               prev)))))]
         (.addEventListener js/window "keydown" on-key)
         (fn [] (.removeEventListener js/window "keydown" on-key))))
     #js [palette-open])
    ;; A drag needs no edge update, because React Flow computes edge paths
    ;; from the live node positions.
    (let [flow  (useMemo (fn []
                           (when received
                             (if dive-name
                               (convert/defop->react-flow dive-name received)
                               (convert/model->react-flow received))))
                         #js [received dive-name])
          edges (.concat (if flow (.-edges flow) #js []) ghost-edges)
          display (apply-collapse nodes edges expanded-overrides toggle-expand
                                  dive-into)]
      ;; Reseed the nodes on a new flow. trace-state is not a dependency,
      ;; because a reseed on each trace event moves dragged nodes back to the
      ;; layout.
      (useEffect
       (fn []
         (when flow
           (-> (layout/layout! (size-for-layout (.-nodes flow) expanded-overrides)
                               (.-edges flow) direction)
               (.then (fn [positioned]
                        (set-nodes
                         (fn [prev]
                           (let [ghosts (.filter prev (fn [n] (ghost-id? (aget n "id"))))
                                 ;; Carry the selection across the reseed by
                                 ;; id, because `positioned` comes from
                                 ;; convert and has no selection.
                                 sel   (let [s #js {}]
                                          (.forEach prev
                                                    (fn [n]
                                                      (when (.-selected n)
                                                        (aset s (aget n "id") true))))
                                          s)
                                 laid   (.map (patch-node-states positioned trace-state
                                                                 progress-state run-model)
                                              (fn [n]
                                                (if (aget sel (aget n "id"))
                                                  (js/Object.assign #js {} n #js {:selected true})
                                                  n)))]
                             (if dive-name
                               (do (when (> (.-length ghosts) 0)
                                     (set! (.-current parked-ghosts) ghosts))
                                   laid)
                               ;; Take the parked ghosts one time and release
                               ;; them, because a held set brings back a ghost
                               ;; that the reader deleted.
                               (let [restore (if (> (.-length ghosts) 0)
                                               ghosts
                                               (.-current parked-ghosts))]
                                 (set! (.-current parked-ghosts) #js [])
                                 (.concat laid restore))))))))
               (.then (fn [_]
                        (when-let [line (.-current pending-reveal)]
                          (set! (.-current pending-reveal) nil)
                          (set-nodes
                           (fn [prev]
                             (apply-reveal! prev line
                                            set-expanded-overrides rf))))))))
         js/undefined)
       ;; expanded-overrides is a dependency, because a chevron toggle changes
       ;; the size of a call-site.
       #js [received direction expanded-overrides dive-name])
      ;; Patch the existing nodes, because their positions can come from a drag.
      (useEffect
       (fn []
         (set-nodes (fn [prev] (patch-node-states prev trace-state progress-state run-model)))
         js/undefined)
       #js [trace-state progress-state run-model])
      #jsx [:div {:style #js {:width "100%" :height "100%"
                              :display "flex" :flexDirection "column"}}
            [:div {:style #js {:flex "1" :minHeight "0" :display "flex"
                               :flexDirection "row"}}
             [:div {:ref pane-ref
                    :style #js {:flex "1" :minWidth "0" :height "100%"
                                :position "relative"}}
              ;; The default minZoom of 0.5 is too large for a wide graph
              ;; (about 4000 px): fitView stops at 0.5 and leaves nodes out of
              ;; view.
              [ReactFlow {:nodes (.-nodes display) :edges (.-edges display) :fitView true
                         :minZoom 0.05 :maxZoom 1.5
                         :nodeTypes cn/node-types
                         :edgeTypes cn/edge-types
                         :onNodeClick (fn [e node] (on-node-click e node received))
                         :onNodeContextMenu on-node-context-menu
                         :onNodeDoubleClick on-node-double-click
                         :onNodesChange
                         (fn [changes]
                           ;; The removal of a ghost call-site also removes its
                           ;; body children and its ghost edges.
                           (let [removed (.filter changes (fn [c] (= "remove" (aget c "type"))))
                                 rid     (.map removed (fn [c] (aget c "id")))]
                             (when (> (.-length rid) 0)
                               (set-ghost-edges
                                (fn [es]
                                  (.filter es (fn [e]
                                                (and (not (.includes rid (aget e "source")))
                                                     (not (.includes rid (aget e "target")))))))))
                             (set-nodes
                              (fn [prev]
                                ;; On expand, the body children show one frame
                                ;; before the parent grows. The extent="parent"
                                ;; clamp of React Flow then emits a position
                                ;; change that pins them at the wrong height. A
                                ;; real drag has dragging=true: drop each other
                                ;; position change of a parented node.
                                (let [parented #js {}
                                      _ (.forEach prev
                                                  (fn [n]
                                                    (when (aget n "parentId")
                                                      (aset parented (aget n "id") true))))
                                      eff (.filter changes
                                                   (fn [c]
                                                     (not (and (= "position" (aget c "type"))
                                                               (not (aget c "dragging"))
                                                               (aget parented (aget c "id"))))))
                                      nxt (applyNodeChanges eff prev)]
                                  (if (> (.-length rid) 0)
                                    (.filter nxt (fn [n]
                                                   (let [p (aget n "parentId")]
                                                     (not (and p (.includes rid p))))))
                                    nxt))))))
                         :onNodeDragStop on-node-drag-stop
                         :onDragOver (fn [^js e]
                                       (.preventDefault e)
                                       (set! (.. e -dataTransfer -dropEffect) "copy"))
                         :onDrop (fn [^js e]
                                   (.preventDefault e)
                                   (let [op-name (.. e -dataTransfer (getData "application/cg-op"))]
                                     (when (and op-name (not= "" op-name))
                                       (when-let [inst (.-current rf)]
                                         (let [pos (.screenToFlowPosition
                                                    inst #js {:x (.-clientX e) :y (.-clientY e)})]
                                           (add-ghost op-name (.-x pos) (.-y pos)))))))
                         ;; Only a pan or zoom by the reader has an event.
                         :onMoveStart (fn [e _]
                                        (when e (set! (.-current auto-fit) false)))
                         :onInit (fn [inst] (set! (.-current rf) inst))}
              [Background]
              [Controls]
              (when minimap?
                #jsx [MiniMap {:pannable true :zoomable true
                               :ariaLabel "Overview of the canvas"
                               :nodeColor minimap-color
                               :maskColor "rgba(0, 0, 0, 0.12)"
                               :style #js {:background "var(--cg-surface-raised, #fff)"}}])
              ;; The width leaves room for the right panel.
              [Panel {:position "top-left"
                      :style #js {:maxWidth (if roomy?
                                              "max(170px, calc(100% - 470px))"
                                              "calc(100% - 210px)")}}
               [toolbar/Toolbar {:operations (when received (aget received "operations"))
                                 :onAddGhost place-ghost}]
               (when (> (.-length dive) 0)
                 #jsx [:div {:className "cg-crumbs" :data-testid "dive-crumbs"
                             :style #js {:marginTop "6px"}}
                       [:button {:className "cg-crumb"
                                 :title "Back to the file's own canvas"
                                 :onClick (fn [_] (dive-up-to 0))}
                        "flow"]
                       (.map dive
                             (fn [nm i]
                               #jsx [:button {:key (str "crumb-" i "-" nm)
                                              :className (str "cg-crumb"
                                                              (when (= i (dec (.-length dive)))
                                                                " current"))
                                              :title (str "Body of " nm)
                                              :onClick (fn [_] (dive-up-to (inc i)))}
                                     (str "› " nm)]))
                       [:span {:className "cg-crumb-hint"} "Esc up"]])]
              [Panel {:position "top-right"
                      :style #js {:display "flex" :alignItems "center"}}
               (when roomy?
                 #jsx [NodeSearch {:nodes (.-nodes display) :onPick center-on}])
               [:button {:className "cg-dir-btn"
                         :title (str "Layout direction: " direction
                                     " (click for " (if (= direction "DOWN") "RIGHT" "DOWN") ")")
                         :onClick (fn [_]
                                    (set-direction (if (= direction "DOWN") "RIGHT" "DOWN")))}
                (if (= direction "DOWN") "↕" "↔")]
               [:button {:className "cg-run-btn"
                         :title (if (runnable? received) "Run this flow" "Load a .cg file to run it")
                         :disabled (not (runnable? received))
                         ;; cg.css has no disabled look for this button.
                         :style (when-not (runnable? received)
                                  #js {:opacity 0.45 :cursor "default"})
                         :onClick (fn [_] (host/post! #js {:type "cg/run"}))}
                (if run-model (str "Run " run-model) "Run")]]]]
             [console/Console {:flow received :run run :narrow (not minimap?)}]]
            (when palette-open
              #jsx [palette/Palette {:operations (when received (aget received "operations"))
                                     ;; place-ghost ignores the palette's random x and y.
                                     :onAddGhost place-ghost
                                     :onClose (fn [] (set-palette-open false))}])])))

(defn mount
  "Renders App into the element `el-id` (default \"root\"). The host webview
   calls this after the module loads."
  ([] (mount "root"))
  ([el-id]
   (.render (createRoot (js/document.getElementById el-id)) #jsx [App])))
