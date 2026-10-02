;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.nodes
  "The custom React Flow node and edge. CnNode always renders the body Handles,
   because React Flow drops an edge to a missing handle. Read :data with aget."
  (:require ["react" :refer [Fragment useState]]
            ["@xyflow/react" :refer [Handle Position BaseEdge EdgeLabelRenderer
                                     getBezierPath getSmoothStepPath useInternalNode]]
            ["@carbon/react" :refer [Tag]]
            [cg-gui.canvas.host :as host]
            [cg-gui.canvas.param-field :refer [ParamFields]]))

(def ^:private default-stat-text
  #js {:idle "ready" :running "running" :complete "complete" :error "error"
       :blocked "blocked" :stale "stale" :skipped "skipped" :partial "partial"})

(defn- state-str [d]
  (let [s (aget d "state")]
    (if (and s (not= s "")) (str s) "idle")))

(defn- progress-text
  "A rows-progress event as the stat line of the node: \"<done> / <total>
   rows\"."
  [^js p]
  (str (.toLocaleString (aget p "done")) " / "
       (.toLocaleString (aget p "total")) " rows"))

(defn- progress-pct [^js p]
  (let [total (aget p "total")]
    (str (if (> total 0)
           (js/Math.round (* 100 (/ (aget p "done") total)))
           0)
         "%")))

(defn- node-class [d selected?]
  (str "cg-node"
       (when (aget d "is-defop?") " defop-call")
       (when (aget d "is-map?") " map-call")
       (when (aget d "is-template?") " template-call")
       (when (aget d "is-sink?") " sink-call")
       (when (aget d "chip?") " chip")
       (when (aget d "opaque?") " opaque-block")
       (let [c (aget d "classification")] (when c (str " cls-" c)))
       (when (aget d "model-head?") " model-head")
       (when (aget d "block?") " block-container")
       (when (aget d "block-body?") " block-body")
       (when (aget d "unresolved?") " unresolved-op")
       (when (aget d "divable?") " divable")
       (when (aget d "in-thread?") " in-thread")
       (let [m (aget d "mode")] (when m (str " mode-" m)))
       (when (aget d "run-model?") " run-model")
       (when selected? " selected")
       " " (state-str d)))

(defn- ports-block
  "Renders the named ports of a node as name:level chips, inputs on top and
   outputs below. The chips are decorative: the default handles carry the edges."
  [^js ports]
  (let [ins  (or (aget ports "inputs") #js [])
        outs (or (aget ports "outputs") #js [])]
    #jsx [:div {:className "cg-ports"}
          (when (> (.-length ins) 0)
            #jsx [:div {:className "cg-ports-row cg-ports-in"}
                  (.map ins
                        (fn [p i]
                          #jsx [:span {:key (str "in-" i)
                                       :className (str "cg-port lvl-" (aget p "level"))}
                                (str "▸ " (aget p "name") ":" (aget p "level"))]))])
          (when (> (.-length outs) 0)
            #jsx [:div {:className "cg-ports-row cg-ports-out"}
                  (.map outs
                        (fn [p i]
                          #jsx [:span {:key (str "out-" i)
                                       :className (str "cg-port lvl-" (aget p "level"))}
                                (str (aget p "name") ":" (aget p "level") " ▸")]))])]))

;; A report op shows its spec as editable inputs. An edit changes the local
;; state and posts cg/edit EditReportSpec. Nothing writes the edit back to the
;; .cg file.

(defn- frag->text
  "Numeric fragment -> compact rule text: {:eq 1} -> \"eq 1\",
   {:range [4 6]} -> \"range 4 6\"."
  [frag]
  (cond
    (nil? frag) ""
    (some? (aget frag "eq"))    (str "eq " (aget frag "eq"))
    (some? (aget frag "gte"))   (str "gte " (aget frag "gte"))
    (some? (aget frag "lte"))   (str "lte " (aget frag "lte"))
    (some? (aget frag "range")) (let [r (aget frag "range")]
                                  (str "range " (aget r 0) " " (aget r 1)))
    :else ""))

(defn- text->frag [s]
  (let [ts (.split (.trim (str s)) (js/RegExp. "\\s+"))
        op (aget ts 0)
        n1 (js/Number (aget ts 1))
        n2 (js/Number (aget ts 2))]
    (cond
      (and (= op "eq")  (not (js/Number.isNaN n1))) #js {:eq n1}
      (and (= op "gte") (not (js/Number.isNaN n1))) #js {:gte n1}
      (and (= op "lte") (not (js/Number.isNaN n1))) #js {:lte n1}
      (and (= op "range") (not (js/Number.isNaN n1)) (not (js/Number.isNaN n2)))
      #js {:range #js [n1 n2]}
      :else nil)))

(defn- rule->text
  "A bucket entry's rule (bare fragment) or a presence row's rule
   (all/any/count+min wrapper) -> editable text."
  [entry bucket?]
  (if bucket?
    (frag->text entry)
    (cond
      (aget entry "all")   (str "all " (frag->text (aget entry "all")))
      (aget entry "any")   (str "any " (frag->text (aget entry "any")))
      (aget entry "count") (str "count " (frag->text (aget entry "count"))
                                " min " (or (aget entry "min") 1))
      :else "")))

(defn- text->rule [s bucket?]
  (if bucket?
    (text->frag s)
    (let [t (.trim (str s))]
      (cond
        (.startsWith t "all ")
        (when-let [f (text->frag (.slice t 4))] #js {:all f})

        (.startsWith t "any ")
        (when-let [f (text->frag (.slice t 4))] #js {:any f})

        (.startsWith t "count ")
        (let [m (.match t (js/RegExp. "^count\\s+(.*?)\\s+min\\s+(\\d+)$"))]
          (when m
            (when-let [f (text->frag (aget m 1))]
              #js {:count f :min (js/Number (aget m 2))})))

        :else nil))))

(defn- with-key
  "A copy of `o` with `k` set to `v`. Uses aset, because of a squint defect
   with a computed key in an object literal."
  [o k v]
  (let [n (js/Object.assign #js {} o)]
    (aset n k v)
    n))

(defn ReportParams [^js props]
  (let [d        (.-data props)
        op       (aget d "report-op")
        bucket?  (= op "summarize")
        list-key (if bucket? "buckets" "rows")
        [spec set-spec] (useState (aget d "report-spec"))
        node-id  (str (aget d "model-name") "__" (aget d "raw-id"))
        commit!  (fn [nxt]
                   (set-spec nxt)
                   (host/post! #js {:type "cg/edit"
                                    :editType "EditReportSpec"
                                    :model (aget d "model-name")
                                    :payload #js {:nodeId node-id :spec nxt}}))
        upd      (fn [k v] (commit! (with-key spec k v)))
        entries  (or (aget spec list-key) #js [])
        set-label (fn [i txt]
                    (upd list-key
                         (.map entries (fn [e j]
                                         (if (= i j) (with-key e "label" txt) e)))))
        set-rule  (fn [i txt]
                    ;; Build the entry from the label and the parsed rule,
                    ;; because old fragment keys must not stay. Invalid text
                    ;; is ignored.
                    (when-let [rule (text->rule txt bucket?)]
                      (upd list-key
                           (.map entries
                                 (fn [e j]
                                   (if (= i j)
                                     (js/Object.assign #js {:label (aget e "label")} rule)
                                     e))))))
        add-entry (fn []
                    (upd list-key
                         (.concat entries
                                  #js [(if bucket?
                                         #js {:label "new" :eq 1}
                                         #js {:label "new" :any #js {:eq 1}})])))
        rm-entry  (fn [i] (upd list-key (.filter entries (fn [_ j] (not= i j)))))
        stop      (fn [^js e] (.stopPropagation e))]
    #jsx [:div {:className "cg-report-params nodrag nopan"
                :data-testid "report-params"
                :onPointerDown stop
                :onClick stop}
          [:div {:className "cg-report-field"}
           [:span {:className "cg-report-key"} "value"]
           [:input {:className "cg-report-input"
                    :data-testid "report-value"
                    :defaultValue (str (or (aget spec "value") ""))
                    :onBlur (fn [^js e] (upd "value" (.. e -target -value)))}]]
          (when bucket?
            #jsx [:div {:className "cg-report-field"}
                  [:span {:className "cg-report-key"} "scope"]
                  [:input {:className "cg-report-input"
                           :data-testid "report-scope"
                           :defaultValue (str (or (aget spec "scope") ""))
                           :onBlur (fn [^js e] (upd "scope" (.. e -target -value)))}]])
          (when bucket?
            #jsx [:div {:className "cg-report-field"}
                  [:span {:className "cg-report-key"} "weight"]
                  [:select {:className "cg-report-input"
                            :data-testid "report-weight"
                            :value (str (or (aget spec "weight") "rows"))
                            :onChange (fn [^js e] (upd "weight" (.. e -target -value)))}
                   [:option {:value "rows"} "rows"]
                   [:option {:value "value"} "value"]]])
          [:div {:className "cg-report-field"}
           [:span {:className "cg-report-key"} (if bucket? "buckets" "rows")]
           [:button {:className "cg-report-add"
                     :data-testid "report-add-entry"
                     :title "add entry"
                     :onClick (fn [_] (add-entry))}
            "+"]]
          (.map entries
                (fn [e i]
                  #jsx [:div {:className "cg-report-entry"
                              :key (str op "-" i)}
                        [:input {:className "cg-report-label"
                                 :data-testid (str "report-entry-label-" i)
                                 :defaultValue (str (aget e "label"))
                                 :onBlur (fn [^js ev] (set-label i (.. ev -target -value)))}]
                        [:input {:className "cg-report-rule"
                                 :data-testid (str "report-entry-rule-" i)
                                 :defaultValue (rule->text e bucket?)
                                 :onBlur (fn [^js ev] (set-rule i (.. ev -target -value)))}]
                        [:button {:className "cg-report-rm"
                                  :title "remove entry"
                                  :onClick (fn [_] (rm-entry i))}
                         "×"]]))]))

;; The left padding and border of .cg-node in cg.css.
(def ^:private card-inset 17)

(defn- side-port-row
  "One argument row of a thread step: the port, the parameter name, and the
   argument. A wired argument gets a wire from the left edge of the frame."
  [^js p i reach]
  (let [label (aget p "label")]
    #jsx [:div {:key (str "side-" i) :className "cg-side-row"}
          (when (aget p "wired?")
            #jsx [:div {:className (str "cg-wire" (when (aget p "frame-in?") " frame-in"))
                        :style #js {:width (str reach "px")}}
                  (when label
                    #jsx [:span {:className "cg-wire-label"} label])])
          (when (> (.-length (aget p "sources")) 0)
            #jsx [Handle {:type "target" :position (.-Left Position) :id (aget p "id")
                          :className "cg-quiet-handle"
                          :style #js {:left (str (- 0 reach card-inset) "px")}}])
          [:span {:className (str "cg-side-dot" (when (aget p "wired?") " wired"))}]
          (when-let [nm (aget p "name")]
            #jsx [:span {:className "cg-side-name"} nm])
          (when-let [t (aget p "text")]
            #jsx [:span {:className "cg-side-val"} t])]))

(defn CnNode [^js props]
  (let [d         (.-data props)
        selected  (.-selected props)
        st        (state-str d)
        ;; canvas.cljs sets this from rows-progress trace events.
        progress  (aget d "progress")
        running?  (= st "running")
        stat-text (or (aget d "stat")
                      (when (and progress running?) (progress-text progress))
                      (aget default-stat-text st) st)
        ;; :label first: a builder puts the display text in :label and the
        ;; bare symbol in :head.
        head      (or (aget d "label") (aget d "head"))
        val       (aget d "val")
        src-line  (aget d "source-line")
        ;; A card that did not run shows no status: "ready" on each card
        ;; says nothing. A body row of a block is source, and a chip is compact.
        footer?   (and (not (or (aget d "block-body?") (aget d "chip?")))
                       (or (not= st "idle") (some? (aget d "stat"))))
        ;; The chevron and the body handles key on :has-body?, the same field
        ;; that apply-collapse uses.
        has-body? (aget d "has-body?")
        expanded? (aget d "expanded?")
        ;; A body step that calls an op opens it with :on-dive, not inline.
        ;; canvas.cljs owns that stack and injects :on-dive.
        divable?   (aget d "divable?")
        dive-names (or (aget d "dive-names") #js [])
        ;; A step with branches reaches more than one op: the button opens a list.
        many-dives? (> (.-length dive-names) 1)
        [dive-open set-dive-open] (useState false)
        icon      (aget d "icon")
        sprite?   (and icon (.startsWith (str icon) "i-"))
        ;; body-in is a source at the top edge and body-out is a target at the
        ;; bottom edge. The positions are inverted, because the bezier edges
        ;; must curve into the parent card. Both are at :body-handle-x, the
        ;; center line of the body column, because the entry and exit edges
        ;; must be vertical.
        bhx            (str (or (aget d "body-handle-x") 30) "px")
        body-in-style  #js {:background "transparent" :border "none"
                            :width "1px" :height "1px" :left bhx :top "0px"}
        body-out-style #js {:background "transparent" :border "none"
                            :width "1px" :height "1px" :left bhx
                            :top "auto" :bottom "0px"}
        in-thread? (aget d "in-thread?")]
    #jsx [Fragment
          (if in-thread?
            #jsx [Handle {:type "target" :position (.-Top Position) :id "thread-in"
                          :className "cg-quiet-handle"}]
            #jsx [Handle {:type "target" :position (.-Top Position)}])
          (when expanded?
            #jsx [Handle {:type "source" :position (.-Bottom Position)
                          :id "body-in" :style body-in-style}])
          [:div {:className (node-class d selected)
                 :title (if-let [sp (aget d "source-preview")]
                          (.slice (str sp) 0 800)
                          (when (aget d "chip?") head))
                 :style (if (or expanded? in-thread?)
                          #js {:minWidth "0" :width "100%" :height "100%"
                               :boxSizing "border-box" :position "relative"}
                          #js {:minWidth "0" :width "100%" :position "relative"})}
           (when (aget d "unresolved?")
             #jsx [:span {:className "node-diag-badge warn"
                          :data-testid "unresolved-op-badge"
                          :title "unresolved operation: no signature in built-ins, this file's ops, or any loaded module"}
                   "?"])
           (when head
             #jsx [:div {:className "cg-head"}
                   (when sprite?
                     #jsx [:svg {:viewBox "0 0 16 16"
                                 :style #js {:width "11px" :height "11px"
                                             :marginRight "4px"
                                             :verticalAlign "middle"}}
                           [:use {:href (str "#" icon)}]])
                   head])
           (when-let [sub (aget d "sub")]
             #jsx [:div {:className "cg-sub"} sub])
           (when-let [side (aget d "side-ports")]
             (.map side (fn [p i] (side-port-row p i (or (aget d "wire-reach") 0)))))
           (when-let [fields (aget d "fields")]
             #jsx [ParamFields {:name (aget d "head") :fields fields}])
           (when-let [ports (aget d "ports")]
             (when-not in-thread? (ports-block ports)))
           (when (aget d "report-spec")
             #jsx [ReportParams {:data d}])
           (when val  #jsx [:div {:className "cg-val"} val])
           (when footer?
             #jsx [:div {:className "cg-stat"} [:span] stat-text])
           (when (and footer? progress running?)
             #jsx [:div {:className "cg-progress"}
                   [:div {:className "cg-progress-fill"
                          :style #js {:width (progress-pct progress)}}]])
           (when src-line
             #jsx [:span {:className "cg-source-link"} (str "L" src-line " →")])
           (when divable?
             #jsx [:span {:className (str "cg-dive-btn"
                                          (when has-body? " with-chevron"))
                          :data-testid "dive-btn"
                          :title (if many-dives?
                                   (str "Goes " (.-length dive-names) " ways: "
                                        (.join dive-names ", "))
                                   (str "Open " (or (aget dive-names 0) head)))
                          :onClick (fn [^js e]
                                     (.stopPropagation e)
                                     (if many-dives?
                                       (set-dive-open (not dive-open))
                                       (when-let [f (aget d "on-dive")]
                                         (f (aget dive-names 0)))))
                          :onPointerDown (fn [^js e] (.stopPropagation e))}
                   #jsx [:svg {:viewBox "0 0 16 16"
                               :style #js {:width "11px" :height "11px"
                                           :display "block"}}
                         [:use {:href "#i-zoom-in"}]]])
           (when (and divable? many-dives? dive-open)
             #jsx [:div {:className "cg-dive-menu nodrag nopan"
                         :data-testid "dive-menu"
                         :onPointerDown (fn [^js e] (.stopPropagation e))}
                   (.map dive-names
                         (fn [nm i]
                           #jsx [:button {:key (str "dive-" i "-" nm)
                                          :className "cg-dive-choice"
                                          :onClick (fn [^js e]
                                                     (.stopPropagation e)
                                                     (set-dive-open false)
                                                     (when-let [f (aget d "on-dive")]
                                                       (f nm)))}
                                 nm]))])
           (when has-body?
             #jsx [:span {:className (str "cg-defop-chevron"
                                          (when expanded? " expanded"))
                          :title (str "Toggle body of "
                                      (or (aget d "defop-name") head))
                          :onClick (fn [^js e]
                                     (.stopPropagation e)
                                     (when-let [f (aget d "on-toggle-expand")]
                                       (f)))
                          :onPointerDown (fn [^js e] (.stopPropagation e))}
                   ;; The href is #i-chevron-right in both states. cg.css
                   ;; rotates it on .expanded.
                   #jsx [:svg {:viewBox "0 0 16 16"
                               :style #js {:width "11px" :height "11px"
                                           :display "block"}}
                         [:use {:href "#i-chevron-right"}]]])]
          (when expanded?
            #jsx [Handle {:type "target" :position (.-Top Position)
                          :id "body-out" :style body-out-style}])
          (if in-thread?
            #jsx [Handle {:type "source" :position (.-Bottom Position) :id "thread-out"
                          :className "cg-quiet-handle"}]
            #jsx [Handle {:type "source" :position (.-Bottom Position)}])]))

(def ^:private macro-tag-type
  #js {:table "teal" :item "purple" :coll "blue" :map "magenta"})

(defn- at
  "A style that centers a handle on the frame point (x, y)."
  [x y]
  #js {:left (str x "px") :top (str y "px") :right "auto" :bottom "auto"
       :transform "translate(-50%, -50%)"})

(defn- frame-band
  [^js b i]
  #jsx [:div {:key (str "band-" i)
              :className "cg-thread-band"
              :style #js {:left (str (aget b "x") "px") :top (str (aget b "y") "px")
                          :width (str (aget b "w") "px") :height (str (aget b "h") "px")}}
        [:span {:className "cg-thread-band-label"} "per row"]])

(defn- frame-port
  [cls text w cx y]
  #jsx [:div {:className (str "cg-thread-port " cls)
              :style #js {:left (str (- cx (/ w 2)) "px") :top (str y "px")
                          :width (str w "px")}}
        text])

(defn ThreadFrame [^js props]
  (let [d      (.-data props)
        geom   (aget d "geom")
        cx     (aget geom "cx")
        src-y  (aget geom "src-y")
        res-y  (aget geom "res-y")
        ph     (aget geom "port-h")
        sw     (aget d "source-width")
        rw     (aget d "result-width")
        mode   (aget d "mode")]
    #jsx [Fragment
          [Handle {:type "target" :position (.-Top Position) :id "src-in"
                   :className "cg-quiet-handle" :style (at cx 0)}]
          [Handle {:type "source" :position (.-Bottom Position) :id "thread-src"
                   :className "cg-quiet-handle" :style (at cx (+ src-y ph))}]
          [Handle {:type "target" :position (.-Top Position) :id "thread-res"
                   :className "cg-quiet-handle" :style (at cx res-y)}]
          [Handle {:type "source" :position (.-Bottom Position) :id "res-out"
                   :className "cg-quiet-handle" :style (at cx (+ res-y ph))}]
          [:div {:className (str (node-class d (.-selected props)) " cg-thread")
                 :title (aget d "label")}
           [:div {:className "cg-thread-head"}
            [:span {:className "cg-thread-title"} (aget d "label")]
            [Tag {:type (or (aget macro-tag-type mode) "gray") :size "sm"
                  :className "cg-tag"}
             (aget d "macro")]
            [Tag {:type "cool-gray" :size "sm" :className "cg-tag"} (aget d "kind")]]
           (.map (aget geom "bands") frame-band)
           (frame-port "source" (aget d "source") sw cx src-y)
           (frame-port "result" (aget d "result") rw cx res-y)]]))

;; convert.cljs gives its nodes `:type "cg"`. `default` also maps to CnNode,
;; for a node with no :type.
(def node-types
  #js {:default CnNode
       :cg CnNode
       :cnThread ThreadFrame})

(defn CnGroupsEdge
  "The edge for the `groups` level: the default bezier with a `×N` badge at the
   midpoint. React Flow puts the cg-edge-groups class on the wrapping <g>."
  [^js props]
  (let [res  (getBezierPath
              #js {:sourceX (.-sourceX props) :sourceY (.-sourceY props)
                   :sourcePosition (.-sourcePosition props)
                   :targetX (.-targetX props) :targetY (.-targetY props)
                   :targetPosition (.-targetPosition props)})
        path (aget res 0)
        lx   (aget res 1)
        ly   (aget res 2)]
    #jsx [Fragment
          [BaseEdge {:id (.-id props) :path path :markerEnd (.-markerEnd props)}]
          [EdgeLabelRenderer
           [:div {:className "cg-edge-badge nodrag nopan"
                  :style #js {:position "absolute"
                              :transform (str "translate(-50%,-50%) translate("
                                              lx "px," ly "px)")}}
            "×N"]]]))

(defn CnThreadEdge
  "A straight thread line. A coll-> line is two rails: the three macros differ
   by line style and not by color alone."
  [^js props]
  (let [sx   (.-sourceX props)
        sy   (.-sourceY props)
        tx   (.-targetX props)
        ty   (.-targetY props)
        mode (aget (or (.-data props) #js {}) "mode")
        path (fn [dx] (str "M" (+ sx dx) "," sy " L" (+ tx dx) "," ty))]
    (if (= "coll" mode)
      #jsx [Fragment
            [BaseEdge {:id (str (.-id props) "-a") :path (path -2.5)}]
            [BaseEdge {:id (.-id props) :path (path 2.5)}]]
      #jsx [BaseEdge {:id (.-id props) :path (path 0)}])))

(defn CnLaneEdge
  "A side line from a card, down the left side of the frame, to the port of a
   step. It turns only outside the frame, because a turn inside crosses a step."
  [^js props]
  (let [tn    (useInternalNode (.-target props))
        frame (useInternalNode (when tn (.-parentId tn)))
        sx    (.-sourceX props)
        sy    (.-sourceY props)
        tx    (.-targetX props)
        ty    (.-targetY props)
        lane  (or (aget (or (.-data props) #js {}) "lane") 0)
        fpos  (when frame (.. frame -internals -positionAbsolute))
        x-in  (- (if fpos (.-x fpos) tx) 16 (* 8 lane))
        y-top (- (if fpos (.-y fpos) ty) 16 (* 6 lane))
        y-run (if (> sy y-top) (+ sy 12) y-top)
        path  (str "M" sx "," sy " L" sx "," y-run " L" x-in "," y-run
                   " L" x-in "," ty " L" tx "," ty)]
    #jsx [BaseEdge {:id (.-id props) :path path}]))

(defn CnSourceEdge
  "The line from a card to the thread frame that reads it. It ends at the top
   of the frame, above the source port, where it cannot cross the title."
  [^js props]
  (let [res (getSmoothStepPath
             #js {:sourceX (.-sourceX props) :sourceY (.-sourceY props)
                  :sourcePosition (.-sourcePosition props)
                  :targetX (.-targetX props) :targetY (.-targetY props)
                  :targetPosition (.-targetPosition props)
                  :borderRadius 0})]
    #jsx [BaseEdge {:id (.-id props) :path (aget res 0)}]))

(def edge-types
  #js {:cnGroupsEdge CnGroupsEdge
       :cnThread CnThreadEdge
       :cnSource CnSourceEdge
       :cnLane CnLaneEdge})
