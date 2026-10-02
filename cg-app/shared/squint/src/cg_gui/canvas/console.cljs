;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.console
  "The console pane: the Source view lists each `println` site, and the Output
   view lists the lines of the last run, which only cg-vscode sends."
  (:require ["react" :refer [useState useMemo useEffect useRef]]
            [cg-gui.canvas.host :as host]))

(def ^:private default-pane-width 300)
(def ^:private min-pane-width 180)
(def ^:private max-pane-width 800)

(defn- clamp-width [w]
  (min max-pane-width (max min-pane-width w)))

(defn- start-resize!
  "Starts a splitter drag that saves the width on pointerup. The listeners are on
   window, because a fast drag leaves the thin rail."
  [^js e w0 set-width]
  (.preventDefault e)
  (let [x0      (.-clientX e)
        settled (atom w0)
        on-move (fn [^js ev]
                  (let [w (clamp-width (+ w0 (- x0 (.-clientX ev))))]
                    (reset! settled w)
                    (set-width w)))
        on-up   (fn on-up [_]
                  (.removeEventListener js/window "pointermove" on-move)
                  (.removeEventListener js/window "pointerup" on-up)
                  (host/merge-ui-state! #js {:console-width @settled}))]
    (.addEventListener js/window "pointermove" on-move)
    (.addEventListener js/window "pointerup" on-up)))

(def ^:private surface "var(--cg-surface, var(--vscode-editor-background, #1e1e1e))")
(def ^:private sunk "var(--cg-surface-sunk, var(--vscode-editorWidget-background, #202020))")
(def ^:private raised "var(--cg-surface-raised, var(--vscode-list-hoverBackground, #2a2a2a))")
(def ^:private ink "var(--cg-ink, var(--vscode-foreground, #d4d4d4))")
(def ^:private muted "var(--cg-ink-muted, var(--vscode-descriptionForeground, #8b96a3))")
(def ^:private line "var(--cg-line-soft, var(--vscode-panel-border, #444))")
(def ^:private accent "var(--cg-accent, var(--vscode-textLink-foreground, #4c9aff))")
(def ^:private mono "var(--cg-font-mono, ui-monospace, monospace)")

(defn- g [o k] (when o (aget o k)))

(defn- entry
  "One console line. `context` is the head of the enclosing block, nil at the
   top level; `depth` is how many blocks deep it sits."
  [o depth context]
  #js {:text (g o "text")
       :args (g o "args")
       :label (g o "label")
       :raw (g o "raw")
       :location (g o "location")
       :depth depth
       :context context})

(defn- block-entries
  "Each `:output` body form under `b` (a block, a catch clause or a finally
   clause), with those of its nested blocks and clauses."
  [b depth context]
  (let [rows (.flatMap (or (g b "body") #js [])
                       (fn [f]
                         (let [kind (g f "kind")]
                           (cond
                             (= "output" kind) #js [(entry (g f "output") depth context)]
                             (= "block" kind)  (let [nb (g f "block")]
                                                 (block-entries nb (inc depth) (str (g nb "head"))))
                             :else #js []))))
        catches (.flatMap (or (g b "catches") #js [])
                          (fn [c] (block-entries c (inc depth) "catch")))
        fin (g b "finally")]
    (.concat rows catches
             (if fin (block-entries fin (inc depth) "finally") #js []))))

(defn- start-line [e]
  (or (g (g e "location") "start-line") 0))

(defn- start-col [e]
  (or (g (g e "location") "start-column") 0))

(defn console-entries
  "The console entries of a flow, in source order."
  [flow]
  (let [tops   (.map (or (g flow "outputs") #js [])
                     (fn [o] (entry o 0 nil)))
        nested (.flatMap (or (g flow "blocks") #js [])
                         (fn [b] (block-entries b 1 (str (g b "head")))))]
    (.sort (.concat tops nested)
           (fn [a b]
             (let [d (- (start-line a) (start-line b))]
               (if (= 0 d) (- (start-col a) (start-col b)) d))))))

(defn- one-line
  "The first line of `s`, cut at `cap` characters."
  [s cap]
  (let [t (str s)
        nl (.indexOf t "\n")
        one (if (>= nl 0) (str (.slice t 0 nl) " …") t)]
    (if (> (.-length one) cap) (str (.slice one 0 cap) " …") one)))

(defn- line-text
  "The text of a console row: the literal of a string println, else its argument.
   Tests `nil?`, because the empty string of a spacer println is falsy in JS."
  [e]
  (let [t (g e "text")]
    (if (nil? t)
      (let [args (or (g e "args") #js [])]
        (one-line (if (= 1 (.-length args)) (aget args 0) (g e "label")) 200))
      (str t))))

(defn- Row [e i hovered set-hovered]
  (let [computed? (nil? (g e "text"))
        blank?    (= "" (g e "text"))
        ctx       (g e "context")
        depth     (g e "depth")
        ln        (start-line e)
        hover?    (= i hovered)]
    #jsx [:button {:key (str "cg-console-" i)
                   :data-testid "console-line"
                   :data-line ln
                   :title (str "L" ln "  " (g e "raw"))
                   :onClick (fn [_] (host/reveal-location! (g e "location") nil))
                   :onMouseEnter (fn [_] (set-hovered i))
                   :onMouseLeave (fn [_] (set-hovered -1))
                   :style #js {:display "flex" :flexDirection "row"
                               :alignItems "baseline" :gap "8px"
                               :width "100%" :textAlign "left"
                               :padding "1px 8px 1px 6px"
                               :border "none" :borderRadius "2px"
                               :background (if hover? raised "transparent")
                               :color ink :cursor "pointer"
                               :font (str "11.5px/1.5 " mono)}}
          [:span {:style #js {:flexShrink 0 :minWidth "30px" :textAlign "right"
                              :color (if hover? accent muted)
                              :fontSize "10px"}}
           (str ln)]
          (when (> depth 0)
            #jsx [:span {:style #js {:flexShrink 0 :color muted :fontSize "9.5px"
                                     :border (str "1px solid " line)
                                     :borderRadius "2px" :padding "0 3px"}}
                  (str ctx)])
          [:span {:style #js {:flex "1" :minWidth "0"
                              :whiteSpace "pre" :overflow "hidden"
                              :textOverflow "ellipsis"
                              :color (if computed? muted ink)
                              :fontStyle (if computed? "italic" "normal")
                              :opacity (if blank? 0.35 1)}}
           (if blank? "·" (line-text e))]]))

(defn- site-index
  "text -> the line that prints it, for each unique string-literal site. A
   duplicate text maps to -1, because its site is ambiguous."
  [entries]
  (let [m #js {}]
    (.forEach entries
              (fn [e]
                (let [t (g e "text")]
                  (when (and (not (nil? t)) (not= "" t))
                    (aset m t (if (aget m t) -1 (start-line e)))))))
    m))

(defn- OutputRow
  "One emitted line. It has a source link only when its whole text is equal to
   the text of a unique literal site."
  [text i src-line on-reveal]
  (let [linked? (and src-line (> src-line 0))]
    #jsx [:div {:key (str "cg-out-" i)
                :data-testid "console-output-line"
                :data-src-line (if linked? src-line 0)
                :style #js {:display "flex" :flexDirection "row"
                            :alignItems "baseline" :gap "8px"
                            :padding "0 8px 0 6px"
                            :font (str "11.5px/1.5 " mono)
                            :color ink :whiteSpace "pre-wrap"
                            :wordBreak "break-word"}}
          [:span {:style #js {:flexShrink 0 :minWidth "30px" :textAlign "right"
                              :fontSize "10px" :color muted
                              :cursor (if linked? "pointer" "default")}
                  :title (if linked?
                           (str "printed by the println on line " src-line)
                           "no single println in this file prints exactly this")
                  :onClick (fn [_] (when linked? (on-reveal src-line)))}
           (if linked? (str "L" src-line) "")]
          [:span {:style #js {:flex "1" :minWidth "0"}}
           (if (= "" text) " " text)]]))

(defn- Tab [label active? n on-click testid]
  #jsx [:button {:onClick (fn [_] (on-click))
                 :data-testid testid
                 :style #js {:background (if active? raised "transparent")
                             :color (if active? ink muted)
                             :borderTop "none" :borderLeft "none"
                             :borderRight "none"
                             :borderBottom (str "2px solid "
                                                (if active? accent "transparent"))
                             :cursor "pointer" :padding "3px 8px"
                             :fontSize "11px" :fontFamily mono}}
        (str label " " n)])

(defn- Header [source-n output-n view set-view status on-collapse]
  #jsx [:div {:style #js {:display "flex" :flexDirection "row" :alignItems "center"
                          :gap "2px" :padding "4px 4px 0 6px"
                          :borderBottom (str "1px solid " line)
                          :flexShrink 0}}
        (Tab "Source" (= "source" view) source-n
             (fn [] (set-view "source")) "console-tab-source")
        (Tab "Output" (= "output" view) output-n
             (fn [] (set-view "output")) "console-tab-output")
        [:span {:data-testid "console-status"
                :style #js {:flex "1" :textAlign "right" :color muted
                            :fontSize "10px" :fontFamily mono
                            :overflow "hidden" :whiteSpace "nowrap"
                            :textOverflow "ellipsis"}}
         (if (= "idle" status) "" status)]
        [:button {:title "Hide the console pane"
                  :data-testid "console-hide"
                  :onClick (fn [_] (on-collapse))
                  :style #js {:background "transparent" :border "none"
                              :color muted :cursor "pointer"
                              :fontSize "13px" :lineHeight 1 :padding "0 2px"}}
         "›"]])

(defn- failed? [status]
  (let [s (str status)]
    (or (.startsWith s "failed")
        (and (.startsWith s "exited ") (not= "exited 0" s)))))

(defn- CollapsedTab [n failed on-open]
  ;; Four border longhands, not `:border "none"` plus `:borderLeft`. squint
  ;; does not keep the key order of a map literal, and a `border` emitted last
  ;; removes the rail.
  #jsx [:button {:title "Show the console pane"
                 :data-testid "console-show"
                 :onClick (fn [_] (on-open))
                 :style #js {:flexShrink 0 :width "26px" :height "100%"
                             :background sunk :color muted
                             :borderTop "none" :borderRight "none"
                             :borderBottom "none"
                             :borderLeft (str "1px solid " line)
                             :cursor "pointer" :fontSize "10.5px"
                             :fontFamily mono :padding "8px 0"
                             :writingMode "vertical-rl"}}
        (str "Console " n (when failed " · run failed"))])

(def ^:private max-output-lines
  "The number of emitted lines that the pane keeps. Each row has a DOM cost."
  2000)

(defn- output-lines
  "The run text as rows, at most the last max-output-lines. A trailing newline
   ends the last line and adds no empty row."
  [text]
  (let [t (str text)
        body (if (.endsWith t "\n") (.slice t 0 -1) t)
        all (if (= "" body) #js [] (.split body "\n"))]
    (if (> (.-length all) max-output-lines)
      (.slice all (- (.-length all) max-output-lines))
      all)))

(defn- SourceView [entries hovered set-hovered]
  #jsx [:div {:data-testid "console-source"
              :style #js {:flex "1" :minHeight "0" :overflowY "auto"
                          :padding "4px 0"}}
        (.map entries (fn [e i] (Row e i hovered set-hovered)))])

(defn- OutputView [lines entries scroll-ref]
  (let [idx (site-index entries)]
    #jsx [:div {:data-testid "console-output"
                :ref scroll-ref
                :style #js {:flex "1" :minHeight "0" :overflowY "auto"
                            :padding "4px 0"}}
          (if (= 0 (.-length lines))
            #jsx [:div {:style #js {:padding "8px" :color muted :fontSize "10.5px"
                                    :lineHeight 1.5}}
                  "Nothing printed yet. Run the flow and its output lands here, beside the printlns that produce it."]
            (.map lines
                  (fn [t i]
                    (OutputRow t i (aget idx t)
                               (fn [ln] (host/reveal-location!
                                         #js {:start-line ln :start-column 1
                                              :end-line ln :end-column 1}
                                         nil))))))]))

(defn- quiet?
  "True when the pane has no println and no output. A quiet pane stays a tab,
   because a Run must not take the room of the canvas."
  [n m]
  (and (zero? n) (zero? m)))

(defn Console
  "Props: flow (the CgFlow, or nil), run (#js {:text :status}) and narrow (true
   when the pane opens only on a click). Renders nothing when quiet and idle."
  [^js props]
  (let [flow    (aget props "flow")
        run     (or (aget props "run") #js {:text "" :status "idle"})
        ;; useState reads the saved webview state one time, on mount.
        saved   (host/ui-state)
        [open? set-open?]     (useState (if (some? (g saved "console-open"))
                                          (boolean (g saved "console-open"))
                                          true))
        [width set-width]     (useState (or (g saved "console-width")
                                            default-pane-width))
        show!   (fn [v]
                  (set-open? v)
                  (host/merge-ui-state! #js {:console-open v}))
        [view set-view]       (useState "source")
        ;; A click on the tab opens a quiet pane anyway.
        [peek? set-peek?]     (useState false)
        [hovered set-hovered] (useState -1)
        entries (useMemo (fn [] (if flow (console-entries flow) #js []))
                         #js [flow])
        status  (or (g run "status") "idle")
        lines   (useMemo (fn [] (output-lines (g run "text")))
                         #js [(g run "text")])
        n       (.-length entries)
        m       (.-length lines)
        scroll-ref (useRef nil)]
    (useEffect (fn on-run-start []
                 (when (= "running" status) (set-view "output"))
                 js/undefined)
               #js [status])
    (useEffect (fn follow-tail []
                 (when-let [el (.-current scroll-ref)]
                   (set! (.-scrollTop el) (.-scrollHeight el)))
                 js/undefined)
               #js [m view])
    (when (or (> n 0) (> m 0) (not= "idle" status))
      (if-not (and open? (or peek? (not (or (aget props "narrow") (quiet? n m)))))
        (CollapsedTab (if (= "output" view) m n) (failed? status)
                      (fn [] (show! true) (set-peek? true)))
        #jsx [:aside {:data-testid "console-pane"
                      :style #js {:flexShrink 0 :width (str width "px")
                                  :maxWidth (if (aget props "narrow") "85%" "50%")
                                  :height "100%" :display "flex"
                                  :flexDirection "column"
                                  :position "relative"
                                  :background sunk :color ink
                                  :borderLeft (str "1px solid " line)}}
              [:div {:data-testid "console-splitter"
                     :title "Drag to resize the console"
                     :onPointerDown (fn [^js e] (start-resize! e width set-width))
                     :style #js {:position "absolute" :left "-3px" :top "0"
                                 :bottom "0" :width "7px" :zIndex 5
                                 :cursor "col-resize"}}]
              ;; A call, not `[Header n ...]`: when the second form of an
              ;; element is not a props map, each argument becomes a child.
              (Header n m view set-view status (fn [] (show! false) (set-peek? false)))
              (if (= "output" view)
                (OutputView lines entries scroll-ref)
                (SourceView entries hovered set-hovered))
              [:div {:style #js {:flexShrink 0 :padding "5px 8px"
                                 :borderTop (str "1px solid " line)
                                 :background surface
                                 :color muted :fontSize "10px" :lineHeight 1.4}}
               (if (= "output" view)
                 "What the last run printed, in the order it printed. A line links to its println when exactly one in this file prints that text."
                 "Every println in the file, in source order, read from the source rather than from a run. Click a line to go to it.")]]))))
