;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.convert
  "Turns a parsed CgFlow into the React Flow {:nodes :edges} of the canvas.
   The host posts the CgFlow as plain JSON: keys and keyword values are strings.")

(def ^:private column-width 280)
(def ^:private node-height 120)
(def ^:private row-y-loader 20)
(def ^:private row-y-opaque 1100)
;; Each card without a body has this width. If the declared box is smaller than
;; the rendered card, ELK packs the row too tight and the cards overlap.
(def ^:private card-width 240)
(def ^:private row-y-param 380)
(def ^:private row-y-block 500)
(def ^:private block-width 320)
(def ^:private block-row-step 34)
(def ^:private block-card-height 56)
(def ^:private block-indent 16)
;; The left and right inset of the body column in an expanded call-site. The
;; insets are equal, because the body column must be on the center line of the
;; container, where the top and bottom handles are.
(def ^:private body-x-offset 40)
(def ^:private body-y-step 90)
(def ^:private call-site-card-height 100)
;; The box that a chip node declares to ELK.
(def ^:private chip-height 24)

(defn- g
  "Reads key `k` of JS object `o`. Returns nil when `o` is nil."
  [o k]
  (when o (aget o k)))

(def ^:private title-font "600 14px system-ui, sans-serif")
(def ^:private sub-font "400 12px ui-monospace, monospace")

(defn- text-px
  [labels font per-char]
  (if-let [doc (.-document js/globalThis)]
    (let [ctx (.getContext (.createElement doc "canvas") "2d")]
      (set! (.-font ctx) font)
      (.reduce labels (fn [m s] (max m (.-width (.measureText ctx (str s))))) 0))
    (.reduce labels (fn [m s] (max m (* per-char (.-length (str s))))) 0)))

(def ^:private sub-cap 320)
(def ^:private button-room 22)
;; Must equal the padding and border of .cg-node in cg.css.
(def ^:private card-chrome 34)
(def ^:private icon-room 16)

(defn- card-box-width
  [label sub icon?]
  (let [tw (+ (text-px #js [(str label)] title-font 8) (if icon? icon-room 0))
        sw (if sub (min sub-cap (text-px #js [(str sub)] sub-font 7.3)) 0)]
    (.ceil js/Math (max card-width (+ card-chrome (max tw sw))))))

(defn- form-loc
  "The span of the whole def form of a binding. The :location of a binding is
   the span of its RHS, which can start after the first line of the def."
  [x]
  (or (g x "form-location") (g x "location")))

(defn- visual-pos
  "The overlay position of a CgFlow node or binding as #js {:x :y}, or nil. The
   host attaches :visual {:pos [x y]} from the ^:cgproj overlay."
  [node]
  (let [v (g node "visual")
        p (when v (g v "pos"))]
    (when (and p (>= (.-length p) 2))
      #js {:x (aget p 0) :y (aget p 1)})))

(defn- unq
  "The last segment of a qualified name: \"geo/buffer\" -> \"buffer\". The
   registry keys a user op by its bare name."
  [qn]
  (let [parts (.split (str qn) "/")]
    (aget parts (dec (.-length parts)))))

(defn- strip-colon
  "Drops a leading colon."
  [s]
  (if (.startsWith s ":") (.slice s 1) s))

(defn- op-icon
  "Maps a qualified name to a sprite id (`i-X`), or nil. A user op has no icon."
  [op-name]
  (when (and op-name (not= "" (str op-name)))
    (let [s (str op-name)]
      (cond
        (.startsWith s "ds/write") "i-flow"
        (.includes s "geojson")    "i-map"
        (.startsWith s "geo/")     "i-map"
        (.includes s "csv")        "i-table"
        (.startsWith s "ds/")      "i-table"
        :else nil))))

(defn- ns-id
  "Prefixes a raw node or edge id with the name of its model. The ids stay
   unique across models and are equal to the trace pids."
  [model-name raw-id]
  (str model-name "__" raw-id))

(defn- unresolved-loc-keys
  "A JS Set of \"line:col\" span starts for the :unresolved-op diagnostics of
   the flow. The checker records each miss at the span of the call site."
  [model]
  (let [s (js/Set.)]
    (.forEach (or (g model "diagnostics") #js [])
              (fn [d]
                (when (= "unresolved-op" (g d "code"))
                  (when-let [loc (g d "location")]
                    (.add s (str (g loc "start-line") ":" (g loc "start-column")))))))
    s))

(defn- unresolved-at?
  [unres loc]
  (boolean (and loc (.has unres (str (g loc "start-line") ":" (g loc "start-column"))))))

(def ^:private wrapper-heads
  "Heads that pass their argument through. A binding with such a head takes its
   label from the inner call."
  #js ["time" "doall" "vec" "deref" "do"])

(defn- binding-head-text
  "The head that labels a binding: the first head under a pass-through wrapper,
   else the head of the binding."
  [b]
  (let [own (str (g b "head-sym"))]
    (if (>= (.indexOf wrapper-heads own) 0)
      (let [inner (aget (or (g b "args") #js []) 0)]
        (if (and inner (= "call" (g inner "kind")) (g inner "head"))
          (str (g inner "head"))
          own))
      own)))

(defn- thr-binding->graph
  "A threading_macro binding -> #js {:name :nodes :input :mode}. A node id is
   `<unq-name>-<n>`, the id that the runtime uses."
  [b]
  (let [bname (or (g b "name") "anon")
        ops   (or (g b "ops") #js [])
        nodes (.map ops
                    (fn [op i]
                      #js {:id (str (unq (g op "qualified-name")) "-" (inc i))
                           :fn (g op "qualified-name")
                           :args (g op "args")
                           :location (g op "location")
                           ;; :emits-level is dataset, groups or map.
                           :component (g op "component")
                           :emits-level (g op "emits-level")
                           :visual (g op "visual")}))]
    #js {:name bname :nodes nodes
         :input (g b "input") :mode (g b "mode")
         :location (form-loc b) :is-template? (g b "is-template")}))

(defn- sink->pseudo-binding
  "A SinkPipeline -> the binding shape that thr-binding->graph takes, named
   `sink-<i>`. A keyword or symbol source becomes :input, for the source line."
  [s i]
  (let [src (g s "source")
        input (when src
                (cond
                  (= "literal" (g src "kind"))
                  (strip-colon (str (g (g src "literal") "raw")))

                  (= "symbol_ref" (g src "kind"))
                  (g src "symbol-text")

                  :else nil))]
    #js {:name (str "sink-" (inc i))
         :ops (g s "ops")
         :mode (g s "mode")
         :input input}))

(defn- opaque-nodes
  "Opaque code blocks as read-only chips in a grid below the pipeline rows. The
   label is the first source line. The full source is in :source-preview."
  [model]
  (.map (or (g model "opaque-blocks") #js [])
        (fn [ob i]
          #js {:id (str "opaque-" (inc i))
               :type "cg"
               :style #js {:width card-width :height chip-height}
               :position #js {:x (* (mod i 6) column-width)
                              :y (+ row-y-opaque (* 90 (js/Math.floor (/ i 6))))}
               :data #js {:label (g ob "label")
                          :head (g ob "label")
                          :classification "opaque"
                          :opaque? true
                          :chip? true
                          :source-preview (g ob "raw")
                          :call-site-location (g ob "location")}})))

(defn- clip
  "The first line of `s`, cut at 60 characters."
  [s]
  (let [t (str s)
        nl (.indexOf t "\n")
        one (if (>= nl 0) (.slice t 0 nl) t)]
    (if (> (.-length one) 60) (str (.slice one 0 60) " …") one)))

(defn- output-label
  "The label of a narration row in a block: the text of a string println, a
   marker for an empty string, else the source line."
  [o]
  (let [text (g o "text")]
    (cond
      (nil? text) (g o "label")
      (= "" text) "(blank line)"
      :else (clip text))))

(def ^:private param-height 42)

;; Must equal the height of .cg-field in cg.css.
(def ^:private field-row-height 24)
;; The padding of a box and the gap after its key, which card-box-width does
;; not count.
(def ^:private field-box-chrome 24)

(defn- field-row-text
  [f]
  (str (when-let [k (g f "key")] (str (strip-colon (str k)) " ")) (g f "raw")))

(defn- param-box
  "The box of a parameter chip. A chip with fields has one row for each field
   below its name."
  [nm raw fields]
  (if fields
    #js {:width (+ field-box-chrome
                   (card-box-width nm (.reduce (.map fields field-row-text)
                                               (fn [a b] (if (> (.-length b) (.-length a)) b a))
                                               "")
                                   false))
         :height (+ (- param-height 16) (* field-row-height (.-length fields)))}
    #js {:width (card-box-width nm (clip raw) false)
         :height param-height}))

(defn- param-nodes
  "Parameters (a def with a fully literal RHS) as chips. A nested or long
   literal has no :fields and stays read only."
  [model]
  (.map (or (g model "parameters") #js [])
        (fn [p i]
          (let [nm  (g p "name")
                lit (g p "value")
                raw (str (g lit "raw"))
                fields (g p "fields")]
            #js {:id (str "param-" nm)
                 :type "cg"
                 :style (param-box nm raw fields)
                 :position (or (visual-pos p)
                               #js {:x (* (mod i 6) column-width)
                                    :y (+ row-y-param
                                          (* 90 (js/Math.floor (/ i 6))))})
                 :data #js {:label (str nm)
                            :sub (when-not fields (clip raw))
                            :fields fields
                            :head (str nm)
                            :classification "param"
                            :param? true
                            :chip? true
                            :source-preview raw
                            :call-site-location (g p "location")}}))))

(def ^:private code-cap 420)

(defn- code-box-width
  [text]
  (.ceil js/Math (max card-width
                      (min code-cap (+ card-chrome (text-px #js [(str text)] sub-font 7.3))))))

(defn- block-binder-text
  "The binder of a block on one line: the bindings of a doseq, the test of a
   when."
  [b]
  (if (= "when" (g b "kind"))
    (str (g (g b "test") "raw"))
    (let [bs (or (g b "bindings") #js [])]
      (if (> (.-length bs) 0)
        (str "["
             (.join (.map bs (fn [x] (str (g x "name") " " (g x "rhs-raw")))) ", ")
             "]")
        ""))))

(defn- block-head-text
  [b]
  (let [binder (block-binder-text b)]
    (clip (if (= "" binder) (str (g b "head")) (str (g b "head") " " binder)))))

(defn- block-row-label
  [f]
  (let [kind (g f "kind")]
    (cond
      (= "output" kind) (output-label (g f "output"))
      (= "block" kind)  (block-head-text (g f "block"))
      :else             (clip (g f "label")))))

(defn- block-row
  [label kind depth loc raw]
  #js {:label label :kind kind :depth depth :location loc :raw raw})

(defn- block-rows
  "The body of a block, a catch clause or a finally clause as a flat list of
   rows with a :depth each. A nested block or clause gives a header row first."
  [b depth]
  (let [rows (.flatMap (or (g b "body") #js [])
                       (fn [f]
                         (let [row (block-row (block-row-label f) (g f "kind") depth
                                              (g f "location")
                                              (or (g f "raw")
                                                  (g (g f "output") "raw")
                                                  (g (g f "block") "raw")))]
                           (if (= "block" (g f "kind"))
                             (.concat #js [row] (block-rows (g f "block") (inc depth)))
                             #js [row]))))
        catches (.flatMap (or (g b "catches") #js [])
                          (fn [c]
                            (.concat
                             #js [(block-row (clip (str "catch " (g c "class") " "
                                                       (g c "binding")))
                                             "catch" depth (g c "location") (g c "raw"))]
                             (block-rows c (inc depth)))))
        fin (g b "finally")]
    (.concat rows catches
             (if fin
               (.concat #js [(block-row "finally" "finally" depth
                                        (g fin "location") (g fin "raw"))]
                        (block-rows fin (inc depth)))
               #js []))))

(defn- block-nodes
  "Top-level control forms as container cards with their body rows as children.
   A row that the parser cannot model is an :expr leaf with the opaque style."
  [model]
  (.flatMap (or (g model "blocks") #js [])
            (fn [b i]
              (let [pid (str "block-" (inc i))
                    rows (block-rows b 0)
                    container
                    #js {:id pid
                         :type "cg"
                         :position #js {:x (* i (+ block-width (* 2 body-x-offset) 40))
                                        :y row-y-block}
                         :style #js {:width (+ block-width (* 2 body-x-offset))
                                     :height (+ block-card-height
                                                (* block-row-step
                                                   (max 1 (.-length rows))))}
                         :data #js {:label (block-head-text b)
                                    :head (str (g b "head"))
                                    :collapsed-width (code-box-width (block-head-text b))
                                    :classification "block"
                                    :block? true
                                    :block-kind (g b "kind")
                                    ;; apply-collapse in canvas.cljs keys on
                                    ;; :has-body?. :expanded? makes CnNode
                                    ;; stretch the card to the body.
                                    :has-body? true
                                    :expanded? true
                                    :body-handle-x (+ body-x-offset (/ block-width 2))
                                    :source-preview (g b "raw")
                                    :call-site-location (g b "location")}}
                    kids (.map rows
                               (fn [r j]
                                 (let [expr? (= "expr" (g r "kind"))
                                       depth (g r "depth")]
                                   #js {:id (str pid "__row-" (inc j))
                                        :type "cg"
                                        :parentId pid
                                        :extent "parent"
                                        :style #js {:width (- block-width
                                                              (* block-indent depth))}
                                        :position #js {:x (+ body-x-offset
                                                             (* block-indent depth))
                                                       :y (+ block-card-height
                                                             (* j block-row-step))}
                                        :data #js {:label (g r "label")
                                                   :head (g r "label")
                                                   :classification (if expr? "opaque" "block-body")
                                                   :opaque? expr?
                                                   :block-body? true
                                                   :body-kind (g r "kind")
                                                   :source-preview (g r "raw")
                                                   :call-site-location (g r "location")}})))]
                (.concat #js [container] kids)))))

(defn- defop-registry
  "{bare-name -> OperationDecl} from the :operations of the CgFlow."
  [operations]
  (let [reg #js {}]
    (.forEach (or operations #js [])
              (fn [op] (aset reg (g op "name") op)))
    reg))

(defn- defop-head-text
  "Call-site head for an op: `<name> · <N> in / <M> out`."
  [defop]
  (let [ins  (.-length (or (g defop "inputs") #js []))
        outs (.-length (or (g defop "outputs") #js []))]
    (str (g defop "name") " · " ins " in / " outs " out")))

(defn- step-defop
  "The loaded op with a body-graph that a body-graph node calls, or nil.
   :component wins over :rhs-fn and :fn, because a run-component call has that head."
  [bn registry]
  (let [nm    (or (g bn "component") (g bn "rhs-fn") (g bn "fn"))
        defop (when nm (aget registry (unq (str nm))))]
    (when (and defop (g defop "body-graph")) defop)))

(defn- step-defops
  "Each op that a body-graph node can lead to, in source order. A step with
   branches gives the call heads of its branches (:rhs-calls)."
  [bn registry]
  (if-let [direct (step-defop bn registry)]
    #js [direct]
    (.filter (.map (or (g bn "rhs-calls") #js [])
                   (fn [nm]
                     (let [d (aget registry (unq (str nm)))]
                       (when (and d (g d "body-graph")) d))))
             (fn [d] d))))

;; A JS array, not a set literal, because a squint set is callable only where
;; the compiler sees the literal.
(def ^:private control-heads
  #js ["cond" "when" "if" "if-let" "when-let" "if-not" "when-not"
       "case" "condp" "or" "and" "->" "->>" "let"])

(defn- chip-step?
  "True for a control-form step (`full? · or`, a bare `case`). The caller shows
   it as a compact chip when it resolves no callee."
  [bn]
  (.includes control-heads (unq (str (or (g bn "rhs-fn") (g bn "fn"))))))

;; The height of a chip row in a body column: the 24 px chip plus 8 px.
(def ^:private chip-row-step 32)

(defn- body-chip?
  "True when body-subnodes marks `bn` as a chip. The row layout uses the same
   test, because the container height must agree with the rows."
  [bn registry]
  (and (= 0 (.-length (step-defops bn registry)))
       (chip-step? bn)))

(defn- body-row-layout
  "The y offsets of the body rows of an op, relative to the call-site, and the
   container height. The height must be exact, because ELK uses the :style size."
  [defop registry]
  (let [bnodes (or (g (g defop "body-graph") "nodes") #js [])
        ys     #js []
        total  (.reduce bnodes
                        (fn [y bn]
                          (.push ys y)
                          (+ y (if (body-chip? bn registry)
                                 chip-row-step
                                 body-y-step)))
                        call-site-card-height)]
    #js {:ys ys
         :height (max total (+ call-site-card-height body-y-step))}))

(defn- qualified-parts
  [qn]
  (let [s (str qn)
        i (.lastIndexOf s "/")]
    (if (> i 0)
      #js [(.slice s (inc i)) (.slice s 0 i)]
      #js [s nil])))

(def ^:private branch-heads
  #js ["when" "when-not" "when-let" "when-some" "when-first" "if" "if-not"
       "if-let" "if-some" "cond" "condp" "case" "or" "and" "do" "let" "reduce"])

(defn- body-step-text
  "#js [title sub]. The sub line names the first call inside a control head,
   because the head alone hides the call that does the work."
  [bn]
  (let [bound (g bn "bound-name")
        rhs   (g bn "rhs-fn")
        calls (or (g bn "rhs-calls") #js [])]
    (cond
      bound
      #js [(str bound)
           (cond
             (and rhs (.includes branch-heads (str rhs)) (> (.-length calls) 0))
             (str rhs " · " (aget calls 0))
             rhs (str rhs)
             :else nil)]

      (g bn "is-tail") #js [(str (g bn "fn")) nil]

      :else (qualified-parts (g bn "fn")))))

(defn- body-box-width
  "One width for the whole body column keeps its centered handles in line."
  [defop]
  (let [bnodes (or (g (g defop "body-graph") "nodes") #js [])]
    (.reduce bnodes
             (fn [w bn]
               (let [t (body-step-text bn)]
                 (max w (card-box-width (aget t 0) (aget t 1) false))))
             card-width)))

(defn- body-subnodes
  "The body-graph nodes of an op as children of the call-site, ids prefixed by
   `body-model`. A step that calls an op is :divable?: one level expands inline.
   A ghost passes overlay? false: its steps take the layout, not the overlay."
  [defop call-site-pid bw body-model registry overlay?]
  (let [bg         (g defop "body-graph")
        bnodes     (or (g bg "nodes") #js [])
        src-path   (g (g defop "source-module") "path")
        row-ys     (aget (body-row-layout defop registry) "ys")]
    (.map bnodes
          (fn [bn i]
            (let [raw      (strip-colon (str (g bn "id")))
                  pid      (ns-id body-model raw)
                  vp       (when overlay? (visual-pos bn))
                  fn-name  (str (g bn "fn"))
                  text     (body-step-text bn)
                  inner    (step-defops bn registry)
                  divable? (> (.-length inner) 0)
                  chip?    (and (not divable?) (chip-step? bn))]
              #js {:id pid
                   :type "cg"
                   :parentId call-site-pid
                   :extent "parent"
                   :style (if chip?
                            #js {:width bw :height chip-height}
                            #js {:width bw})
                   ;; An overlay position is relative to the call-site.
                   :position (or vp #js {:x body-x-offset :y (aget row-ys i)})
                   :data #js {:label (aget text 0)
                              :sub (when-not chip? (aget text 1))
                              :head fn-name
                              :icon (op-icon fn-name)
                              :classification "threading"
                              :kind "defop-body-step"
                              :model-name body-model
                              :raw-id raw
                              :divable? divable?
                              :chip? chip?
                              :pinned? (boolean vp)
                              :dive-names (.map inner (fn [d] (g d "name")))
                              :call-site-location (g bn "location")
                              :source-path src-path}})))))

(defn- body-subedges
  "The edges in an expanded op call-site: body-in -> first body node, the body
   chain, and last body node -> body-out."
  [defop call-site-pid body-model]
  (let [bg         (g defop "body-graph")
        bnodes     (or (g bg "nodes") #js [])
        bedges     (or (g bg "edges") #js [])
        n          (.-length bnodes)
        first-raw  (when (> n 0) (strip-colon (str (g (aget bnodes 0) "id"))))
        last-raw   (when (> n 0) (strip-colon (str (g (aget bnodes (dec n)) "id"))))
        entry (if first-raw
                #js [#js {:id (str call-site-pid "__body-entry")
                          :source call-site-pid
                          :sourceHandle "body-in"
                          :target (ns-id body-model first-raw)
                          :sourcePosition "bottom"
                          :data #js {:kind "defop-body-entry"}}]
                #js [])
        internal (.map bedges
                       (fn [e i]
                         (let [from (strip-colon (str (g e "from")))
                               to   (strip-colon (str (g e "to")))]
                           #js {:id (str body-model "__body-e" i "-" from "-" to)
                                :source (ns-id body-model from)
                                :target (ns-id body-model to)
                                :data #js {:kind "defop-body-edge"}})))
        exit (if last-raw
               #js [#js {:id (str call-site-pid "__body-exit")
                         :source (ns-id body-model last-raw)
                         :target call-site-pid
                         :targetHandle "body-out"
                         :targetPosition "top"
                         :data #js {:kind "defop-body-exit"}}]
               #js [])]
    (.concat entry internal exit)))

(defn- node-expansion
  "Returns #js {:defop <the op whose body the node wraps, or nil> :is-map?}. The
   op of :component wins, because the body of cg.cache/checkpoint is cache code."
  [node registry]
  (let [qn         (g node "fn")
        defop-name (when qn (unq qn))
        direct     (when defop-name (aget registry defop-name))
        comp-name  (g node "component")
        comp-defop (when comp-name (aget registry comp-name))]
    #js {:defop (or comp-defop direct)
         :is-map? (boolean comp-defop)}))

(defn- port-level
  "The level of an op port: its :role, else its :type. The parser gives :role on
   inputs only."
  [p]
  (or (g p "role") (g p "type")))

(defn- node-ports
  "The named ports of an op: #js {:inputs [#js {:name :level}] :outputs [...]}."
  [defop]
  #js {:inputs  (.map (or (g defop "inputs") #js [])
                      (fn [p] #js {:name (g p "name") :level (port-level p)}))
       :outputs (.map (or (g defop "outputs") #js [])
                      (fn [p] #js {:name (g p "name") :level (port-level p)}))})

(defn- loader-node
  "One function_call binding as a flat card."
  [b idx unres]
  (let [bname    (g b "name")
        head-sym (binding-head-text b)
        vp       (visual-pos b)]
    #js {:id (str "binding-" bname)
         :type "cg"
         :style #js {:width (card-box-width bname head-sym
                                            (boolean (op-icon (str head-sym))))}
         :position (or vp #js {:x (* idx column-width) :y row-y-loader})
         :data #js {:label (str bname)
                    :sub (str head-sym)
                    :head (str head-sym)
                    :icon (op-icon (str head-sym))
                    :classification "loader"
                    ;; An overlay position: ELK must not move the node (see layout.cljs).
                    :pinned? (boolean vp)
                    :model-name (str "binding-" bname)
                    :unresolved? (unresolved-at? unres (g b "location"))
                    :call-site-location (form-loc b)}}))

(defn- component-name-arg
  "The component name (a string first argument) of a `run/run-component` call,
   or nil for any other call."
  [b]
  (when (= "run-component" (unq (str (binding-head-text b))))
    (let [a0 (aget (or (g b "args") #js []) 0)]
      (when (and a0 (= "literal" (g a0 "kind")))
        (let [lit (g a0 "literal")]
          (when (and lit (= "string" (g lit "kind")))
            (g lit "data")))))))

(defn- binding-defop
  "The op with a body-graph that a binding calls, or nil. A component name in a
   run-component argument wins over the head."
  [b registry]
  (let [head  (or (component-name-arg b) (binding-head-text b))
        defop (when head (aget registry (unq (str head))))]
    (when (and defop (g defop "body-graph")) defop)))

(defn- loader-nodes
  "One loaders-row node for each function_call binding. A binding that calls an
   op with a body becomes a call-site sized to wrap that body."
  [fc-bindings unres registry]
  (.flatMap fc-bindings
            (fn [b idx]
              (let [nd    (loader-node b idx unres)
                    defop (binding-defop b registry)]
                (if-not defop
                  #js [nd]
                  (let [pid  (aget nd "id")
                        bw   (body-box-width defop)
                        d    (aget nd "data")]
                    (aset nd "style"
                          #js {:width (max (+ bw (* 2 body-x-offset))
                                           (aget (aget nd "style") "width"))
                               :height (aget (body-row-layout defop registry)
                                             "height")})
                    (aset d "collapsed-width" (card-box-width (g d "label") (g d "sub") false))
                    (aset d "is-defop?" true)
                    (aset d "defop-name" (g defop "name"))
                    (aset d "ports" (node-ports defop))
                    (aset d "has-body?" true)
                    (aset d "expanded?" true)
                    (aset d "body-handle-x" (+ body-x-offset (/ bw 2)))
                    (.concat #js [nd] (body-subnodes defop pid bw pid registry true))))))))

;; The title and the namespace line of a step, with the padding of the card.
(def ^:private step-height 52)
(def ^:private report-row-height 24)
(def ^:private frame-title-font "600 16px system-ui, sans-serif")
(def ^:private tag-font "400 12px system-ui, sans-serif")

(defn- macro-text
  [mode]
  (if mode (str mode "->") "thread"))

(defn- frame-head-width
  "The width that the title row of a thread frame needs."
  [title tags]
  (+ 32 (text-px #js [(str title)] frame-title-font 9)
     (.reduce tags (fn [w t] (+ w 24 (text-px #js [(str t)] tag-font 7))) 0)))

(defn- port-chip-width
  [label]
  (.ceil js/Math (+ 28 (text-px #js [(str label)] sub-font 7.3))))

(defn- per-row-op?
  "True when the op that `qn` names takes a ^:row input: the runtime fuses a
   run of such steps into one."
  [qn registry]
  (let [d (aget registry (unq (str qn)))]
    (boolean (and d (.some (or (g d "inputs") #js [])
                           (fn [p] (= "row" (g p "role"))))))))

(defn- report-spec-of
  [qn args]
  (let [u (unq (str qn))]
    (when (or (= u "summarize") (= u "block-presence"))
      (let [a0 (aget (or args #js []) 0)]
        (when (and a0 (= "inline_map" (g a0 "kind")))
          #js [u (g (g a0 "inline-map") "data")])))))

(defn- report-height
  [spec bucket?]
  (let [n (.-length (or (g spec (if bucket? "buckets" "rows")) #js []))]
    (+ step-height 12 (* report-row-height (+ n (if bucket? 4 2))))))

(defn- arg-refs
  "Each parameter_ref name in an OpArg tree, at any depth."
  [args]
  (.flatMap (or args #js [])
            (fn [a]
              (let [kind (g a "kind")]
                (cond
                  (= "parameter_ref" kind) #js [(g a "parameter-name")]
                  (= "call" kind)          (arg-refs (g a "args"))
                  (= "literal" kind)       (arg-refs (g a "items"))
                  (= "inline_map" kind)
                  (.flatMap (or (g a "entries") #js [])
                            (fn [pair] (arg-refs (.filter pair (fn [x] x)))))
                  :else #js [])))))

(def ^:private port-row-height 20)

(defn- one-line
  "`s` on one line, cut at `n` characters."
  [s n]
  (let [t (.trim (.replace (str s) (js/RegExp. "\\s+" "g") " "))]
    (if (> (.-length t) n) (str (.slice t 0 n) "…") t)))

(defn- arg-text
  "An argument as one line of source."
  [a]
  (let [kind (g a "kind")]
    (cond
      (= "parameter_ref" kind) (str (g a "parameter-name"))
      (= "symbol_ref" kind)    (str (g a "symbol-text"))
      (= "literal" kind)       (one-line (g (g a "literal") "raw") 36)
      (= "inline_map" kind)    (one-line (g (g a "inline-map") "raw") 36)
      (= "call" kind)          (str "(" (g a "head") " …)")
      :else "")))

(defn- thread-slots
  "The inputs that the explicit args of a thread step fill: each input of the
   callee but the :row or :table one, which takes the threaded value."
  [callee]
  (.filter (or (g callee "inputs") #js [])
           (fn [p]
             (let [r (g p "role")]
               (not (or (= r "row") (= r "table")))))))

(defn- key-pairs
  "#js [key arg] for each entry of an inline map with a value that is not a
   literal."
  [a key-text]
  (.map (.filter (g a "entries") (fn [pair] (aget pair 1)))
        (fn [pair] #js [(key-text (arg-text (aget pair 0))) (aget pair 1)])))

(def ^:private token-re
  (js/RegExp. "\"(?:[^\"\\\\]|\\\\.)*\"|[\\[\\](){}]|[^\\s,\\[\\](){}\"]+" "g"))

(defn- opener? [t] (or (= t "(") (= t "[") (= t "{")))
(defn- closer? [t] (or (= t ")") (= t "]") (= t "}")))

(defn- join-tokens
  [tokens]
  (.reduce tokens
           (fn [acc t]
             (if (or (= acc "") (opener? (.slice acc -1)) (closer? t))
               (str acc t)
               (str acc " " t)))
           ""))

(defn- top-forms
  "The top-level forms of a token list, each as one line of text."
  [tokens]
  (let [out   #js []
        cur   #js []
        depth (atom 0)]
    (.forEach tokens
              (fn [t]
                (.push cur t)
                (cond
                  (opener? t) (swap! depth inc)
                  (closer? t) (swap! depth dec))
                (when (= 0 @depth)
                  (.push out (join-tokens (.splice cur 0))))))
    out))

(def ^:private max-open-keys 8)

(defn- literal-map-pairs
  "#js [key arg] for each entry of a literal map, from its source text. nil
   when the text is not a short list of pairs, and the map stays one row."
  [a key-text]
  (let [tokens (or (.match (str (g (g a "inline-map") "raw")) token-re) #js [])
        n      (.-length tokens)]
    (when (and (> n 2) (= "{" (aget tokens 0)) (= "}" (aget tokens (dec n))))
      (let [forms (top-forms (.slice tokens 1 (dec n)))
            pairs (/ (.-length forms) 2)]
        (when (and (js/Number.isInteger pairs) (<= pairs max-open-keys))
          (.map (.filter forms (fn [_ i] (= 0 (mod i 2))))
                (fn [k i] #js [(key-text k)
                               #js {:kind "literal" :literal #js {:raw (aget forms (inc (* 2 i)))}}])))))))

(defn- named-args
  "#js [name arg] for each explicit arg of a step, nil name for an unknown
   callee. A map arg opens to one pair for each key."
  [args slots]
  (.flatMap (or args #js [])
            (fn [a i]
              (let [slot     (aget slots i)
                    key-text (if (and slot (g slot "options")) strip-colon identity)
                    opened   (when (= "inline_map" (g a "kind"))
                               (if (g a "entries")
                                 (key-pairs a key-text)
                                 (literal-map-pairs a key-text)))]
                (or opened #js [#js [(when slot (g slot "name")) a]])))))

(defn- side-port
  "One port row of a step. A name that the frame takes as an input, or that a
   card produces, gets a line from the left; other args show as text."
  [pair i frame-inputs producers]
  (let [a      (aget pair 1)
        refs   (js/Array.from (js/Set. (arg-refs #js [a])))
        ins    (.filter refs (fn [r] (aget frame-inputs r)))
        outs   (.filter refs (fn [r] (and (not (aget frame-inputs r)) (aget producers r))))
        wired  (.concat ins outs)
        nm     (aget pair 0)
        plain? (and (= "parameter_ref" (g a "kind")) (> (.-length wired) 0))
        ;; A plain name with no parameter name shows in the row. The wire needs
        ;; no second copy of it.
        label? (and (> (.-length wired) 0)
                    (or nm (not plain?) (> (.-length ins) 0)))]
    #js {:id (str "in-" i)
         :name nm
         :text (when-not (and plain? nm) (arg-text a))
         :label (when label? (.join wired ", "))
         :wired? (> (.-length wired) 0)
         :frame-in? (and (> (.-length ins) 0) (= 0 (.-length outs)))
         :sources outs}))

(defn- side-ports
  [args callee frame-inputs producers]
  (.map (named-args args (if callee (thread-slots callee) #js []))
        (fn [pair i] (side-port pair i frame-inputs producers))))

(def ^:private port-gap 8)

(defn- port-row-px
  [p]
  (let [nm (aget p "name")
        tx (aget p "text")]
    (+ (if nm (text-px #js [nm] sub-font 7.3) 0)
       (if tx (+ port-gap 8 (text-px #js [tx] sub-font 7.3)) 0))))

(defn- step-box-width
  [label sub icon? ports]
  (.ceil js/Math
         (max (card-box-width label sub icon?)
              (+ card-chrome (.reduce ports (fn [m p] (max m (port-row-px p))) 0)))))

(def ^:private sub-line-height 16)

(defn- step-box-height
  "The height of a step card: the title, the namespace line when there is one,
   and one row for each port."
  [sub ports]
  (+ step-height
     (if sub 0 (- sub-line-height))
     (* port-row-height (.-length ports))))

(defn- below-ports
  "Moves a body step of a call-site down by the height of the port rows of
   the call-site. A step with an overlay position stays."
  [n ph]
  (when-not (aget (aget n "data") "pinned?")
    (aset (aget n "position") "y" (+ ph (aget (aget n "position") "y"))))
  n)

(defn- model-step-node
  "One step of a top-level thread as a child of its frame, plus the body of a
   call-site."
  [m frame-id node op-idx last? registry unres producers]
  (let [model-name (g m "name")
        raw-id     (str (g node "id"))
        pid        (ns-id model-name raw-id)
        qn         (g node "fn")
        exp        (node-expansion node registry)
        defop      (g exp "defop")
        is-map?    (g exp "is-map?")
        report     (report-spec-of qn (g node "args"))
        ;; The form of a report step shows its spec, and the title of a map
        ;; step names its component.
        ports      (if report
                     #js []
                     (side-ports (if is-map? (.slice (or (g node "args") #js []) 1) (g node "args"))
                                 (when-not is-map? (aget registry (unq (str qn))))
                                 #js {} producers))
        bg         (when defop (g defop "body-graph"))
        bw         (when bg (body-box-width defop))
        parts      (qualified-parts qn)
        label      (if is-map? (str (aget parts 0) " " (g node "component")) (aget parts 0))
        sub        (aget parts 1)
        box-h      (step-box-height sub ports)
        ;; The body of an open call-site starts under the port rows.
        ph         (- box-h step-height)
        icon       (when-not defop (op-icon (str qn)))
        room       (if bg button-room 0)
        vp         (visual-pos node)
        nd #js {:id pid
                :type "cg"
                :parentId frame-id
                :expandParent true
                :position (or vp #js {:x 0 :y 0})
                :data #js {:label label
                           :sub sub
                           :head (str qn)
                           :icon icon
                           :classification "threading"
                           :in-thread? true
                           :thread-index op-idx
                           :per-row? (per-row-op? qn registry)
                           :pinned? (boolean vp)
                           :model-name model-name
                           :raw-id raw-id
                           :is-defop? (boolean (and defop (not is-map?)))
                           :is-map? (boolean is-map?)
                           :is-sink? (boolean (and (g m "sink?") last?))
                           :report-op (when report (aget report 0))
                           :report-spec (when report (aget report 1))
                           :unresolved? (unresolved-at? unres (g node "location"))
                           :defop-name (when defop (g defop "name"))
                           :ports (when defop (node-ports defop))
                           :side-ports ports
                           :has-body? (boolean bg)
                           :expanded? (boolean bg)
                           :collapsed-width (when bg (+ room (step-box-width label sub false ports)))
                           :collapsed-height box-h
                           :body-handle-x (when bw (+ body-x-offset (/ bw 2)))
                           :emits-level (g node "emits-level")
                           :call-site-location (g node "location")}}]
    (aset nd "style"
          (cond
            bg #js {:width (+ bw (* 2 body-x-offset))
                    :height (+ ph (aget (body-row-layout defop registry) "height"))}
            report #js {:width (max 320 (card-box-width label sub (boolean icon)))
                        :height (report-height (aget report 1) (= "summarize" (aget report 0)))}
            :else #js {:width (step-box-width label sub (boolean icon) ports)
                       :height box-h}))
    (if bg
      (.concat #js [nd]
               (.map (body-subnodes defop pid bw (str (g defop "name") "-body") registry true)
                     (fn [sub-node] (below-ports sub-node ph))))
      #js [nd])))

(defn- op-step-node
  "One step of an op body thread as a child of its frame. A step that calls an
   op with a body opens a dive, because only one level expands inline."
  [op frame-id prefix bn i registry frame-inputs producers]
  (let [raw   (strip-colon (str (g bn "id")))
        qn    (g bn "fn")
        parts (qualified-parts qn)
        label (aget parts 0)
        sub   (aget parts 1)
        inner (step-defops bn registry)
        icon  (op-icon (str qn))
        room  (if (> (.-length inner) 0) button-room 0)
        ports (side-ports (g bn "args") (aget registry (unq (str qn))) frame-inputs producers)
        vp    (visual-pos bn)]
    #js {:id (ns-id prefix raw)
         :type "cg"
         :parentId frame-id
         :expandParent true
         :position (or vp #js {:x 0 :y 0})
         :style #js {:width (+ room (step-box-width label sub (boolean icon) ports))
                     :height (step-box-height sub ports)}
         :data #js {:label label
                    :sub sub
                    :head (str qn)
                    :icon icon
                    :side-ports ports
                    :classification "threading"
                    :kind "defop-body-step"
                    :in-thread? true
                    :thread-index i
                    :per-row? (per-row-op? qn registry)
                    :pinned? (boolean vp)
                    :model-name prefix
                    :raw-id raw
                    :divable? (> (.-length inner) 0)
                    :dive-names (.map inner (fn [d] (g d "name")))
                    :emits-level (g bn "emits-level")
                    :call-site-location (g bn "location")
                    :source-path (g (g op "source-module") "path")}}))

(defn- step-column-width
  "One width for the plain and closed steps of a frame keeps the cards of the
   thread in one column."
  [steps]
  (.reduce steps
           (fn [w n]
             (let [d (aget n "data")]
               (max w (if (aget d "has-body?")
                        (aget d "collapsed-width")
                        (aget (aget n "style") "width")))))
           card-width))

(defn- even-column!
  [steps]
  (let [cw (step-column-width steps)]
    (.forEach steps
              (fn [n]
                (let [d (aget n "data")]
                  (if (aget d "has-body?")
                    (aset d "collapsed-width" cw)
                    (aset (aget n "style") "width" (max cw (aget (aget n "style") "width")))))))
    cw))

(defn- frame-node
  "A thread frame: a React Flow parent node whose children are the steps.
   layout.cljs stacks the steps and sizes the frame."
  [id title mode kind source result model-name loc extra]
  (let [tags #js [(macro-text mode) kind]]
    #js {:id id
         :type "cnThread"
         :position #js {:x 0 :y 0}
         :data (js/Object.assign
                #js {:label (str title)
                     :head (str title)
                     :mode mode
                     :macro (macro-text mode)
                     :kind kind
                     :source source
                     :source-width (port-chip-width source)
                     :result result
                     :result-width (port-chip-width result)
                     :head-width (frame-head-width title tags)
                     :classification "thread"
                     :thread-frame? true
                     :model-name model-name
                     :call-site-location loc}
                extra)}))

(defn- step-ids
  [steps]
  (.map (.filter steps (fn [n] (aget (aget n "data") "in-thread?")))
        (fn [n] (aget n "id"))))

(defn- thread-line-edges
  "The thread line of a frame: source port, each step in order, result port."
  [frame-id ids mode levels]
  (let [stops (.concat #js [nil] ids #js [nil])]
    (.map (.slice stops 1)
          (fn [to i]
            (let [from  (aget stops i)
                  level (when from (aget levels i))]
              #js {:id (str frame-id "__line-" i)
                   :source (or from frame-id)
                   :sourceHandle (if from "thread-out" "thread-src")
                   :target (or to frame-id)
                   :targetHandle (if to "thread-in" "thread-res")
                   :type (if (= level "groups") "cnGroupsEdge" "cnThread")
                   :className (str "cg-thread-line mode-" (or mode "none")
                                   (when level (str " cg-edge-" level)))
                   :data #js {:kind "thread-line" :mode mode :level level}})))))

(defn- step-levels
  [steps]
  (.map (.filter steps (fn [n] (aget (aget n "data") "in-thread?")))
        (fn [n] (aget (aget n "data") "emits-level"))))

(def ^:private wire-run 24)
(def ^:private wire-label-chrome 28)

(defn- step-ports
  [n]
  (or (aget (aget n "data") "side-ports") #js []))

(defn- gutter-width
  "The room at the left of the step column for the names of the side inputs.
   0 when no step has one."
  [steps]
  (let [labels (.flatMap steps
                         (fn [n]
                           (.map (.filter (step-ports n) (fn [p] (aget p "label")))
                                 (fn [p] (aget p "label")))))]
    (cond
      (> (.-length labels) 0)
      (.ceil js/Math (+ wire-run wire-label-chrome (text-px labels sub-font 7.3)))

      (.some steps (fn [n] (.some (step-ports n) (fn [p] (> (.-length (aget p "sources")) 0)))))
      wire-run

      :else 0)))

(defn- lane-index
  "An offset index for each distinct card that feeds a frame, because two lines
   down the side of the frame must not lie on top of each other."
  [refs]
  (let [m #js {}
        n (atom 0)]
    (.forEach refs (fn [r] (when-not (some? (aget m r)) (aset m r @n) (swap! n inc))))
    m))

(defn- side-line-edges
  "A thin line from each card that a step names to the port that takes it."
  [steps producers model-name]
  (let [lanes (lane-index (.flatMap steps
                                    (fn [n] (.flatMap (step-ports n)
                                                      (fn [p] (aget p "sources"))))))]
    (.flatMap steps
              (fn [n]
                (let [nid (aget n "id")]
                  (.flatMap (step-ports n)
                            (fn [p]
                              (.map (aget p "sources")
                                    (fn [ref]
                                      (let [src (aget producers ref)]
                                        #js {:id (str "oa-" ref "-" nid "-" (aget p "id"))
                                             :source (g src "id")
                                             :sourceHandle (g src "handle")
                                             :target nid
                                             :targetHandle (aget p "id")
                                             :type "cnLane"
                                             :className "cg-side-line"
                                             :data #js {:kind "op-arg"
                                                        :lane (aget lanes ref)
                                                        :producer ref
                                                        :consumer model-name}}))))))))))

(defn- model-thread
  "The frame, steps, thread line and side lines of one top-level thread."
  [m registry unres producers]
  (let [nm       (g m "name")
        graph    (g m "graph")
        mode     (g graph "mode")
        frame-id (str "model-" nm)
        op-nodes (or (g graph "nodes") #js [])
        n        (.-length op-nodes)
        kids     (.flatMap op-nodes
                           (fn [node i]
                             (model-step-node m frame-id node i (= i (dec n))
                                              registry unres producers)))
        steps    (.filter kids (fn [k] (aget (aget k "data") "in-thread?")))
        cw       (even-column! steps)
        frame    (frame-node frame-id
                             (if (g m "sink?") "sink" nm)
                             mode
                             (if (g m "sink?") "sink" "def")
                             (or (g graph "input") "?")
                             "result"
                             nm
                             (g graph "location")
                             #js {:column-width cw
                                  :inputs-width (gutter-width steps)
                                  :is-template? (g graph "is-template?")})]
    #js {:nodes (.concat #js [frame] kids)
         :edges (.concat (thread-line-edges frame-id (step-ids kids) mode (step-levels kids))
                         (side-line-edges steps producers nm))}))

(defn- op-frame-inputs
  "The inputs of an op that its steps can name: each input but the one that
   the thread reads."
  [op]
  (let [src (g (g op "body-graph") "input")
        s   #js {}]
    (.forEach (or (g op "inputs") #js [])
              (fn [p] (let [n (g p "name")] (when (not= n src) (aset s n true)))))
    s))

(defn- thread-op?
  "True for an op of this file whose body is one threading form."
  [op]
  (boolean (and (not (g op "source-module"))
                (g (g op "body-graph") "mode"))))

(defn- op-result-label
  [op]
  (let [o (aget (or (g op "outputs") #js []) 0)]
    (if o
      (str (g o "name") (when (g o "type") (str " : " (g o "type"))))
      "result")))

(defn- op-thread
  "The frame, steps and thread line of an op whose body is a thread. A step id in
   `taken` gets the frame prefix, because React Flow drops a duplicate id."
  [op registry taken producers]
  (let [nm       (g op "name")
        bg       (g op "body-graph")
        mode     (g bg "mode")
        frame-id (str "op-" nm)
        bnodes   (or (g bg "nodes") #js [])
        pid-pfx  (str nm "-body")
        clash?   (.some bnodes (fn [bn] (aget taken (ns-id pid-pfx (strip-colon (str (g bn "id")))))))
        prefix   (if clash? frame-id pid-pfx)
        inputs   (op-frame-inputs op)
        steps    (.map bnodes (fn [bn i]
                                (op-step-node op frame-id prefix bn i registry inputs producers)))
        cw       (even-column! steps)
        frame    (frame-node frame-id nm mode "op" (or (g bg "input") "?")
                             (op-result-label op) prefix (g op "location")
                             #js {:column-width cw
                                  :inputs-width (gutter-width steps)})]
    #js {:nodes (.concat #js [frame] steps)
         :edges (.concat (thread-line-edges frame-id (.map steps (fn [s] (aget s "id")))
                                            mode (step-levels steps))
                         (side-line-edges steps producers prefix))}))

(defn- model-body-edges
  "The body subgraph edges for each op call-site in each model."
  [models registry]
  (.flatMap models
            (fn [m]
              (let [model-name (g m "name")
                    nodes      (or (g (g m "graph") "nodes") #js [])]
                (.flatMap nodes
                          (fn [node]
                            (let [defop (g (node-expansion node registry) "defop")]
                              (if (and defop (g defop "body-graph"))
                                (body-subedges defop
                                               (ns-id model-name (str (g node "id")))
                                               (str (g defop "name") "-body"))
                                #js []))))))))

(defn- binding-body-edges
  "The body subgraph edges for each binding that calls an op."
  [fc-bindings registry]
  (.flatMap fc-bindings
            (fn [b]
              (let [defop (binding-defop b registry)]
                (if defop
                  (let [pid (str "binding-" (g b "name"))]
                    (body-subedges defop pid pid))
                  #js [])))))

(defn- param-refs
  [b]
  (arg-refs (g b "args")))

(defn- producer
  [id handle]
  #js {:id id :handle handle})

(defn- producer-ids
  "binding name -> #js {:id :handle} of what produces its value. The value of a
   thread leaves the result port of its frame."
  [fc-bindings models params]
  (let [m #js {}]
    (.forEach (or params #js [])
              (fn [p] (aset m (g p "name") (producer (str "param-" (g p "name")) nil))))
    (.forEach fc-bindings
              (fn [b] (aset m (g b "name") (producer (str "binding-" (g b "name")) nil))))
    (.forEach models
              (fn [mo]
                (when-not (g mo "sink?")
                  (aset m (g mo "name") (producer (str "model-" (g mo "name")) "res-out")))))
    m))

(defn- binding-ref-edges
  "producer -> consumer edges for each binding whose args name a parameter or a
   binding of any kind."
  [fc-bindings producers]
  (.flatMap fc-bindings
            (fn [b]
              (let [bn (g b "name")]
                (.map (.filter (param-refs b)
                               (fn [ref] (and (aget producers ref) (not= ref bn))))
                      (fn [ref]
                        (let [p (aget producers ref)
                              e #js {:id (str "be-" ref "-" bn)
                                     :source (g p "id")
                                     :sourceHandle (g p "handle")
                                     :target (str "binding-" bn)
                                     :data #js {:kind "binding-ref"
                                                :producer ref :consumer bn}}]
                          ;; A line out of a frame starts inside it, under the
                          ;; frame unless it is raised.
                          (when (g p "handle") (aset e "zIndex" 1))
                          e)))))))

(defn- thread-source-edges
  "The line from the card that a thread reads to the top of its frame, above
   the source port."
  [models producers]
  (.flatMap models
            (fn [m]
              (let [graph (g m "graph")
                    input (g graph "input")
                    p     (when input (aget producers input))
                    fid   (str "model-" (g m "name"))]
                (if (and p (not= (g p "id") fid))
                  #js [#js {:id (str "src__" input "->" fid)
                            :source (g p "id")
                            :sourceHandle (g p "handle")
                            :target fid
                            :targetHandle "src-in"
                            :type "cnSource"
                            :className (str "cg-thread-line mode-" (or (g graph "mode") "none"))
                            :data #js {:kind "thread-source" :producer input}}]
                  #js [])))))

(defn- id-set
  [nodes]
  (let [s #js {}]
    (.forEach nodes (fn [n] (aset s (aget n "id") true)))
    s))

(defn model->react-flow
  "CgFlow (JS object) -> #js {:nodes [...] :edges [...]} for @xyflow."
  [model]
  (let [bindings   (or (g model "bindings") #js [])
        fc         (.filter bindings (fn [b] (= "function_call"  (g b "body-shape"))))
        thr        (.filter bindings (fn [b] (= "threading_macro" (g b "body-shape"))))
        registry   (defop-registry (g model "operations"))
        unres      (unresolved-loc-keys model)
        thr-models (.map thr (fn [b] #js {:name (or (g b "name") "anon")
                                          :graph (thr-binding->graph b)}))
        sink-models (.map (or (g model "sinks") #js [])
                          (fn [s i]
                            (let [pb (sink->pseudo-binding s i)]
                              #js {:name (g pb "name")
                                   :graph (thr-binding->graph pb)
                                   :sink? true})))
        models     (.concat thr-models sink-models)
        params     (or (g model "parameters") #js [])
        producers  (producer-ids fc models params)
        threads    (.map models (fn [m] (model-thread m registry unres producers)))
        ;; :outputs has no nodes, because the console pane shows the output of
        ;; a file.
        base-nodes (.concat (param-nodes model)
                            (loader-nodes fc unres registry)
                            (.flatMap threads (fn [t] (aget t "nodes")))
                            (block-nodes model)
                            (opaque-nodes model))
        taken      (id-set base-nodes)
        op-threads (.map (.filter (or (g model "operations") #js []) thread-op?)
                         (fn [op] (op-thread op registry taken producers)))
        nodes (.concat base-nodes (.flatMap op-threads (fn [t] (aget t "nodes"))))
        edges (.concat (binding-ref-edges fc producers)
                       (binding-body-edges fc registry)
                       (thread-source-edges models producers)
                       (.flatMap threads (fn [t] (aget t "edges")))
                       (.flatMap op-threads (fn [t] (aget t "edges")))
                       (model-body-edges models registry))]
    #js {:nodes nodes :edges edges}))

(def ^:private dive-head-height 120)

;; The head card and the body steps share an id prefix, and the raw id of a
;; step is its bound name. This suffix must not be a bound name, because React
;; Flow drops the second node of a duplicate id.
(def ^:private dive-head-suffix "__cg-dive-head")

(defn- arity-text
  [defop]
  (str (.-length (or (g defop "inputs") #js [])) " in / "
       (.-length (or (g defop "outputs") #js [])) " out"))

(defn- dive-head-node
  "The card of the op that a dive shows. It uses the location of the first input
   port, because an OperationDecl has no span."
  [defop body-model]
  (let [head (str (g defop "name"))
        sub  (arity-text defop)
        in0  (aget (or (g defop "inputs") #js []) 0)]
    #js {:id (str body-model dive-head-suffix)
         :type "cg"
         :style #js {:width (card-box-width head sub false) :height dive-head-height}
         :position #js {:x 0 :y 0}
         :data #js {:label head
                    :sub sub
                    :head head
                    :classification "model-head"
                    :model-head? true
                    :model-name body-model
                    :defop-name (g defop "name")
                    :ports (node-ports defop)
                    :call-site-location (g in0 "location")
                    :source-path (g (g defop "source-module") "path")}}))

(defn- dive-step-nodes
  "The body-graph of one op as top-level nodes (the dive view), with ids equal to
   the trace pids. A step with branches gets one dive target for each branch."
  [defop body-model registry]
  (let [bnodes   (or (g (g defop "body-graph") "nodes") #js [])
        src-path (g (g defop "source-module") "path")]
    (.flatMap bnodes
              (fn [bn i]
                (let [raw     (strip-colon (str (g bn "id")))
                      pid     (ns-id body-model raw)
                      fn-name (str (g bn "fn"))
                      text    (body-step-text bn)
                      title   (aget text 0)
                      sub     (aget text 1)
                      inner   (step-defop bn registry)
                      ;; Only a step with branches needs these. An inline
                      ;; call-site shows its one body.
                      branches (when-not inner (step-defops bn registry))
                      divable? (boolean (and branches (> (.-length branches) 0)))
                      chip?    (and (not inner) (not divable?) (chip-step? bn))
                      bw      (when inner (body-box-width inner))
                      nd #js {:id pid
                              :type "cg"
                              :position #js {:x 0 :y (* (inc i) node-height)}
                              :style (cond
                                       inner #js {:width (max (card-box-width title sub false)
                                                              (+ bw (* 2 body-x-offset)))
                                                  :height (aget (body-row-layout inner registry)
                                                                "height")}
                                       chip? #js {:width (card-box-width title nil false)
                                                  :height chip-height}
                                       :else #js {:width (card-box-width title sub
                                                                         (boolean (op-icon fn-name)))})
                              :data #js {:label title
                                         :sub (when-not chip? sub)
                                         :head fn-name
                                         :icon (when-not inner (op-icon fn-name))
                                         :classification "threading"
                                         :kind "defop-body-step"
                                         :model-name body-model
                                         :raw-id raw
                                         :is-defop? (boolean inner)
                                         :defop-name (when inner (g inner "name"))
                                         :ports (when inner (node-ports inner))
                                         :has-body? (boolean inner)
                                         :expanded? (boolean inner)
                                         :divable? divable?
                                         :chip? chip?
                                         :dive-names (when branches
                                                       (.map branches
                                                             (fn [d] (g d "name"))))
                                         :body-handle-x (when inner
                                                          (+ body-x-offset (/ bw 2)))
                                         :call-site-location (g bn "location")
                                         :source-path src-path}}]
                  (if inner
                    (.concat #js [nd] (body-subnodes inner pid bw pid registry true))
                    #js [nd]))))))

(defn defop->react-flow
  "The body of one op as a whole canvas (the dive view). An unknown name gives an
   empty canvas, because the host can post a new flow while a dive is open."
  [defop-name model]
  (let [registry (defop-registry (g model "operations"))
        defop    (aget registry (str defop-name))]
    (if-not (and defop (g defop "body-graph"))
      #js {:nodes #js [] :edges #js []}
      (let [body-model (str (g defop "name") "-body")
            bnodes     (or (g (g defop "body-graph") "nodes") #js [])
            bedges     (or (g (g defop "body-graph") "edges") #js [])
            head       (dive-head-node defop body-model)
            steps      (dive-step-nodes defop body-model registry)
            first-raw  (when (> (.-length bnodes) 0)
                         (strip-colon (str (g (aget bnodes 0) "id"))))
            head-edge  (if first-raw
                         #js [#js {:id (str body-model dive-head-suffix "-e")
                                   :source (aget head "id")
                                   :target (ns-id body-model first-raw)
                                   :data #js {:kind "model-head"
                                              :model-name body-model}}]
                         #js [])
            chain      (.map bedges
                             (fn [e i]
                               (let [from (strip-colon (str (g e "from")))
                                     to   (strip-colon (str (g e "to")))]
                                 #js {:id (str body-model "__e" i "-" from "-" to)
                                      :source (ns-id body-model from)
                                      :target (ns-id body-model to)
                                      :data #js {:kind "internal"
                                                 :model-name body-model}})))
            inner-edges (.flatMap bnodes
                                  (fn [bn]
                                    (let [inner (step-defop bn registry)]
                                      (if inner
                                        (let [pid (ns-id body-model
                                                         (strip-colon (str (g bn "id"))))]
                                          (body-subedges inner pid pid))
                                        #js []))))]
        #js {:nodes (.concat #js [head] steps)
             :edges (.concat head-edge chain inner-edges)}))))

(defn ghost-piece
  "The #js {:nodes :edges} for a ghost from the toolbar: an expanded call-site
   when `fn-name` resolves to a loaded op with a body, else one plain node."
  [ghost-id fn-name x y operations]
  (let [registry   (defop-registry operations)
        defop-name (unq (str fn-name))
        defop      (aget registry defop-name)
        bg         (when defop (g defop "body-graph"))]
    (if bg
      (let [head       (defop-head-text defop)
            bw         (body-box-width defop)
            nd #js {:id ghost-id
                    :type "cg"
                    :position #js {:x x :y y}
                    :style #js {:width (+ bw (* 2 body-x-offset))
                                :height (aget (body-row-layout defop registry)
                                              "height")}
                    :data #js {:label head
                               :head head
                               :classification "ghost"
                               :kind "ghost"
                               :model-name ghost-id
                               :is-defop? true
                               :defop-name (g defop "name")
                               :body-handle-x (+ body-x-offset (/ bw 2))
                               :has-body? true
                               :expanded? true}}]
        #js {:nodes (.concat #js [nd] (body-subnodes defop ghost-id bw ghost-id registry false))
             :edges (body-subedges defop ghost-id ghost-id)})
      #js {:nodes #js [#js {:id ghost-id
                            :type "cg"
                            :position #js {:x x :y y}
                            :data #js {:label fn-name
                                       :head fn-name
                                       :icon (op-icon (str fn-name))
                                       :classification "ghost"
                                       :kind "ghost"
                                       :model-name ghost-id}}]
           :edges #js []})))
