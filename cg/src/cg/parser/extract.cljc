;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.extract
  "Strict shape recognition on a tree-sitter Tree. `extract` classifies each
   top-level form of a `.cg` file into a CgFlow, and reports each form outside
   the accepted shapes as a diagnostic. The rules across forms are in
   cg.parser.validate, and type flow is in cg.parser.types.

   This namespace must stay free of eval: it must not require cg.dsl,
   cg.macros, cg.run or cg.codegen (cg.dsl.signatures is pure data)."
  (:require [cg.dsl.signatures :as sigs]
            [cg.parser.walk :as w]
            [cg.util :as util]
            [clojure.string :as str]))

#?(:clj (set! *warn-on-reflection* true))

(defn- diag
  ([code severity message location]
   {:code code :severity severity :message message :location location})
  ([code message location]
   (diag code :error message location)))

(def threading-macro-syms
  "The names of the threading macros that start a pipeline body."
  #{"item->" "coll->" "table->" "layers->"})

(def forbidden-def-heads
  "Top-level heads that are an error in a flow file, each with the message
   fragment for the :forbidden-form diagnostic."
  {"deftype"     "deftype"
   "defrecord"   "defrecord"
   "defprotocol" "defprotocol"
   "defmacro"    "macro definitions"})

(def ^:private control-form-heads
  "Clojure control forms, and await, that can head the RHS of a def. None is
   an operation. A binding with such a head gets :is-control and a
   :control-form-binding diagnostic, and no lookup in the signature registry.
   Keep the set narrow: each head here is a form that the parser claims to
   recognize."
  #{"or" "and" "try" "if" "when" "cond" "condp" "case"
    "let" "if-let" "when-let" "do" "await"})

(def legacy-macro-namespaces
  "Namespaces that do not provide the threading macros. A require that refers
   a threading macro from one of them gets a :legacy-macro-namespace warning."
  #{"cg.dsl" "cg.dsl.macros"})

(defn- str-lit-text
  "The source text of a string-literal node without its quotes."
  [str-lit-node]
  (let [text (w/node-text str-lit-node)]
    (if (and (>= (count text) 2)
             (= \" (.charAt ^String text 0))
             (= \" (.charAt ^String text (dec (count text)))))
      (subs text 1 (dec (count text)))
      text)))

(def ^:private undecodable
  "The `:data` value of decode-literal for a value that does not decode. It
   cannot be nil, because nil is a literal that a source file can write."
  ::undecodable)

(declare decode-literal)

(defn- decode-scalar
  "The leaf half of decode-literal: `{:kind ... :data ...}` for a literal
   that has no nested forms, else nil. A number whose source text does not
   read gets `undecodable` as :data."
  [node]
  (cond
    (w/num? node)
    {:kind :number
     :data (let [t (w/node-text node)]
             #?(:clj  (try (let [v (read-string t)]
                             (if (number? v) v undecodable))
                           (catch Exception _ undecodable))
                :cljs (let [v (js/Number t)]
                        (if (js/Number.isNaN v) undecodable v))))}

    (w/str-lit? node)
    {:kind :string :data (str-lit-text node)}

    (w/kwd? node)
    {:kind :keyword
     :data (let [t (w/node-text node)]
             #?(:clj  (keyword (subs t 1))
                :cljs (subs t 1)))}

    (w/bool-lit? node)
    {:kind :boolean :data (= "true" (w/node-text node))}

    (w/nil-lit? node)
    {:kind :nil :data nil}

    :else nil))

(defn- decode-collection-data
  "The decoded plain value of a vec, map or set node, or `undecodable`. A set
   never decodes: it has no JSON form, and a vector in its place would change
   the meaning of the source. One undecodable entry makes the whole value
   undecodable."
  [kind kids]
  (let [decoded (mapv (fn [c] (get (decode-literal c) :data undecodable)) kids)
        bad? (some (fn [x] (= undecodable x)) decoded)]
    (case kind
      :vec  (if bad? undecodable decoded)
      :mapv (if (or bad? (odd? (count kids)))
              undecodable
              (into {} (mapv vec (partition 2 decoded))))
      :set  undecodable)))

(defn- decode-literal
  "Decode a literal node into {:kind :deep? :data}, or nil when node is not a
   literal shape. :deep? is true when each nested form is also a literal, and
   :data is the plain value or `undecodable`. `[:a (f x)]` has :kind :vec but
   is not deep, because a def whose RHS holds a call must not classify as a
   literal parameter. A set and a literal nil count as deep."
  [node]
  (if-let [scalar (decode-scalar node)]
    (assoc scalar :deep? true)
    (let [kind (cond
                 (w/vec-form? node) :vec
                 (w/map-form? node) :mapv
                 (w/set-form? node) :set
                 :else nil)]
      (when kind
        (let [kids (w/named-children node)]
          {:kind kind
           :deep? (every? (fn [c] (get (decode-literal c) :deep? false)) kids)
           :data (decode-collection-data kind kids)})))))

(defn- literal-value
  "A LiteralValue map {:kind ... :raw <source text>}, or nil. A literal that
   decodes also carries :data, the plain value. A set anywhere inside removes
   :data for the whole value."
  [node]
  (when-let [{:keys [kind data]} (decode-literal node)]
    (cond-> {:kind kind :raw (w/node-text node)}
      (not= undecodable data) (assoc :data data))))

(defn- literal-kind
  "The LiteralValue kind keyword if node is a literal shape, else nil."
  [node]
  (:kind (decode-literal node)))

(defn- literal-deep?
  "True when node is a literal whose every nested form is also a literal."
  [node]
  (boolean (:deep? (decode-literal node))))

(def ^:private max-literal-fields
  "A literal with more members than this has no :fields: a host shows it
   read only."
  8)

(defn- literal-field
  "One scalar that a host can edit in place: {:kind :raw :location}, and
   :key (the source text of its map key) when `key-node` is given."
  [node key-node]
  (cond-> {:kind (:kind (decode-scalar node))
           :raw (w/node-text node)
           :location (w/node->span node)}
    key-node (assoc :key (w/node-text key-node))))

(defn- literal-fields
  "The scalars of a literal, each with its own span, for an edit of one
   value in the source text: the scalar itself, the values of a flat map or
   the members of a flat vector. nil for a nested, long or empty literal."
  [node]
  (let [kids (when (or (w/map-form? node) (w/vec-form? node))
               (w/named-children node))
        flat? (and (seq kids) (every? decode-scalar kids))]
    (cond
      (decode-scalar node)
      [(literal-field node nil)]

      (and flat? (w/map-form? node)
           (even? (count kids))
           (<= (count kids) (* 2 max-literal-fields)))
      (mapv (fn [[k v]] (literal-field v k)) (partition 2 kids))

      (and flat? (w/vec-form? node) (<= (count kids) max-literal-fields))
      (mapv (fn [v] (literal-field v nil)) kids)

      :else nil)))

(defn- extract-libspec
  "Read {:namespace :alias :referred :location} from a quoted vector
   libspec, or return {::error <reason>}."
  [quoting-node]
  (let [inner (first (w/named-children quoting-node))]
    (if (and inner (w/vec-form? inner))
      (let [children (w/named-children inner)
            ns-node (first children)
            opts (rest children)]
        (if (and ns-node (w/sym? ns-node))
          (let [namespace (w/node-text ns-node)
                pairs (partition-all 2 opts)
                m (reduce (fn [acc [k v]]
                            (cond
                              (and (w/kwd? k) (= ":as" (w/node-text k))
                                   (and v (w/sym? v)))
                              (assoc acc :alias (w/node-text v))

                              (and (w/kwd? k) (= ":refer" (w/node-text k))
                                   (and v (w/vec-form? v)))
                              (assoc acc :referred
                                     (->> (w/named-children v)
                                          (filter w/sym?)
                                          (mapv w/node-text)))

                              :else acc))
                          {:namespace namespace :alias nil :referred []}
                          pairs)]
            (assoc m :location (w/node->span quoting-node)))
          {::error :ns-not-symbol}))
      {::error :not-a-vec})))

(defn- extract-require
  "Parse a (require '[<ns> :as|:refer ...]) form into a NamespaceImport map
   or a Diagnostic. This fn reads only the first libspec of a require."
  [list-node]
  (let [args (rest (w/named-children list-node))
        first-arg (first args)]
    (cond
      (not first-arg)
      (diag :require-malformed
            "(require ...) needs a quoted libspec like '[cg.geo :as geo]"
            (w/node->span list-node))

      (not (w/quoting? first-arg))
      (diag :require-malformed
            "require libspec must be quoted: (require '[<ns> ...])"
            (w/node->span first-arg))

      :else
      (let [r (extract-libspec first-arg)]
        (if (::error r)
          (diag :require-malformed
                (case (::error r)
                  :not-a-vec      "require libspec must be a vector: '[<ns> ...]"
                  :ns-not-symbol  "require libspec must start with a namespace symbol")
                (w/node->span first-arg))
          r)))))

(defn- first-line
  [s]
  (let [i (.indexOf ^String s "\n")]
    (if (neg? i) s (subs s 0 i))))

(defn- truncate-label
  [s n]
  (if (> (count s) n) (str (subs s 0 n) " …") s))

(defn- opaque-block
  "An OpaqueBlock entry for a form that the classifier rejects: :label (the
   first source line, truncated), :raw (the full source) and :location.
   extract records it only with :opaque-blocks true."
  [node]
  (let [text (w/node-text node)]
    {:label (truncate-label (first-line text) 60)
     :raw text
     :location (w/node->span node)}))

(defn- extract-output
  "Parse a top-level (println ...) form into an OutputDecl. A println is
   narration, not dataflow, and it gets its own node kind in place of an
   opaque card with a diagnostic. `:text` is the string when the one argument
   is a string literal, else nil."
  [list-node]
  (let [args (vec (rest (w/named-children list-node)))
        text (w/node-text list-node)
        sole (when (= 1 (count args)) (first args))]
    {:head "println"
     :arity (count args)
     :text (when (and sole (w/str-lit? sole)) (str-lit-text sole))
     :args (mapv w/node-text args)
     :label (truncate-label (first-line text) 60)
     :raw text
     :location (w/node->span list-node)}))

(defn- extract-load-module
  "Parse a (run/load-module \"path\") form and capture the path string. The
   parser does not follow the path: the caller decides."
  [list-node]
  (let [children  (w/named-children list-node)
        path-node (nth children 1 nil)
        span      (w/node->span list-node)]
    (cond
      (nil? path-node)
      {:kind :error
       :diagnostics [(diag :load-module-malformed
                           "(run/load-module \"path\") requires a string path argument"
                           span)]}

      (not (w/str-lit? path-node))
      {:kind :error
       :diagnostics [(diag :load-module-malformed
                           "(run/load-module \"path\") path must be a string literal"
                           (w/node->span path-node))]}

      :else
      {:kind :load-module
       :value {:path (str-lit-text path-node)
               :location span}
       :diagnostics []})))

(defn- extract-module-ns
  "Parse a top-level (ns <name> ...) form and record the name. A module file
   that run/load-module loads must start with an ns form, and a module
   follower uses the name to link a require alias to the operations of the
   module. The parser does not read the docstring or the (:require ...)
   clauses of the ns form."
  [list-node]
  (let [children  (w/named-children list-node)
        name-node (nth children 1 nil)
        span      (w/node->span list-node)]
    (if (and name-node (w/sym? name-node))
      {:kind :module-ns
       ;; sym-name, not node-text: the text of a name with metadata
       ;; includes the metadata.
       :value {:name (w/sym-name name-node)
               :location span}
       :diagnostics []}
      {:kind :error
       :diagnostics [(diag :module-ns-malformed
                           "(ns ...) requires a symbol namespace name"
                           span)]})))

(defn- extract-op-arg
  [node]
  (let [span (w/node->span node)]
    (cond
      (w/quoting? node)
      ;; A quoted op form is illegal. decode-threading-form emits
      ;; :quoted-op-form for it.
      {:kind :symbol_ref :symbol-text (w/node-text node) :location span}

      (w/list-form? node)
      ;; A nested call, such as (ds/load-geojson (str data-dir "...")),
      ;; becomes a recursive OpArg tree. A consumer can then find the
      ;; parameter refs and the literals inside.
      (let [children (w/named-children node)
            head     (first children)
            arg-nodes (rest children)]
        {:kind :call
         :head (when head (w/node-text head))
         :args (mapv extract-op-arg arg-nodes)
         :location span})

      (literal-kind node)
      ;; A collection with a member that is not a literal keeps its members
      ;; as OpArgs. `{:rows parcels}` is how a run/run-model call names its
      ;; input, and the reference must not live only in the raw text. A fully
      ;; literal collection decodes to :data and has neither key.
      (let [kind (literal-kind node)
            deep? (literal-deep? node)
            kids (w/named-children node)]
        (cond
          (= :mapv kind)
          (cond-> {:kind :inline_map :inline-map (literal-value node) :location span}
            (not deep?)
            (assoc :entries (mapv (fn [[k v]]
                                    [(extract-op-arg k)
                                     (when v (extract-op-arg v))])
                                  (partition-all 2 kids))))

          :else
          (cond-> {:kind :literal :literal (literal-value node) :location span}
            (and (not deep?) (contains? #{:vec :set} kind))
            (assoc :items (mapv extract-op-arg kids)))))

      (w/sym? node)
      (let [text (w/node-text node)]
        ;; A parameter ref is a local symbol (no /). A symbol with a
        ;; namespace, such as `p/threshold`, is a symbol_ref.
        (if (>= (.indexOf ^String text "/") 0)
          {:kind :symbol_ref :symbol-text text :location span}
          {:kind :parameter_ref :parameter-name text :location span}))

      :else
      {:kind :symbol_ref :symbol-text (w/node-text node) :location span})))

(defn- op-arg-keyword?
  "True when an OpArg is a keyword literal."
  [op-arg]
  (and op-arg
       (= :literal (:kind op-arg))
       (= :keyword (get-in op-arg [:literal :kind]))))

(defn- extract-op-call
  "Parse an op form such as (geo/buffer 100 ...) in a threading body into an
   OpCall map. Returns a Diagnostic when the head is not a symbol."
  [list-node]
  (let [head (first (w/named-children list-node))
        args (rest (w/named-children list-node))]
    (if (and head (w/sym? head))
      {:qualified-name (w/node-text head)
       :args (mapv extract-op-arg args)
       :location (w/node->span list-node)}
      (diag :def-malformed
            "operation form must start with a symbol like geo/buffer"
            (w/node->span list-node)))))

(def ^:private component-arg-index
  "The index of the component argument of each op that takes a component (a
   table op, named at the call site), by head spelling. A rung writes the
   head of a .cg module op fully qualified."
  {"map" 0 "cg.cache/checkpoint" 1})

(defn- enrich-map-component
  "When an OpCall takes a component (`map` or cg.cache/checkpoint), put the
   name of that arg on :component. The name is not resolved here:
   cg.parser.validate checks that it names a declared op. An arg that is not
   a name leaves the OpCall unchanged."
  [opcall]
  (if-let [idx (or (get component-arg-index (:qualified-name opcall))
                   (when (= "map" (util/unq-name (:qualified-name opcall))) 0))]
    (let [arg (nth (:args opcall) idx nil)
          cname (cond
                  (= :parameter_ref (:kind arg)) (:parameter-name arg)
                  (= :symbol_ref (:kind arg))    (:symbol-text arg)
                  :else nil)]
      (if cname (assoc opcall :component cname) opcall))
    opcall))

(defn- op-emits-level
  "The output level of the op, from its signature in the generated built-ins
   (keyed by source spelling). nil when the op is unknown or declares no
   :level."
  [qualified-name]
  (-> (get sigs/built-ins qualified-name) :outputs first :level))

(defn- enrich-op
  "Add to an OpCall :emits-level (from the signature of the op) and, for an
   op that takes a component, :component."
  [opcall]
  (let [lvl (op-emits-level (:qualified-name opcall))]
    (-> (cond-> opcall lvl (assoc :emits-level lvl))
        enrich-map-component)))

(def ^:private filter-by-column-heads
  "Both spellings of filter-by-column, because the parser does not resolve
   namespaces."
  #{"ds/filter-by-column" "filter-by-column"})

(defn- check-known-predicate-args
  "For a filter-by-column call, check that a keyword literal in the predicate
   slot is in cg.dsl.signatures/predicate-kinds. The slot is at position 1
   when the dataset comes from the thread (`dataset-implicit?`), else at
   position 2. Returns a vector of diagnostics."
  [head-text args dataset-implicit?]
  (if-not (contains? filter-by-column-heads head-text)
    []
    (let [pos (if dataset-implicit? 1 2)
          arg (get args pos)]
      (if (and arg
               (= :literal (:kind arg))
               (= :keyword (:kind (:literal arg))))
        (let [raw (:raw (:literal arg))
              kw-name (if (str/starts-with? raw ":") (subs raw 1) raw)]
          (if (contains? sigs/predicate-kinds kw-name)
            []
            [(diag :unknown-predicate
                   (str "filter-by-column predicate '" raw
                        "' is not in the predicate registry; see cg.dsl.signatures/predicate-kinds")
                   (:location arg))]))
        []))))

(defn- threading-form?
  "True when node is a list_lit whose head is item->/coll->/table->/layers->."
  [node]
  (w/list-head-in? node threading-macro-syms))

(defn- mode-keyword
  "The ThreadingMode keyword for a threading-macro name. nil for layers->,
   which the parser admits but nothing implements: the decoder then reports
   :threading-mode-unimplemented."
  [head-text]
  (case head-text
    "item->"  :item
    "coll->"  :coll
    "table->" :table
    nil))

(defn- decode-threading-form
  "Decode a (item->|coll->|table->|layers-> source ops... config?) form.
   Returns {:mode :input :source :ops :config :diagnostics}. :input is the
   name of a keyword source, else nil. :source is the source as an OpArg, or
   nil on a shape error."
  [body-node]
  (let [children (w/named-children body-node)
        head (first children)
        rest-children (rest children)
        mode-text (w/node-text head)
        mode (mode-keyword mode-text)
        first-arg (first rest-children)
        op-and-config (rest rest-children)
        last-arg (last op-and-config)
        has-trailing-config? (and last-arg (w/map-form? last-arg))
        op-forms (if has-trailing-config? (butlast op-and-config) op-and-config)
        diags (volatile! [])
        _ (when (nil? mode)
            (vswap! diags conj
                   (diag :threading-mode-unimplemented :warning
                         (str mode-text " is admitted by the threading vocabulary but "
                              "has no implementation yet; this flow "
                              "will render but not execute")
                         (w/node->span body-node))))
        input (cond
                (nil? first-arg)
                (do (vswap! diags conj
                           (diag :def-malformed
                                 (str mode-text " body needs a source as its first argument")
                                 (w/node->span body-node)))
                    nil)
                ;; A list form or a quoted form cannot be an OpArg.
                (or (w/list-form? first-arg) (w/quoting? first-arg))
                (do (vswap! diags conj
                           (diag :non-keyword-data-ref
                                 (str "first arg to " mode-text " cannot be promoted to an "
                                      "OpArg shape (got " (w/node-type first-arg) ")")
                                 (w/node->span first-arg)))
                    nil)
                (w/kwd? first-arg)
                (let [t (w/node-text first-arg)]
                  (if (str/starts-with? t ":")
                    (subs t 1)
                    t))
                ;; A source that is not a keyword: :source carries it.
                :else
                nil)
        ops (reduce (fn [acc op-node]
                      (cond
                        (w/quoting? op-node)
                        (do (vswap! diags conj
                                   (diag :quoted-op-form
                                         "quoted operation forms are not allowed in strict flow files"
                                         (w/node->span op-node)))
                            acc)

                        (w/list-form? op-node)
                        (let [r (extract-op-call op-node)]
                          (if (:code r)
                            (do (vswap! diags conj r) acc)
                            (do (doseq [d (check-known-predicate-args
                                           (:qualified-name r) (:args r) true)]
                                  (vswap! diags conj d))
                                (conj acc (enrich-op r)))))

                        :else
                        (do (vswap! diags conj
                                   (diag :def-malformed
                                         "threading-macro body element must be an op-form like (geo/buffer 100)"
                                         (w/node->span op-node)))
                            acc)))
                    []
                    op-forms)
        config (when has-trailing-config?
                 {:raw (w/node-text last-arg)
                  :location (w/node->span last-arg)})
        ;; :source stays nil for a list form or a quoted first arg. The
        ;; caller reads nil as a rejected form.
        source (when (and first-arg
                          (not (w/list-form? first-arg))
                          (not (w/quoting? first-arg)))
                 (extract-op-arg first-arg))]
    {:mode mode
     :input input
     :source source
     :ops ops
     :config config
     :diagnostics @diags}))

(defn- extract-model-body
  "Parse a threading-macro form into {:model :diagnostics}. The model is a
   Binding with `:body-shape :threading_macro`, or nil when the form has no
   valid source. `:is-template` is true when the source is a keyword
   literal."
  [name body-node]
  (let [{:keys [mode input source ops config diagnostics]}
        (decode-threading-form body-node)]
    {:model (when source
              {:name name
               :body-shape :threading_macro
               :mode mode
               :input input
               :source source
               :is-template (op-arg-keyword? source)
               :ops ops
               :config config
               :location (w/node->span body-node)})
     :diagnostics diagnostics}))

(defn- valid-binding-arg?
  "False only for a quoted form, because a quote has no runtime meaning in a
   flow body. Each other node can be an OpArg."
  [node]
  (not (w/quoting? node)))

(defn- extract-binding
  "Parse a (def <name> (<head> <arg>...)) form into {:kind :binding :value
   <Binding> :diagnostics [...]}, or {:kind :error ...} when an arg is a
   quoted form. `:location` is the span of the RHS, where a diagnostic about
   the call belongs. The caller adds `:form-location`, the span of the whole
   def.

   A keyword head (a projection of another binding) sets :is-accessor, and a
   head in `control-form-heads` sets :is-control. The type checker looks for
   a signature for neither."
  [name body-node]
  (let [children  (w/named-children body-node)
        head      (first children)
        arg-nodes (rest children)
        head-sym  (w/node-text head)
        accessor? (w/kwd? head)
        control?  (contains? control-form-heads head-sym)
        bad       (when-not control?
                    (first (remove valid-binding-arg? arg-nodes)))]
    (if bad
      {:kind :error
       :diagnostics [(diag :binding-malformed
                           (str "binding '" name "' has an argument that isn't a literal, "
                                "parameter ref, symbol ref, or inline map")
                           (w/node->span bad))]}
      (let [args (if control? [] (mapv extract-op-arg arg-nodes))
            head-sig (when-not (or accessor? control?)
                       (get sigs/built-ins head-sym))]
        {:kind :binding
         :value {:name name
                 :body-shape :function_call
                 :head-sym head-sym
                 :args args
                 :is-accessor accessor?
                 :is-control control?
                 :is-dataset-source (= :source (:kind head-sig))
                 :location (w/node->span body-node)}
         :diagnostics
         (cond
           control?
           [(diag :control-form-binding :warning
                  (str "binding '" name "' is computed by the control form ("
                       head-sym " ...); the dialect does not model its dataflow, "
                       "so the value is opaque to the canvas")
                  (w/node->span body-node))]

           accessor? []

           :else (check-known-predicate-args head-sym args false))}))))

(def ^:private let-body-sym "let")

(defn- pipeline-body-form?
  "True when node is a list_lit whose head is a threading-macro name."
  [node]
  (w/list-head-in? node threading-macro-syms))

(defn- let-body-form?
  "True when node is a list_lit whose head is plain `let`."
  [node]
  (w/list-head= node let-body-sym))

(defn- kwd->name
  "Strips the leading colon from a kwd_lit's source text."
  [kwd-node]
  (let [t (w/node-text kwd-node)]
    (if (str/starts-with? t ":") (subs t 1) t)))

(defn- map-pairs
  "The named children of a map_lit as [key value] pairs. A key with no value
   gets nil. This fn uses partition-all, not `(partition 2 2 nil coll)`:
   squint's 4-arity partition reads `.length` on the nil pad and throws on a
   short last group. The other pair walks in this namespace use partition-all
   for the same reason."
  [map-node]
  (partition-all 2 (w/named-children map-node)))

(defn- map-get-kwd
  "The value node for the keyword `target-text` (for example \":type\") in
   map-node, or nil."
  [map-node target-text]
  (some (fn [[k v]]
          (when (and k (w/kwd? k) (= target-text (w/node-text k))) v))
        (map-pairs map-node)))

(defn- str-lit-content
  "Returns a String literal's inner text (without the surrounding quotes)."
  [node]
  (let [t (w/node-text node)]
    (subs t 1 (dec (count t)))))

(defn- extract-type-spec
  "Read a :type value (a keyword or a string literal) from a port config
   map. Returns {:type <String> :diagnostics [...]}. The caller reports a
   missing :type."
  [op-name port-kind port-name type-node]
  (cond
    (w/kwd? type-node)
    {:type (kwd->name type-node) :diagnostics []}

    (w/str-lit? type-node)
    {:type (str-lit-content type-node) :diagnostics []}

    :else
    {:type nil
     :diagnostics
     [(diag :op-malformed
            (str "op '" op-name "' " port-kind " '" port-name
                 "' :type must be a keyword or string")
            (w/node->span type-node))]}))

(defn- extract-typed-output
  "Walks a per-output config map_lit and returns {:output <TypedOutput> :diagnostics [...]}."
  [op-name output-name cfg-node]
  (if-not (w/map-form? cfg-node)
    {:output {:name output-name :type nil
              :location (w/node->span cfg-node)}
     :diagnostics
     [(diag :op-malformed
            (str "op '" op-name "' output '" output-name
                 "' config must be a map literal")
            (w/node->span cfg-node))]}
    (let [type-node (map-get-kwd cfg-node ":type")
          {tstr :type tdiags :diagnostics}
          (if (nil? type-node)
            {:type nil
             :diagnostics
             [(diag :op-malformed
                    (str "op '" op-name "' output '" output-name
                         "' must declare a :type")
                    (w/node->span cfg-node))]}
            (extract-type-spec op-name "output" output-name type-node))]
      {:output {:name output-name
                :type tstr
                :location (w/node->span cfg-node)}
       :diagnostics tdiags})))

(defn- extract-port-decl
  "Parse the outer {:inputs {...}} or {:outputs {...}} declaration map.
   `port-key-text` is \":inputs\" or \":outputs\". `per-key-fn` reads one
   port from a (name, config map) pair. `sibling-keys` is the set of the
   other key texts that the map admits: each key outside it gives
   :op-malformed. Returns {:ports [...] :diagnostics [...]}."
  [op-name outer-node port-key-text sibling-keys per-key-fn]
  (if-not (w/map-form? outer-node)
    {:ports []
     :diagnostics
     [(diag :op-malformed
            (str "op '" op-name "' port declaration must be a map literal containing "
                 port-key-text)
            (w/node->span outer-node))]}
    (let [pairs (map-pairs outer-node)
          inner-node (map-get-kwd outer-node port-key-text)
          admitted (conj (set sibling-keys) port-key-text)
          extra-keys (remove (fn [[k _]]
                               (and k (w/kwd? k)
                                    (contains? admitted (w/node-text k))))
                             pairs)
          extra-diags (mapv (fn [[k _]]
                              (diag :op-malformed
                                    (str "op '" op-name "' " port-key-text
                                         " declaration only admits " port-key-text
                                         " (got " (w/node-text k) ")")
                                    (w/node->span k)))
                            extra-keys)]
      (cond
        (nil? inner-node)
        {:ports []
         :diagnostics
         (into [(diag :op-malformed
                      (str "op '" op-name "' port declaration must contain "
                           port-key-text)
                      (w/node->span outer-node))]
               extra-diags)}

        (not (w/map-form? inner-node))
        {:ports []
         :diagnostics
         (into [(diag :op-malformed
                      (str "op '" op-name "' " port-key-text
                           " value must be a map of name -> config")
                      (w/node->span inner-node))]
               extra-diags)}

        :else
        (let [collected
              (reduce (fn [acc [k v]]
                        (cond
                          (nil? k)
                          (update acc :diagnostics conj
                                  (diag :op-malformed
                                        (str "op '" op-name "' " port-key-text
                                             " entry is missing a value")
                                        (w/node->span inner-node)))

                          (not (w/kwd? k))
                          (update acc :diagnostics conj
                                  (diag :op-malformed
                                        (str "op '" op-name "' " port-key-text
                                             " names must be keywords")
                                        (w/node->span k)))

                          :else
                          (let [{:keys [input output diagnostics]}
                                (per-key-fn op-name (kwd->name k) v)]
                            (-> acc
                                (update :ports conj (or input output))
                                (update :diagnostics into diagnostics)))))
                      {:ports [] :diagnostics []}
                      (map-pairs inner-node))]
          (update collected :diagnostics into extra-diags))))))

(defn- extract-row-key-set
  "Decode a `:row-reads` or `:row-writes` value (a set literal of keywords)
   into {:keys [<name> ...] :diagnostics [...]}. A nil node gives nil :keys,
   to keep 'not declared' apart from 'declared empty'."
  [op-name key-text node]
  (cond
    (nil? node) {:keys nil :diagnostics []}

    (not (w/set-form? node))
    {:keys nil
     :diagnostics
     [(diag :op-malformed
            (str "op '" op-name "' " key-text
                 " must be a set literal of row keys, e.g. #{:geometry}")
            (w/node->span node))]}

    :else
    (let [children (w/named-children node)
          bad (remove w/kwd? children)]
      {:keys (mapv kwd->name (filter w/kwd? children))
       :diagnostics
       (mapv (fn [n]
               (diag :op-malformed
                     (str "op '" op-name "' " key-text
                          " entries must be keywords (got " (w/node-text n) ")")
                     (w/node->span n)))
             bad)})))

(declare extract-op-body-form)

(defn- extract-pipeline-body-form
  "Decode a threading-macro form in an op body into an OpBodyForm
   {:kind :pipeline ...}, with decode-threading-form."
  [node]
  (let [{:keys [mode input ops config diagnostics]} (decode-threading-form node)]
    {:body-form {:kind :pipeline
                 :mode mode
                 :input input
                 :ops ops
                 :config config
                 :raw (w/node-text node)
                 :location (w/node->span node)}
     :diagnostics diagnostics}))

(defn- pipeline-body->graph
  "Build a flow graph {:nodes :edges} from a :pipeline OpBodyForm, or nil for
   another kind. A node id is :<unq-name>-<counter> (from 1), the same id
   that cg.macros/generate-node-id gives. The edges chain the ops in order."
  [body-form]
  (when (= :pipeline (:kind body-form))
    (let [ops (:ops body-form)
          nodes (vec
                 (map-indexed
                  (fn [idx {qn :qualified-name args :args loc :location
                            lvl :emits-level comp :component}]
                    (cond-> {:type :operation
                             :id (util/as-kw (str (util/unq-name qn) "-" (inc idx)))
                             :fn qn
                             :args args
                             :location loc}
                      lvl  (assoc :emits-level lvl)
                      comp (assoc :component comp)))
                  ops))
          ids (mapv :id nodes)
          edges (vec
                 (map (fn [from to]
                        {:from from :to to :type :data-flow})
                      (butlast ids)
                      (rest ids)))]
      {:nodes nodes
       :edges edges
       :mode (:mode body-form)
       :input (:input body-form)})))

(defn- tail-form-label
  "The label of a let-tail form: its head symbol, `let` for a nested let, or
   `result` for a collection literal, which has no head."
  [form]
  (or (:head form)
      (case (:kind form)
        :let_body "let"
        "result")))

(defn- tail-node-worthy?
  "True when a tail produces the value: a call, a decoded shape or a
   collection literal. A bare symbol tail only passes the last binding
   through, and gets no node."
  [form]
  (or (some? (:head form))
      (not= :expr (:kind form))
      (boolean (re-find #"^[\{\[#]" (str/triml (str (:raw form)))))))

(defn- let-body->graph
  "Build a flow graph from a :let_body OpBodyForm, or nil for another kind.
   Each let binding is one node, and each tail form that tail-node-worthy?
   accepts is a node after the bindings. The edges chain the nodes in source
   order."
  [body-form]
  (when (= :let_body (:kind body-form))
    (let [bindings (:bindings body-form)
          ;; A name that binds again gets a unique id (foo, foo-2, foo-3),
          ;; because React Flow drops a node with a duplicate id.
          [seen-after-bindings nodes]
          (reduce
           (fn [[seen acc] {bname :name rhs-fn :rhs-fn loc :location
                            comp :component calls :rhs-calls}]
             (let [n (inc (get seen bname 0))
                   id-text (if (= 1 n) bname (str bname "-" n))
                   label (if rhs-fn (str bname " · " rhs-fn) bname)]
               [(assoc seen bname n)
                (conj acc (cond-> {:type :operation
                                   :id (util/as-kw id-text)
                                   :fn label
                                   :rhs-fn rhs-fn
                                   :bound-name bname
                                   :args []
                                   :location loc}
                            ;; The head of a run-component binding is always
                            ;; run-component: :component is the only name of
                            ;; the op that it calls.
                            comp  (assoc :component comp)
                            ;; The head of a branching RHS is `cond` or
                            ;; `when`: :rhs-calls names what its branches call.
                            calls (assoc :rhs-calls calls)))]))
           [{} []]
           bindings)
          ;; The tail nodes use the same `seen` map, to give a unique id to a
          ;; tail that has the label of a binding.
          [_seen2 tail-nodes]
          (reduce
           (fn [[seen acc] form]
             (let [label (tail-form-label form)
                   n (inc (get seen label 0))
                   id-text (if (= 1 n) label (str label "-" n))]
               [(assoc seen label n)
                (conj acc {:type :operation
                           :id (util/as-kw id-text)
                           :fn label
                           :is-tail true
                           :args []
                           :location (:location form)})]))
           [seen-after-bindings []]
           (filterv tail-node-worthy? (:tail body-form)))
          nodes (into nodes tail-nodes)
          ids (mapv :id nodes)
          edges (vec
                 (map (fn [from to]
                        {:from from :to to :type :data-flow})
                      (butlast ids)
                      (rest ids)))]
      {:nodes nodes
       :edges edges
       :mode nil
       :input nil})))

(defn- body-form->graph
  "The graph of a body form, or nil for a kind with no graph (:expr)."
  [body-form]
  (case (:kind body-form)
    :pipeline (pipeline-body->graph body-form)
    :let_body (let-body->graph body-form)
    nil))

(defn- let-binding-component
  "The component that the RHS of a let binding names, when the RHS is a
   `run-component` call. run-component takes its component as a string first
   argument, and a lookup by head finds nothing. The name is not resolved
   here. Both spellings of the head (`run/run-component` and bare) match,
   because the parser does not resolve namespaces."
  [rhs-fn val-node]
  (when (and rhs-fn (= "run-component" (util/unq-name rhs-fn)))
    (let [arg (second (w/named-children val-node))]
      (when (and arg (w/str-lit? arg))
        (str-lit-text arg)))))

(def ^:private branching-heads
  "Heads whose value comes from a form inside them, not from the head. A
   binding with one of these heads calls the fns that its branches call, and
   `:rhs-fn` names only the control form. `reduce` is here because the value
   of a fold comes from its lambda. The set is closed: each other head keeps
   the one callee that `:rhs-fn` gives."
  #{"when" "when-not" "when-let" "when-some" "when-first"
    "if" "if-not" "if-let" "if-some"
    "cond" "condp" "case"
    "or" "and" "do" "let"
    "reduce"})

(defn- rhs-call-heads
  "Each call head inside a branching RHS, in source order and distinct,
   without the branching heads. nil for an RHS that does not branch. The
   names are not resolved here: cg.parser.validate or the canvas gives them
   a meaning."
  [rhs-fn val-node]
  (when (contains? branching-heads rhs-fn)
    (let [heads (->> (w/descendants-of-type val-node "list_lit")
                     (keep w/first-sym-text)
                     (remove branching-heads)
                     distinct
                     vec)]
      (when (seq heads) heads))))

(defn- extract-let-body-binding
  "Read one [name rhs] pair of a `let`. `:rhs-fn` is the head symbol of an
   RHS that is a list form, else nil. A run-component call adds `:component`.
   Returns nil for a name that is not a symbol (destructuring), which the
   body graph does not model."
  [name-node val-node]
  (when (and name-node val-node (w/sym? name-node))
    (let [bname (w/sym-name name-node)
          rhs-fn (when (w/list-form? val-node)
                   (w/first-sym-text val-node))
          comp   (let-binding-component rhs-fn val-node)
          calls  (rhs-call-heads rhs-fn val-node)]
      (cond-> {:name      bname
               :rhs-fn    rhs-fn
               :rhs-raw   (w/node-text val-node)
               :location  (w/node->span name-node)}
        comp  (assoc :component comp)
        calls (assoc :rhs-calls calls)))))

(defn- extract-let-body
  "Decode a `(let [name expr ...] body...)` body of a defn into a :let_body
   OpBodyForm. Each binding is an entry in `:bindings`. Each body form after
   the bindings vector decodes through `extract-op-body-form` into `:tail`,
   and `:tail-raw` keeps the source text."
  ([op-name node mode]
   (let [children (w/named-children node)
         bindings-node (nth children 1 nil)
         body-nodes (drop 2 children)
         span (w/node->span node)]
     (cond
       (or (nil? bindings-node) (not (w/vec-form? bindings-node)))
       {:body-form nil
        :diagnostics
        [(diag :let-body-malformed
               (str "op '" op-name "' let body requires a [name expr ...] "
                    "bindings vector")
               span)]}

       (zero? (count body-nodes))
       {:body-form nil
        :diagnostics
        [(diag :let-body-malformed
               (str "op '" op-name "' let body requires a tail expression "
                    "after the bindings vector")
               span)]}

       :else
       (let [binding-children (w/named-children bindings-node)
             odd-count? (odd? (count binding-children))
             pairs (partition-all 2 binding-children)
             bindings (vec
                       (keep (fn [[name-node val-node]]
                               (extract-let-body-binding name-node val-node))
                             pairs))
             odd-diag (when odd-count?
                        [(diag :let-body-malformed
                               (str "op '" op-name
                                    "' let bindings vector must have an even number of forms "
                                    "(got " (count binding-children) ")")
                               (w/node->span bindings-node))])
             tail-results (mapv (fn [n] (extract-op-body-form op-name n mode)) body-nodes)]
         {:body-form {:kind :let_body
                      :bindings bindings
                      :tail (vec (keep :body-form tail-results))
                      :tail-raw (str/join "\n" (mapv w/node-text body-nodes))
                      :raw (w/node-text node)
                      :location span}
          :diagnostics (vec (concat (or odd-diag [])
                                    (mapcat :diagnostics tail-results)))})))))

;; An op body is ordinary Clojure, and it runs as written. The parser
;; models two shapes of it: a threading form, which builds the Model of a
;; table op, and a let, whose bindings are the steps of the body.

(defn- extract-op-body-form
  "Decode one body form of a defn into an OpBodyForm. A threading form in a
   defn with ^:op is a :pipeline, and a plain let is a :let_body. Each other
   shape is an :expr with its head symbol and raw text. `mode` is :defn for
   the body of a plain defn."
  [op-name node mode]
  (cond
    (and (not= :defn mode) (pipeline-body-form? node)) (extract-pipeline-body-form node)
    (let-body-form? node) (extract-let-body op-name node mode)
    :else {:body-form {:kind :expr
                       :head (when (w/list-form? node) (w/first-sym-text node))
                       :raw (w/node-text node)
                       :location (w/node->span node)}
           :diagnostics []}))

(defn- meta-values
  "The value of each ^... marker on `node`, in source order. A symbol can
   carry more than one marker (^:op ^:idempotent tag), and w/field gives
   only the first."
  [node]
  (into []
        (comp (filter #(= "meta_lit" (w/node-type %)))
              (mapcat w/named-children))
        (w/named-children node)))

(defn- extract-name-flags
  "The keyword metadata on the name symbol of a defn, as a vector of flag
   strings (for example [\"op\" \"idempotent\"]). This fn ignores other
   metadata shapes, such as ^{:flag-name true}."
  [name-node]
  (into [] (comp (filter w/kwd?) (map kwd->name)) (meta-values name-node)))

;; A defn as an operation, in each .cg file. A defn with ^:op on its name is
;; a flow op: the parser reads its op data from the name flags, the attr-map
;; and the param markers, as cg.run/var-op-decl reads them from the var. A
;; plain defn is an operation too: a call to it resolves and the canvas
;; can open its body, but a rung cannot name it (:rung-not-op).

(defn- defn-arity
  "One arity of a defn: its fixed params, its rest param (after &) or
   nil, and its body nodes."
  [params-node body-nodes]
  (let [ps   (w/named-children params-node)
        amp? (fn [p] (= "&" (w/node-text p)))]
    {:fixed (vec (take-while (fn [p] (not (amp? p))) ps))
     :rest  (second (drop-while (fn [p] (not (amp? p))) ps))
     :body  (vec body-nodes)}))

(defn- defn-arities
  "The arities of a defn, from the nodes after its name, docstring and
   attr-map: one [params] body, or one (params body) list per arity."
  [nodes]
  (if (w/vec-form? (first nodes))
    [(defn-arity (first nodes) (rest nodes))]
    (vec (keep (fn [n]
                 (when (w/list-form? n)
                   (let [cs (w/named-children n)]
                     (when (w/vec-form? (first cs))
                       (defn-arity (first cs) (rest cs))))))
               nodes))))

(defn- arity-width [a]
  (+ (count (:fixed a)) (if (:rest a) 1 0)))

(def ^:private param-roles
  "The param flags that give an op param its role."
  {"row" :row "table" :table "reducer" :reducer})

(defn- key-name
  "The name of a key in an option map: a symbol as written, a keyword
   without its colon."
  [k]
  (if (w/kwd? k) (kwd->name k) (w/node-text k)))

(defn- option-map
  "The keys of an option map, `& {:keys! [...] :keys [...] :or {...}}`: the
   required keys and the optional keys in source order, and the source text
   of each default. nil when the rest param is not a map.
   cg.run/var-op-decl gives the same data from the var."
  [p]
  (when (w/map-form? p)
    (let [key-names (fn [k]
                      (let [v (map-get-kwd p k)]
                        (if (and v (w/vec-form? v))
                          (mapv key-name (w/named-children v))
                          [])))
          or-node (map-get-kwd p ":or")]
      {:required (key-names ":keys!")
       :optional (key-names ":keys")
       :defaults (if (and or-node (w/map-form? or-node))
                   (into {}
                         (keep (fn [[k v]] (when (and k v) [(key-name k) (w/node-text v)])))
                         (map-pairs or-node))
                   {})})))

(defn- defn-input
  "The input of the param `p` at index `i`. A role flag (^:row) gives its
   role, and a ^{:type ...} map gives its type, which is any without one.
   A destructured param is named by its position. A rest param that is a
   map is an option map, and its keys go under :options."
  [p i optional? rest?]
  (let [ms (meta-values p)
        type-node (some (fn [m] (when (w/map-form? m) (map-get-kwd m ":type"))) ms)
        options (when rest? (option-map p))]
    (cond-> {:name (if (w/sym? p) (w/sym-name p) (str "arg" i))
             :type (if (and type-node (w/kwd? type-node)) (kwd->name type-node) "any")
             :role (some (fn [m] (when (w/kwd? m) (get param-roles (kwd->name m)))) ms)
             :optional optional?}
      options (assoc :options options))))

(defn- defn-inputs
  "The params of the widest arity. A param past the shortest arity is
   optional. The rest param is optional too, except an option map with a
   required key when each arity takes it: a caller must give that map."
  [arities]
  (let [widest    (reduce (fn [a b] (if (> (arity-width b) (arity-width a)) b a)) arities)
        min-fixed (reduce min (map (fn [a] (count (:fixed a))) arities))]
    (vec (concat
          (map-indexed (fn [i p] (defn-input p i (>= i min-fixed) false))
                       (:fixed widest))
          (when-let [r (:rest widest)]
            (let [in (defn-input r (count (:fixed widest)) true true)]
              [(cond-> in
                 (and (seq (get-in in [:options :required])) (every? :rest arities))
                 (assoc :optional false))]))))))

(defn- returns-model?
  "True when a decoded body form ends in a threading form."
  [body-form]
  (case (:kind body-form)
    :pipeline true
    :let_body (boolean (some-> (:tail body-form) peek returns-model?))
    false))

(defn- extract-op-attrs
  "The outputs and the row contract of an op, from its attr-map. With no
   :outputs, the op has one output, typed any."
  [op-name attr-node]
  (let [attr-keys (into [] (comp (map first) (filter #(and % (w/kwd? %))) (map w/node-text))
                        (when attr-node (map-pairs attr-node)))
        outs (if (and attr-node (map-get-kwd attr-node ":outputs"))
               (extract-port-decl op-name attr-node ":outputs" attr-keys
                                  (fn [op-n out-n cfg] (extract-typed-output op-n out-n cfg)))
               {:ports [{:name "result" :type "any"}] :diagnostics []})
        reads (extract-row-key-set op-name ":row-reads"
                                   (some-> attr-node (map-get-kwd ":row-reads")))
        writes (extract-row-key-set op-name ":row-writes"
                                    (some-> attr-node (map-get-kwd ":row-writes")))]
    {:outputs (:ports outs)
     :row-reads (:keys reads)
     :row-writes (:keys writes)
     :diagnostics (vec (concat (:diagnostics outs) (:diagnostics reads) (:diagnostics writes)))}))

(defn- op-rule-diagnostics
  "The rules of an op on its params and its body, as cg.run checks them
   at registration: one role param at most, as the first param, and a
   row op returns a value."
  [op-name inputs body span]
  (let [roled (keep-indexed (fn [i in] (when (:role in) i)) inputs)]
    (cond-> []
      (next roled)
      (conj (diag :op-malformed (str "op '" op-name "' has more than one role param") span))

      (and (seq roled) (not= 0 (first roled)))
      (conj (diag :op-malformed (str "op '" op-name "': a role param must be the first param") span))

      (and (= :row (:role (first inputs))) (some-> (peek body) returns-model?))
      (conj (diag :row-op-returns-model
                  (str "op '" op-name "' takes a ^:row param and returns a model. "
                       "A row op must return a value. Use a ^:table op to return a model.")
                  span)))))

(defn- extract-defn
  "Walks a (defn NAME doc? attr-map? [params] body...) form, or its
   multi-arity shape. A defn with ^:op reads its attr-map, and a threading
   form in its body decodes as a pipeline. A plain defn has one output,
   typed any, and its body decodes in :defn mode."
  [list-node]
  (let [children  (w/named-children list-node)
        head      (w/first-sym-text list-node)
        name-node (nth children 1 nil)
        after     (drop 2 children)
        doc-node  (when (w/str-lit? (first after)) (first after))
        after     (if doc-node (rest after) after)
        attr-node (when (w/map-form? (first after)) (first after))
        after     (if attr-node (rest after) after)
        arities   (defn-arities after)
        span      (w/node->span list-node)]
    (if (or (nil? name-node) (not (w/sym? name-node)) (empty? arities))
      {:kind :error
       :diagnostics [(diag :defn-malformed
                           (str "(" head " <name> [params] body...) needs a name and a params vector")
                           span)]}
      (let [op-name (w/sym-name name-node)
            name-flags (extract-name-flags name-node)
            op? (boolean (some #(= "op" %) name-flags))
            widest  (reduce (fn [a b] (if (> (arity-width b) (arity-width a)) b a)) arities)
            results (mapv (fn [bn] (extract-op-body-form op-name bn (when-not op? :defn)))
                          (:body widest))
            body    (vec (keep :body-form results))
            inputs  (defn-inputs arities)
            attrs   (if op?
                      (extract-op-attrs op-name attr-node)
                      {:outputs [{:name "result" :type "any"}] :diagnostics []})]
        {:kind :operation
         :value {:name op-name
                 :inputs inputs
                 :outputs (:outputs attrs)
                 :row-reads (:row-reads attrs)
                 :row-writes (:row-writes attrs)
                 :flags (cond-> (into ["defn"] (remove #(= "defn" %)) name-flags)
                          (some :rest arities) (conj "variadic")
                          (= "defn-" head)     (conj "private"))
                 :doc (when doc-node (str-lit-text doc-node))
                 :body body
                 :body-graph (some body-form->graph body)
                 :location span}
         :diagnostics (vec (concat (mapcat :diagnostics results)
                                   (:diagnostics attrs)
                                   (when op? (op-rule-diagnostics op-name inputs body span))))}))))

(defn- extract-deftemplate
  "Parse a (deftemplate NAME (threading-> :input ops...)) form into a
   threading-macro Binding."
  [list-node]
  (let [children (w/named-children list-node)
        name-node (nth children 1 nil)
        body-node (nth children 2 nil)
        span (w/node->span list-node)]
    (cond
      (or (not name-node) (not (w/sym? name-node)))
      {:kind :error
       :diagnostics [(diag :def-malformed
                           "(deftemplate <name> <body>) requires a symbol name"
                           span)]}

      (or (nil? body-node) (not (w/list-form? body-node))
          (not (contains? threading-macro-syms (w/first-sym-text body-node))))
      {:kind :error
       :diagnostics [(diag :def-malformed
                           (str "deftemplate '" (w/sym-name name-node)
                                "' body must be a threading-macro form "
                                "(item->|coll->|table->|layers->)")
                           span)]}

      :else
      (let [{:keys [model diagnostics]}
            (extract-model-body (w/sym-name name-node) body-node)]
        {:kind :binding
         ;; some-> not assoc: a shape error leaves model nil, and assoc on
         ;; nil would hand the accumulator a truthy map to conj.
         :value (some-> model (assoc :form-location span))
         :diagnostics diagnostics}))))

(defn- extract-def
  "Classify a (def <name> <body>) form by its RHS: a parameter (a fully
   literal RHS), a threading-macro binding, a collection binding (a
   collection literal with a member that is not a literal), a call binding,
   an alias binding (a symbol), or an error (:unclassified-top-level-form).
   Returns {:kind ... :value ... :diagnostics ...}."
  [list-node]
  (let [children (w/named-children list-node)
        name-node (second children)
        ;; node-text would keep the metadata of the name (^:private x).
        nm (when (and name-node (w/sym? name-node)) (w/sym-name name-node))
        body-node (nth children 2 nil)
        span (w/node->span list-node)]
    (cond
      (or (not name-node) (not (w/sym? name-node)))
      {:kind :error
       :diagnostics [(diag :def-malformed
                           "(def <name> <body>) requires a symbol name"
                           span)]}

      (nil? body-node)
      {:kind :error
       :diagnostics [(diag :def-malformed
                           "(def <name> <body>) requires a body"
                           span)]}

      (threading-form? body-node)
      (let [{:keys [model diagnostics]}
            (extract-model-body nm body-node)]
        {:kind :binding
         :value (some-> model (assoc :form-location span))
         :diagnostics diagnostics})

      (literal-deep? body-node)
      (let [fields (literal-fields body-node)]
        {:kind :parameter
         :value (cond-> {:name nm
                         :value (literal-value body-node)
                         :config nil
                         :location span}
                  fields (assoc :fields fields))
         :diagnostics []})

      (literal-kind body-node)
      ;; A collection literal with a member that is not a literal, such as
      ;; (def opts {:thresholds other-binding}). It is a Binding, not a
      ;; Parameter: it computes a value from other bindings, and nothing draws
      ;; a Parameter as a node. :head-sym is the delimiters of the collection,
      ;; and :args carries the contents to keep the references reachable.
      (let [arg (extract-op-arg body-node)]
        {:kind :binding
         :value {:name nm
                 :body-shape :function_call
                 :head-sym (case (literal-kind body-node)
                             :mapv "{}"
                             :vec  "[]"
                             :set  "#{}"
                             "()")
                 :args (or (:items arg)
                           (some->> (:entries arg) (mapcat identity) (filterv some?))
                           [])
                 :is-collection true
                 :is-accessor false
                 :is-control false
                 :is-dataset-source false
                 :location span
                 :form-location span}
         :diagnostics []})

      (and (w/list-form? body-node)
           (let [h (first (w/named-children body-node))]
             (or (w/sym? h) (w/kwd? h))))
      (let [r (extract-binding nm body-node)]
        (cond-> r (:value r) (update :value assoc :form-location span)))

      (w/sym? body-node)
      ;; An alias: (def output-dir data-dir). :head-sym is the aliased name,
      ;; and the arg gives the edge.
      {:kind :binding
       :value {:name nm
               :body-shape :function_call
               :head-sym (w/node-text body-node)
               :args [(extract-op-arg body-node)]
               :is-alias true
               :is-accessor false
               :is-control false
               :is-collection false
               :is-dataset-source false
               :location span
               :form-location span}
       :diagnostics []}

      :else
      {:kind :error
       :diagnostics [(diag :unclassified-top-level-form
                           (str "top-level (def " (w/node-text name-node)
                                " ...) RHS is neither a literal parameter, "
                                "a collection, an alias, a threading-macro "
                                "model, nor a binding call")
                           span)]})))

(defn- cgproj-meta?
  "True when map-node carries ^:cgproj metadata."
  [map-node]
  (when-let [m (w/field map-node "meta")]
    (boolean
     (some (fn [c] (and (w/kwd? c) (= ":cgproj" (w/node-text c))))
           (w/named-children m)))))

(defn- extract-overlay
  "Returns {:overlay ... :diagnostics ...} for a ^:cgproj map_lit. The
   overlay holds only the raw text and the span: the consumer (flow-graph)
   decodes the node positions, and the parser does not check the keys."
  [map-node]
  {:overlay {:raw (w/node-text map-node)
             :location (w/node->span map-node)}
   :diagnostics []})

(defn- forbidden-form-diagnostic
  "A :forbidden-form Diagnostic when the head of list-node is in
   `forbidden-def-heads`, else nil."
  [list-node]
  (let [head (w/first-sym-text list-node)]
    (when-let [label (get forbidden-def-heads head)]
      (diag :forbidden-form
            (str label " (" head ") are not allowed in flow files")
            (w/node->span list-node)))))

(def ^:private block-constructor-heads
  "Control forms that a .cg script can use at the top level. The parser
   models each one as a block: a known outer shape around a decoded body.
   `:binder` says what the second child of the form is, and `:body-from`
   gives the child index where the body starts. `control-form-heads` is a
   different set: the heads of the RHS of a def."
  {"doseq"  {:kind :doseq  :binder :bindings :body-from 2}
   "let"    {:kind :let    :binder :bindings :body-from 2}
   "if-let" {:kind :if-let :binder :bindings :body-from 2}
   "when"   {:kind :when   :binder :test     :body-from 2}
   "try"    {:kind :try    :binder :none     :body-from 1}})

(declare extract-block)

(defn- block-form?
  [node]
  (w/list-head-in? node block-constructor-heads))

(defn- clause-form?
  [head node]
  (w/list-head= node head))

(defn- head-text
  "The head of a list form as source text, symbol or keyword.
   `w/first-sym-text` gives nil for a keyword head, such as
   `(:census-blocks analysis)`."
  [node]
  (when (w/list-form? node)
    (let [h (first (w/named-children node))]
      (when (and h (or (w/sym? h) (w/kwd? h)))
        (w/node-text h)))))

(defn- block-binding
  "`:name` holds the source text of the name form, not a symbol name. Most
   doseq forms destructure, and the block must keep a record of what it
   iterates."
  [name-node val-node]
  (when (and name-node val-node)
    {:name (w/node-text name-node)
     :destructured? (not (w/sym? name-node))
     :rhs-fn (head-text val-node)
     :rhs-raw (w/node-text val-node)
     :location (w/node->span name-node)}))

(defn- decode-block-bindings
  "The `[name expr ...]` vector of a doseq, let or if-let. `:bindings` is
   nil when the vector is absent. The caller treats that as a hard shape
   error."
  [head bindings-node span]
  (if (or (nil? bindings-node) (not (w/vec-form? bindings-node)))
    {:bindings nil
     :diagnostics [(diag :block-malformed
                         (str "(" head " [name expr ...] body...) requires a "
                              "bindings vector")
                         span)]}
    (let [kids (w/named-children bindings-node)]
      {:bindings (vec (keep (fn [[n v]] (block-binding n v))
                            (partition-all 2 kids)))
       :diagnostics (if (odd? (count kids))
                      [(diag :block-malformed :warning
                             (str "(" head " ...) bindings vector must have an "
                                  "even number of forms (got " (count kids) ")")
                             (w/node->span bindings-node))]
                      [])})))

(defn- decode-block-test
  "A `when` test, held as raw source. Whether the test is true is a
   runtime question, and the canvas does not answer it."
  [head test-node span]
  (if (nil? test-node)
    {:test nil
     :diagnostics [(diag :block-malformed
                         (str "(" head " <test> body...) requires a test expression")
                         span)]}
    {:test {:raw (w/node-text test-node)
            :head (head-text test-node)
            :location (w/node->span test-node)}
     :diagnostics []}))

(defn- expr-leaf
  "A body form that the top-level vocabulary does not reach: an opaque region
   inside a recognized block, with the label, raw text and span of an opaque
   card. It reports no diagnostic, because the source is valid Clojure and
   runs."
  [node]
  (let [text (w/node-text node)]
    {:kind :expr
     :head (head-text node)
     :label (truncate-label (first-line text) 60)
     :raw text
     :location (w/node->span node)}))

(defn- decode-block-body-form
  "One body form, decoded against the top-level vocabulary. A nested
   block constructor is a block. A println is an output. Every other form
   is an `:expr` leaf."
  [node]
  (cond
    (block-form? node)
    (let [{:keys [kind value diagnostics]} (extract-block node)]
      {:body-form (if (= :block kind)
                    {:kind :block :block value :location (w/node->span node)}
                    (expr-leaf node))
       :diagnostics diagnostics})

    (clause-form? "println" node)
    {:body-form {:kind :output
                 :output (extract-output node)
                 :location (w/node->span node)}
     :diagnostics []}

    :else
    {:body-form (expr-leaf node) :diagnostics []}))

(defn- decode-block-body
  [body-nodes]
  (reduce (fn [acc n]
            (let [{:keys [body-form diagnostics]} (decode-block-body-form n)]
              (-> acc
                  (update :body conj body-form)
                  (update :diagnostics into diagnostics))))
          {:body [] :diagnostics []}
          body-nodes))

(defn- extract-catch-clause
  [node]
  (let [kids (w/named-children node)
        {:keys [body diagnostics]} (decode-block-body (drop 3 kids))]
    {:clause {:class (some-> (nth kids 1 nil) w/node-text)
              :binding (some-> (nth kids 2 nil) w/node-text)
              :body body
              :raw (w/node-text node)
              :location (w/node->span node)}
     :diagnostics diagnostics}))

(defn- extract-finally-clause
  [node]
  (let [{:keys [body diagnostics]} (decode-block-body
                                    (drop 1 (w/named-children node)))]
    {:clause {:body body
              :raw (w/node-text node)
              :location (w/node->span node)}
     :diagnostics diagnostics}))

(defn- extract-try-clauses
  "The shape decode-block-body returns, plus `:catches` and `:finally`.
   The clause forms do not stay in the guarded body."
  [body-nodes]
  (let [catch? (fn [n] (clause-form? "catch" n))
        finally? (fn [n] (clause-form? "finally" n))
        guarded (decode-block-body
                 (remove (fn [n] (or (catch? n) (finally? n))) body-nodes))
        catches (mapv extract-catch-clause (filter catch? body-nodes))
        fin (some-> (first (filter finally? body-nodes)) extract-finally-clause)]
    {:body (:body guarded)
     :catches (mapv :clause catches)
     :finally (:clause fin)
     :diagnostics (-> (:diagnostics guarded)
                      (into (mapcat :diagnostics catches))
                      (into (:diagnostics fin)))}))

(defn- extract-block
  "A top-level control form as a BlockDecl. The parser models the block, not
   its contents: a body form that the vocabulary does not reach decodes to an
   `:expr` leaf."
  [node]
  (let [head (w/first-sym-text node)
        {:keys [kind binder body-from]} (get block-constructor-heads head)
        kids (w/named-children node)
        span (w/node->span node)
        text (w/node-text node)
        binder-node (nth kids 1 nil)
        {:keys [bindings test] :as binder-res}
        (case binder
          :bindings (decode-block-bindings head binder-node span)
          :test     (decode-block-test head binder-node span)
          :none     {:diagnostics []})
        body-nodes (drop body-from kids)
        decoded (if (= :try kind)
                  (extract-try-clauses body-nodes)
                  (decode-block-body body-nodes))]
    (if (or (and (= :bindings binder) (nil? bindings))
            (and (= :test binder) (nil? test)))
      {:kind :error :diagnostics (:diagnostics binder-res)}
      {:kind :block
       :value (cond-> {:kind kind
                       :head head
                       :body (:body decoded)
                       :label (truncate-label (first-line text) 60)
                       :raw text
                       :location span}
                (= :bindings binder) (assoc :bindings bindings)
                (= :test binder)     (assoc :test test)
                (= :try kind)        (assoc :catches (:catches decoded)
                                            :finally (:finally decoded)))
       :diagnostics (into (vec (:diagnostics binder-res))
                          (:diagnostics decoded))})))

(defn- extract-require-form
  "extract-require in the walker shape {:kind :value :diagnostics}, for the
   `top-level-heads` table."
  [node]
  (let [r (extract-require node)]
    (if (:code r)
      {:kind :error :diagnostics [r]}
      {:kind :require :value r :diagnostics []})))

(defn- extract-output-form
  [node]
  {:kind :output :value (extract-output node) :diagnostics []})

(defn- extract-top-level-pipeline
  "A bare top-level threading-macro form. It is legal only when its last op
   is a sink op: otherwise nothing names or consumes its value."
  [node]
  (let [{:keys [mode source ops config diagnostics]} (decode-threading-form node)
        terminal-name (some-> ops last :qualified-name)
        terminal-sig (when terminal-name (get sigs/built-ins terminal-name))
        terminal-kind (:kind terminal-sig)]
    (cond
      ;; A shape error in the form comes first: a sink verdict must not hide
      ;; it.
      (seq diagnostics)
      {:kind :error :diagnostics diagnostics}

      (= :sink terminal-kind)
      {:kind :sink
       :value {:mode mode
               :source source
               :ops ops
               :config config
               :location (w/node->span node)}
       :diagnostics []}

      :else
      {:kind :error
       :diagnostics [(diag :unsinked-top-level-pipeline
                           (str "bare top-level " (w/first-sym-text node)
                                " pipeline must terminate in a @sink-flagged op"
                                (when terminal-name
                                  (str " (got " terminal-name ", kind "
                                       (or terminal-kind "unknown") ")")))
                           (w/node->span node))]})))

(def ^:private top-level-heads
  "The top-level vocabulary of a .cg file as data: head symbol -> the walker
   that decodes a form with that head. Each walker returns {:kind :value
   :diagnostics}, and its :kind is the classification kind."
  (merge
   {"require"         extract-require-form
    ;; A .cg file gives an alias to a loaded module with this form, after its
    ;; run/load-module.
    "clojure.core/require" extract-require-form
    "run/load-module" extract-load-module
    "ns"              extract-module-ns
    "def"             extract-def
    "deftemplate"     extract-deftemplate
    "println"         extract-output-form}
   (into {} (map (fn [s] [s extract-top-level-pipeline]) threading-macro-syms))
   (into {} (map (fn [s] [s extract-block]) (keys block-constructor-heads)))))

(defn- classify-unaccepted-form
  "The verdict for a list form whose head is not in the vocabulary:
   :forbidden-form when the head is a def-shape a flow file rules out,
   otherwise :unclassified-top-level-form."
  [node]
  (if-let [forbidden (forbidden-form-diagnostic node)]
    {:kind :error
     :diagnostics [forbidden]}
    {:kind :error
     :diagnostics [(diag :unclassified-top-level-form
                         (str "top-level form (" (or (w/first-sym-text node) "?")
                              " ...) is not allowed in a flow file")
                         (w/node->span node))]}))

(defn- classify-top-level-map
  "A top-level map_lit is legal only as the ^:cgproj overlay."
  [node]
  (if (cgproj-meta? node)
    (let [{:keys [overlay diagnostics]} (extract-overlay node)]
      {:kind :overlay :value overlay :diagnostics diagnostics})
    ;; Nothing decodes a ^:inputs declaration: a ^:inputs map also gets this
    ;; error.
    {:kind :error
     :diagnostics [(diag :unclassified-top-level-form
                         "top-level map literal is only legal as ^:cgproj overlay or ^:inputs declaration"
                         (w/node->span node))]}))

(def ^:private kind->collection
  "Classification kind -> the CgFlow collection that takes its `:value`.
   `:overlay` and `:module-ns` hold one value each and have their own
   first-one-wins rules. This table does not hold them."
  {:require     :requires
   :load-module :loaded-modules
   :parameter   :parameters
   :binding     :bindings
   :operation   :operations
   :sink        :sinks
   :output      :outputs
   :block       :blocks})

(defn- classify-top-level
  "Classifies a single top-level form into {:kind ... :value ...
   :diagnostics [...]}. The kinds are the keys of `kind->collection`
   plus :overlay, :module-ns, :comment and :error; :value is absent for
   the last three."
  [node]
  (cond
    (w/comment? node)
    {:kind :comment :diagnostics []}

    (w/list-form? node)
    (let [walk (get top-level-heads (w/first-sym-text node))]
      (cond
        walk                                            (walk node)
        (contains? #{"defn" "defn-"} (w/first-sym-text node)) (extract-defn node)
        (= "declare" (w/first-sym-text node))           {:kind :comment :diagnostics []}
        :else                                           (classify-unaccepted-form node)))

    (w/map-form? node)
    (classify-top-level-map node)

    ;; A module ends with a bare symbol, its load value.
    (w/sym? node)
    {:kind :comment :diagnostics []}

    :else
    {:kind :error
     :diagnostics [(diag :unclassified-top-level-form
                         (str "top-level " (w/node-type node)
                              " is not allowed in a flow file")
                         (w/node->span node))]}))

(defn- check-legacy-macro-namespace
  "Emits a :legacy-macro-namespace diagnostic per require that pulls
   item->/coll->/table-> from cg.dsl or cg.dsl.macros."
  [requires]
  (->> requires
       (filter (fn [r]
                 (and (contains? legacy-macro-namespaces (:namespace r))
                      (some threading-macro-syms (:referred r)))))
       (mapv (fn [r]
               (diag :legacy-macro-namespace :warning
                     (str "threading macros must be required from cg.macros, not "
                          (:namespace r))
                     (:location r))))))

(defn function-call-bindings
  "The `:bindings` of `cg-flow` whose body shape is `:function_call`."
  [cg-flow]
  (filterv #(= :function_call (:body-shape %)) (:bindings cg-flow)))

(defn threading-macro-bindings
  "The `:bindings` of `cg-flow` whose body shape is `:threading_macro`."
  [cg-flow]
  (filterv #(= :threading_macro (:body-shape %)) (:bindings cg-flow)))

(defn extract
  "Walk a tree-sitter Tree and return a CgFlow with its shape diagnostics:

     {:requires       [<NamespaceImport> ...]
      :loaded-modules [<LoadModule> ...]
      :inputs         [<InputDecl> ...]
      :parameters     [<Parameter> ...]
      :bindings       [<Binding> ...]
      :operations     [<OperationDecl> ...]
      :sinks          [<SinkPipeline> ...]
      :outputs        [<OutputDecl> ...]
      :blocks         [<BlockDecl> ...]
      :overlay        <OverlaySource> | nil
      :module-ns      <ModuleNs> | nil
      :diagnostics    [<Diagnostic> ...]}

   With `:opaque-blocks true` in `opts`, a rejected form also gives an entry
   in `:opaque-blocks`. The parser does not follow a `run/load-module` path:
   the caller parses that file and merges its :operations. cg.parser.validate
   holds the rules across forms (unique names, references)."
  ([tree] (extract tree nil))
  ([tree opts]
   (let [opaque? (boolean (:opaque-blocks opts))
        initial {:requires []
                 :loaded-modules []
                 :inputs []
                 :parameters []
                 :bindings []
                 :operations []
                 :sinks []
                 :outputs []
                 :blocks []
                 :overlay nil
                 :module-ns nil
                 :opaque-blocks []
                 :diagnostics []}
        opaque-codes #{:forbidden-form :unclassified-top-level-form
                       :block-malformed}
        flow (reduce
              (fn [acc n]
                (let [{:keys [kind value] :as classified}
                      (classify-top-level n)
                      acc (update acc :diagnostics into (:diagnostics classified))
                      acc (if (and opaque?
                                   (= :error kind)
                                   (some (fn [d] (contains? opaque-codes (:code d)))
                                         (:diagnostics classified)))
                            (update acc :opaque-blocks conj (opaque-block n))
                            acc)]
                  (cond
                    (contains? kind->collection kind)
                    (if (some? value)
                      (update acc (get kind->collection kind) conj value)
                      acc)

                    (= :overlay kind)
                    (if (:overlay acc)
                      (update acc :diagnostics conj
                              (diag :single-overlay
                                    "at most one ^:cgproj overlay per file; ignoring later block"
                                    (:location value)))
                      (assoc acc :overlay value))

                    ;; The first (ns ...) wins, because run/load-module reads
                    ;; the first ns form of the file.
                    (= :module-ns kind)
                    (if (:module-ns acc) acc (assoc acc :module-ns value))

                    :else acc)))
              initial
              (w/top-level-forms tree))]
    (update flow :diagnostics into
            (check-legacy-macro-namespace (:requires flow))))))
