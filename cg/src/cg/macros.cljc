;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; item->, coll-> and table-> build a model of the computation as data. cg
;; compiles the model for the platform and then runs it.

(ns cg.macros
  (:require [cg.model :as model]
            [cg.feature :as feature]
            [cg.dsl.signatures :as sigs]
            [cg.util :as util]
            #?(:cljs [clojure.string :as str])))

#?(:clj (set! *warn-on-reflection* true))

#?(:cljs
   (defn- decode-kw
     "The name string of a compiler Keyword object, or x itself."
     [x]
     (if (and (some? x) (not (string? x)) (not (number? x)) (string? (.-name x)))
       (.-name x)
       x)))

#?(:cljs
   (defn- compiler-map->pairs
     "A vector of [k v] pairs from a cljs.core map that the squint compiler
      gives a macro."
     [m]
     (mapv (fn [e] [(.-key e) (.-val e)]) (js/Array.from m))))

;; Alias symbol -> full namespace symbol, for example geo -> cg.geo.
(defonce ^:private namespace-aliases
  (atom {}))

(defn register-alias!
  "Register `alias-sym` as an alias of the namespace `full-ns-sym`. The
  require of .cg code calls this."
  [alias-sym full-ns-sym]
  (swap! namespace-aliases assoc alias-sym full-ns-sym))

(defn resolve-alias
  "The full namespace for an alias, or `ns-sym` itself when it is not an
  alias. Registered aliases come first, then the aliases of *ns* (JVM only)."
  [ns-sym]
  (or
   (get @namespace-aliases ns-sym)
   #?(:clj
      (let [current-ns *ns*]
        (when (instance? clojure.lang.Namespace current-ns)
          (when-let [aliased-ns (get (ns-aliases current-ns) ns-sym)]
            (ns-name aliased-ns))))
      :cljs nil)
   ns-sym))

(defn generate-node-id
  "The node id `<operation-name>-<counter>` as a keyword, for example
  :buffer-1."
  [operation-name counter]
  (util/as-kw (str (util/unq-name operation-name) "-" counter)))

(defn map-positional-to-named
  "Map positional args to the names in `param-schema`. With no schema, the
  result is {} for no args, {:value x} for one arg and {:args [...]} for more."
  [args param-schema]
  (if (and param-schema (seq param-schema))
    (into {}
          (map (fn [arg schema-entry]
                 [(:name schema-entry) arg])
               args
               param-schema))
    (cond
      (empty? args) {}
      (= 1 (count args)) {:value (first args)}
      :else {:args (vec args)})))

(defn- keyword-form?
  "True for a keyword in a rung. On :cljs the squint compiler hands a
   cljs.core Keyword object, which prints with its colon."
  [x]
  #?(:clj (keyword? x)
     :cljs (and (some? x) (not (string? x)) (not (number? x))
                (.startsWith (str x) ":"))))

(defn- with-option-map
  "The args of a rung whose op ends in an option map: the first
   `n-positional` args, then the option map (one arg as it is, or keyword args
   as a map), or no map when no such arg exists."
  [op-name args n-positional]
  (let [positional (vec (take n-positional args))
        more (vec (drop n-positional args))]
    (cond
      (empty? more) positional
      (= 1 (count more)) (conj positional (first more))

      (and (even? (count more)) (every? keyword-form? (map first (partition 2 more))))
      (conj positional
            (into {} (map (fn [[k v]] #?(:clj [k v] :cljs [(decode-kw k) (decode-kw v)])))
                  (partition 2 more)))

      :else
      (throw (ex-info (str "op '" op-name "': after its " n-positional
                           " positional args, a rung gives its option map"
                           " as one value or as keyword args")
                      {:op (str op-name) :args (mapv str more)})))))

(declare parse-operation-form parse-regular-operation-form)

(def ^:private when-row-heads
  "The heads that parse-when-row-form reads: `when`, `when-row`, and `filter`
   with a predicate map at position 1."
  #?(:clj #{'when-row 'when 'filter}
     :cljs #{"when-row" "when" "filter"}))

(defn- option-key?
  "True when k is a keyword whose name is in `sigs/option-keys`."
  [k]
  #?(:clj (and (keyword? k) (contains? sigs/option-keys (name k)))
     ;; k is a compiler Keyword object here, not a string. Decode it first.
     :cljs (contains? sigs/option-keys (decode-kw k))))

(defn- parse-when-row-form
  "Parse `(when-row {pred+opts} body)` into the operation shape of
   `build-operation-node`. Throws ex-info on a malformed form."
  [form]
  (let [args (rest form)
        pred-map (first args)
        body-form (second args)
        head (first form)]
    ;; squint's map? does not know the cljs.core map that the compiler gives
    ;; the macro. On :cljs, accept each value that is not a scalar.
    (when-not #?(:clj (map? pred-map)
                 :cljs (and (some? pred-map)
                            (not (string? pred-map))
                            (not (number? pred-map))))
      (throw (ex-info (str "(" head " ...) requires a map literal at position 1; got " (pr-str pred-map))
                      {:form form})))
    (when (nil? body-form)
      (throw (ex-info (str "(" head " ...) requires a body form at position 2")
                      {:form form})))
    (when (> (count args) 2)
      (throw (ex-info (str "(" head " ...) takes exactly two arguments (a predicate map and a single body form); got "
                           (count args))
                      {:form form})))
    (let [entries #?(:clj (seq pred-map)
                     :cljs (compiler-map->pairs pred-map))
          {opt-entries true pred-entries false}
          (group-by (fn [[k _v]] (option-key? k)) entries)
          pred-entries (vec pred-entries)
          opt-entries (vec opt-entries)]
      (when (zero? (count pred-entries))
        (throw (ex-info (str "(" head " ...) predicate map must include exactly one predicate entry "
                             "(got 0); option-only maps are not a valid form")
                        {:form form :options opt-entries})))
      (when (> (count pred-entries) 1)
        (throw (ex-info (str "(" head " ...) predicate map must include exactly one predicate entry "
                             "(got " (count pred-entries) "); multi-key composition is deferred")
                        {:form form :predicates pred-entries})))
      (let [[pred-k pred-v] (first pred-entries)
            predicate {:kind (util/unq-name pred-k)
                       :arg #?(:clj pred-v :cljs (decode-kw pred-v))}
            opts-decoded (into {}
                               (map (fn [[k v]]
                                      #?(:clj [(name k) (if (keyword? v) (name v) v)]
                                         :cljs [(util/unq-name k) (decode-kw v)]))
                                    opt-entries))
            opts-map (merge sigs/option-defaults opts-decoded)
            parsed-body (parse-operation-form body-form)
            body-shape {:fn (str (:fn parsed-body))
                        :params (:params parsed-body)}]
        ;; The params are a vector around the map, because apply-params must
        ;; pass the map whole to (when-row dataset args).
        ;; On :cljs :fn is a string: a quoted symbol compiles to
        ;; cljs.core.symbol(...), and squint exports no `symbol`. The SCI path
        ;; on :clj needs the symbol.
        {:fn #?(:clj 'cg.run/when-row :cljs "cg.run/when-row")
         :params [{:predicate predicate
                   :options opts-map
                   :body body-shape}]}))))

(defn parse-operation-form
  "Parse an operation form into {:fn ... :params ...}, with positional arg
  names from the operation registry. A form that is not a list has no args."
  [form]
  ;; squint's list? does not know the cljs.core lists that the compiler gives
  ;; a macro. On :cljs, accept each value that is not a scalar.
  (if #?(:clj (list? form)
         :cljs (and (some? form) (not (string? form)) (not (number? form))))
    (let [op-fn (first form)
          first-arg (second form)
          ;; On :cljs the head is a compiler Symbol object, not a string. The
          ;; full ns/name string keeps `ds/filter` apart from bare `filter`.
          head #?(:clj op-fn :cljs (str op-fn))]
      (if (and #?(:clj (symbol? op-fn) :cljs (string? head))
               (contains? when-row-heads head)
               ;; squint's map? is false for the compiler's cljs.core map.
               ;; Because `when-row` and `when` have no other meaning as a
               ;; rung, the head routes them. Only `filter` still needs map?.
               #?(:clj (map? first-arg)
                  :cljs (or (contains? #{"when-row" "when"} head)
                            (map? first-arg))))
        (parse-when-row-form form)
        (parse-regular-operation-form form)))
    {:fn (util/as-sym (str form)) :params {}}))

(defn- parse-regular-operation-form
  "`parse-operation-form` for a list that is not a when-row. When the arg count
   equals the schema param count, a last map literal is a positional arg."
  [form]
  (let [op-fn (first form)
        args (rest form)
        fn-sym #?(:clj (if (symbol? op-fn)
                         (if-let [ns (namespace op-fn)]
                           (let [resolved-ns (resolve-alias (symbol ns))]
                             (symbol (name resolved-ns) (name op-fn)))
                           op-fn)
                         (symbol (str op-fn)))
                  :cljs (let [s (str op-fn)
                              slash (.indexOf s "/")]
                          (if (neg? slash)
                            s
                            (let [ns-part (subs s 0 slash)
                                  name-part (subs s (inc slash))
                                  resolved-ns (resolve-alias ns-part)]
                              (str resolved-ns "/" name-part)))))
        fn-sym-str (str fn-sym)
        bare? #?(:clj (neg? (.indexOf fn-sym-str "/"))
                 :cljs (neg? (.indexOf fn-sym-str "/")))
        op-info (or (feature/get-operation-info fn-sym)
                    (feature/get-group-info fn-sym)
                    (when bare?
                      (or (feature/get-operation-info (str "cg." fn-sym-str))
                          (feature/get-group-info (str "cg." fn-sym-str))
                          (when-let [ns-name (feature/get-bare-op-namespace fn-sym-str)]
                            (or (feature/get-operation-info (str ns-name "/" fn-sym-str))
                                (feature/get-group-info (str ns-name "/" fn-sym-str)))))))
        ;; The rung's args fill the other params, because the threading driver
        ;; supplies the :role :table input.
        param-schema (some->> (:params op-info)
                              (remove #(= :table (:role %)))
                              vec)
        param-count (when param-schema (count param-schema))
        [positional opts] (cond
                            (:option-map (last param-schema))
                            [(with-option-map fn-sym args (dec param-count)) {}]

                            (and param-count
                                 (pos? (count args))
                                 (= (count args) param-count))
                            [args {}]

                            (and (pos? (count args))
                                 (map? (last args))
                                 (> (count args) 1))
                            [(butlast args) (last args)]

                            :else
                            [args {}])
        named (map-positional-to-named
               ;; On :cljs a keyword arg is a compiler Keyword object, and
               ;; (str kw) gives "[object Object]" at runtime. decode-kw reads
               ;; only the top-level args: a keyword in a vector or a map
               ;; stays raw.
               #?(:clj positional
                  :cljs (mapv decode-kw positional))
               param-schema)
        ;; A :type :component arg names a table op that run-component runs.
        ;; It stays a name, because run-component takes a name and is the one
        ;; path that passes :timeout-ms to the component's run. JVM only: the
        ;; JS runtime lifts each symbol arg to a runtime value.
        component-key (some #(when (= :component (:type %)) (:name %)) param-schema)
        component #?(:clj (let [v (get named component-key)]
                            (when (symbol? v) (str v)))
                     :cljs nil)]
    (cond-> {:fn fn-sym
             :params (merge (cond-> named
                              component (assoc component-key component))
                            opts)}
      component (assoc :component component))))

(defn extract-config-overrides
  "Split `params` into {:config ... :params ...}. The config keys are
  :parallelism, :timeout-ms, :buffer-size and :batch-size."
  [params]
  ;; when-row params are a vector, and dissoc on a vector throws on the JVM.
  ;; A vector carries no config overrides.
  (if (map? params)
    (let [config-keys #{:parallelism :timeout-ms :buffer-size :batch-size}
          config (select-keys params config-keys)
          op-params (apply dissoc params config-keys)]
      {:config config
       :params op-params})
    {:config {} :params params}))

(defn build-source-node
  "Build a source node for a data reference."
  [data-ref]
  {:type :source
   :id (util/as-kw (str (util/unq-name data-ref) "-src"))
   ;; On :cljs run-model matches data-ref against string binding keys. On
   ;; :clj the SCI run-model path expects the keyword.
   :data-ref #?(:clj data-ref :cljs (util/unq-name data-ref))
   :schema {}})

(defn- op-signature-level
  "The declared output level of an op, or nil when the op declares none.
   Resolves a bare name as parse-regular-operation-form does."
  [fn-sym]
  (let [s (str fn-sym)
        bare? (neg? (.indexOf s "/"))
        info (or (feature/get-operation-info fn-sym)
                 (when bare?
                   (or (feature/get-operation-info (str "cg." s))
                       (when-let [ns-name (feature/get-bare-op-namespace s)]
                         (feature/get-operation-info (str ns-name "/" s))))))]
    (-> info :outputs first :level)))

(defn- annotate-operand-levels
  "Set :operand-level, the level that flows in (from :dataset), on each op
   node. A node with :emits-level changes the level for the nodes after it."
  [op-nodes]
  (loop [nodes op-nodes
         current :dataset
         acc []]
    (if (empty? nodes)
      acc
      (let [n (first nodes)
            n' (assoc n :operand-level current)
            next-level (or (:emits-level n) current)]
        (recur (rest nodes) next-level (conj acc n'))))))

(defn build-operation-node
  "Build an operation node from a parsed operation. A registered group gives a
   :type :group node with its expansion."
  [operation mode node-id]
  (let [{:keys [config params]} (extract-config-overrides (:params operation))
        ;; :fn is a string, to prevent symbol resolution in the macro expansion.
        fn-ref (:fn operation)
        fn-sym #?(:clj (if (symbol? fn-ref) fn-ref (symbol fn-ref))
                  :cljs (str fn-ref))
        fn-str (str fn-sym)
        group-info (feature/get-group-info fn-sym)]
    (if group-info
      (let [expansion-fn (:expansion-fn group-info)
            expansion (expansion-fn params)]
        {:type :group
         :id node-id
         :group-type (util/as-kw (util/unq-name fn-sym))
         :fn fn-str
         :params params
         :mode mode
         :config config
         :expansion expansion})
      ;; The node keys match the nodes of cg.parser.extract. :component is
      ;; not a runtime arg, and cg.dataset/into-map takes only the container,
      ;; which is why both leave :params.
      (let [unq (util/unq-name fn-str)
            lvl (op-signature-level fn-sym)
            map-op? (= "map" unq)
            into-op? (= "into" unq)
            params' (if (or map-op? into-op?) (dissoc params :value) params)
            base {:type :operation
                  :id node-id
                  :fn fn-str
                  :params params'
                  :mode mode
                  :config config}]
        (cond-> base
          lvl     (assoc :emits-level lvl)
          map-op? (assoc :component (str (:value params)))
          (:component operation) (assoc :component (:component operation)))))))

(defn build-sink-node
  "Build a sink node for result output."
  [output-binding]
  {:type :sink
   :id :result
   :output-binding output-binding})

(defn build-edges
  "Build edges connecting node IDs in sequence."
  [node-ids]
  (mapv (fn [from to]
          {:from from :to to :type :data-flow})
        (butlast node-ids)
        (rest node-ids)))

(defn detect-platform
  "Detect current platform for model configuration."
  []
  #?(:clj :jvm
     :cljs :javascript))

#?(:cljs
   (defn- model->create-call
     "The return value of a threading macro on :cljs: an object whose toString
      gives the call cg$model.get_staged_model(N). `args-js` is the JS array of
      the rung args that lift-rung-args took out of the model."
     ([model-map] (model->create-call model-map nil))
     ([model-map args-js]
      (let [id (model/stage-model! model-map)
            call (if args-js
                   (str "cg$model.get_staged_model(" id ", " args-js ")")
                   (str "cg$model.get_staged_model(" id ")"))]
        (js-obj "toString" (fn [] call))))))

(defn build-threading-model
  "Build the model map for item->, coll-> or table-> (`mode` :item, :coll or
   :table). `annotate?` runs annotate-operand-levels; a last map of several
   `operations` is the config."
  [mode annotate? data-ref operations]
  (let [[ops config] (if (and (map? (last operations))
                              (> (count operations) 1)
                              (not (list? (last operations))))
                       [(butlast operations) (last operations)]
                       [operations {}])
        parsed-ops (mapv parse-operation-form ops)
        op-node-ids (mapv #(generate-node-id (:fn %1) (inc %2))
                          parsed-ops (range))
        source-node (build-source-node data-ref)
        base-op-nodes (mapv #(build-operation-node %1 mode %2)
                            parsed-ops op-node-ids)
        op-nodes (if annotate? (annotate-operand-levels base-op-nodes) base-op-nodes)
        sink-node (build-sink-node :result)
        all-node-ids (concat [(:id source-node)] op-node-ids [(:id sink-node)])
        edges (build-edges all-node-ids)]
    {:nodes (vec (concat [source-node] op-nodes [sink-node]))
     :edges edges
     :config (merge {:platform (detect-platform)} config)}))

;; Because squint calls a `:macros` entry as (f &form &env & args), these fns
;; take two leading params. squint (from 0.14.200) emits no runtime binding
;; for a `defmacro`.
#?(:cljs
   (do
     (defn item->* [_form _env data-ref & operations]
       (model->create-call (build-threading-model :item false data-ref operations)))
     (defn coll->* [_form _env data-ref & operations]
       (model->create-call (build-threading-model :coll false data-ref operations)))
     (defn table->* [_form _env data-ref & operations]
       (model->create-call (build-threading-model :table true data-ref operations)))))

;; A printer of compiler forms as source text, for the JS runtime. A :macros
;; entry cannot build new forms from the compiler's cljs.core forms.
;; threading* prints each rung argument that it lifts, and compiles
;; the text. The first character of a printed collection gives its kind,
;; because squint's list?, vector? and map? do not know cljs.core collections.
#?(:cljs
   (do
     (defn- form-kind [x]
       (cond
         (nil? x) "nil"
         (string? x) "string"
         (or (number? x) (boolean? x)) "scalar"
         (instance? js/RegExp x) "regex"
         :else (let [p (str x)]
                 (cond
                   (.startsWith p "(") "list"
                   (.startsWith p "[") "vector"
                   (.startsWith p "{") "map"
                   (.startsWith p "#{") "set"
                   (.startsWith p ":") "keyword"
                   :else "symbol"))))

     (defn- form-items [x]
       (vec (js/Array.from x)))

     (declare emit-form)

     (defn- emit-forms [xs ctx]
       (str/join " " (map (fn [x] (emit-form x ctx)) xs)))

     (defn- symbol-head [items]
       (let [head (first items)]
         (when (= "symbol" (form-kind head))
           (str head))))

     (defn- emit-form
       "Print a compiler form as source text, with a local of the macro call
        (ctx :locals) as a token for arg-values-js."
       [x ctx]
       (case (form-kind x)
         "nil" "nil"
         "string" (js/JSON.stringify x)
         "scalar" (str x)
         "regex" (str "#\"" (.-source x) "\"")
         "keyword" (str x)
         "list" (str "(" (emit-forms (form-items x) ctx) ")")
         "vector" (str "[" (emit-forms (form-items x) ctx) "]")
         "map" (str "{"
                    (str/join " " (map (fn [[k v]]
                                         (str (emit-form k ctx) " " (emit-form v ctx)))
                                       (compiler-map->pairs x)))
                    "}")
         "set" (str "#{" (emit-forms (form-items x) ctx) "}")
         (if-let [ident (get (:locals ctx) (str x))]
           (let [token (str "cg$local$" (count @(:tokens ctx)) "$")]
             (swap! (:tokens ctx) assoc token ident)
             token)
           (str x))))))

;; A rung argument that is not a literal must evaluate where the threading
;; macro is. A :macros entry cannot emit the compiler's forms into the model,
;; and it uses a placeholder string that get_staged_model fills from one JS
;; array compiled in the scope of the macro call.
#?(:cljs
   (do
     (defn- literal-arg?
       "True for a rung argument that is its own value in the model: nil, a
        string, number, boolean, keyword or regex. A collection is not a literal."
       [x]
       (contains? #{"nil" "string" "scalar" "keyword" "regex"} (form-kind x)))

     (defn- keeps-its-args?
       "True for a rung whose arguments are not values: a when-row form,
        and filter with a predicate map (parse-when-row-form reads both)."
       [h items]
       (or (contains? #{"when-row" "cg.macros/when-row" "when"} h)
           (and (= "filter" h) (= "map" (form-kind (nth items 1 nil))))))

     (defn- lift-rung-args
       "Replace each non-literal rung argument with the placeholder cg$arg$<i>.
        Returns [operations forms], with the lifted forms in placeholder order."
       [operations]
       (let [forms #js []
             ops (mapv (fn [op]
                         (let [items (when (= "list" (form-kind op)) (form-items op))
                               h (symbol-head items)
                               args (rest items)]
                           (if (or (nil? h)
                                   (keeps-its-args? h items)
                                   (every? literal-arg? args))
                             op
                             (into [(first items)]
                                   (map (fn [a]
                                          (if (literal-arg? a)
                                            a
                                            (do (.push forms a)
                                                (str "cg$arg$" (dec (.-length forms)))))))
                                   args))))
                       operations)]
         [ops (vec forms)]))

     (defn- env-locals
       "{name -> JS identifier} for the locals at a macro call, from :var->ident
        in &env."
       [env]
       (let [v->i (when env
                    (some (fn [[k v]] (when (= "var->ident" (decode-kw k)) v))
                          (compiler-map->pairs env)))]
         (if v->i
           (into {} (map (fn [[sym ident]] [(str sym) (util/munge-name (str ident))]))
                 (compiler-map->pairs v->i))
           {})))

     (defn- arg-values-js
       "The JS of an array of the values of `forms`, in the scope of the macro
        call, with each local token replaced by the local's identifier."
       [forms env compile-expr]
       (let [locals (env-locals env)
             tokens (atom {})
             ctx {:locals locals :tokens tokens}
             js (compile-expr (str "[" (emit-forms forms ctx) "]"))]
         (reduce (fn [acc [token ident]] (.replaceAll acc token ident)) js @tokens)))

     (defn threading*
       "The :macros entry for item->, coll-> or table-> (mode :item, :coll or
        :table) on the JS runtime. `compile-expr` compiles source text to JS."
       [mode compile-expr]
       (fn [_form env data-ref & operations]
         (let [[ops forms] (lift-rung-args operations)]
           (model->create-call (build-threading-model mode (= mode :table) data-ref ops)
                               (when (seq forms) (arg-values-js forms env compile-expr))))))))

;; The JVM expansion syntax-quotes model/create-model, which resolves to
;; cg.model/create-model. An unqualified symbol fails, because the macro
;; output runs in the user namespace. The CLJS expansion emits no symbols
;; (see model->create-call).

(defmacro item->
  "Process each item independently. Returns a model of the computation. A
  last map is the config.

    (item-> buildings (buffer 100) (simplify 0.1) {:parallelism 8})"
  [data-ref & operations]
  (let [model-map (build-threading-model :item false data-ref operations)]
    #?(:clj `(model/create-model ~model-map)
       :cljs (model->create-call model-map))))

(defmacro coll->
  "Process the collection as a whole. Returns a model of the computation. A
  last map is the config.

    (coll-> buildings (union-all))"
  [data-ref & operations]
  (let [model-map (build-threading-model :coll false data-ref operations)]
    #?(:clj `(model/create-model ~model-map)
       :cljs (model->create-call model-map))))

(defmacro table->
  "Process a dataset with table operations. Returns a model of the
  computation. A last map is the config.

    (table-> buildings (filter :height #(> % 50)) (spatial-join parcels))"
  [data-ref & operations]
  (let [model-map (build-threading-model :table true data-ref operations)]
    #?(:clj `(model/create-model ~model-map)
       :cljs (model->create-call model-map))))
