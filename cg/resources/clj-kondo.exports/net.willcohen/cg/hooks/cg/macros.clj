(ns hooks.cg.macros
  "clj-kondo hooks for cg's macro layer: the pseudo-threading macros
   (item->, coll->, table->, and layers->, which the parser admits and
   cg.macros does not define).

   The threading-macro bodies are DSL forms, not function calls.
   `(geo/buffer 100)` inside `(item-> :parcels ...)` is parsed by
   cg.macros/parse-operation-form into an operation map with :fn
   cg.geo/buffer. geo/buffer is never invoked as
   `(geo/buffer threaded 100)`.

   Rewrite each op-form so that:
   - argument analysis survives (misspelled literals, unresolved symbols
     inside args still surface);
   - a qualified op-head (geo/buffer, ds/max-by) stays as a var
     reference, which marks its namespace used and surfaces an unknown
     var, without arity rules;
   - a BARE op-head is dropped. Bare heads resolve against the runtime
     op registry (cg.run/resolve-operation, register-bare-op!), an open
     vocabulary static analysis cannot know; the parser's
     :unresolved-op pass (cg.parser.types pass 1) owns that check with
     the registry and module-following in hand. Keeping the head would
     make every registry-resolved bare op read as an unresolved symbol.

   `when-row` holds a predicate map and op-forms. The rewrite drops its
   head, as for a bare op-head, keeps the predicate map, and rewrites each
   body form as an op-form."
  (:require [clj-kondo.hooks-api :as api]))

(declare rewrite-op-form)

(defn- node-head [node]
  (when (api/list-node? node)
    (let [h (first (:children node))]
      (when (api/token-node? h)
        (let [s (api/sexpr h)]
          (when (symbol? s) s))))))

(def ^:private when-row-heads '#{when-row cg.run/when-row run/when-row})

(defn- rewrite-when-row-op-form
  "(when-row pred-map op-form...) in op-form position ->
   (do pred-map rewritten-op-forms...). The body forms are themselves
   op-forms (the threading value is implicit), so they recurse."
  [node]
  (let [[_head & body] (:children node)]
    (with-meta
      (api/list-node
       (list* (api/token-node 'do) (map rewrite-op-form body)))
      (meta node))))

(defn- rewrite-op-form [node]
  (let [head (node-head node)]
    (cond
      (nil? head) node
      (contains? when-row-heads head) (rewrite-when-row-op-form node)
      :else
      (let [children (:children node)
            kept (if (namespace head) children (rest children))
            do-form (api/list-node
                     (list* (api/token-node 'do) kept))]
        (with-meta do-form (meta node))))))

(defn threading-macro [{:keys [node]}]
  (let [[_macro-sym data-ref & ops] (:children node)
        rewritten-ops (mapv rewrite-op-form ops)
        new-children (concat [(api/token-node 'do)]
                             (when data-ref [data-ref])
                             rewritten-ops)
        new-node (with-meta (api/list-node new-children) (meta node))]
    {:node new-node}))
