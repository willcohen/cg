(ns hooks.cg.macros
  "clj-kondo hooks for cg's macro layer: the pseudo-threading macros
   (item->, coll->, table->, layers->).

   The threading-macro bodies are DSL forms, not function calls.
   `(geo/buffer 100)` inside `(item-> :parcels ...)` is parsed by
   cg.macros/parse-operation-form into {:fn 'geo/buffer :params
   {:distance 100}} — geo/buffer is never invoked as
   `(geo/buffer threaded 100)`.

   Rewrite each op-form so that:
   - argument analysis survives (typo'd literals, unresolved symbols
     inside args still surface);
   - a qualified op-head (geo/buffer, ds/max-by) stays as a var
     reference, which marks its namespace used and surfaces a genuinely
     unknown var, without arity rules;
   - a BARE op-head is dropped. Bare heads resolve against the runtime
     op registry (cg.run/resolve-operation, register-bare-op!), an open
     vocabulary static analysis cannot know; the parser's
     :unresolved-op pass (cg.parser.types pass 1) owns that check with
     the registry and module-following in hand. Keeping the head would
     make every registry-resolved bare op read as an unresolved symbol.

   `when-row` is DSL vocabulary, not a var: cg.macros recognises it by
   symbol at macroexpand time, and the SCI runtime never interns it.
   Keeping its head in the rewrite would read as an unresolved symbol, so
   the rewrite drops it and keeps the predicate map plus the body."
  (:require [clj-kondo.hooks-api :as api]))

(declare rewrite-op-form)

(defn- node-head
  "Return the head of a list node as a symbol, or nil."
  [node]
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

(defn- rewrite-op-form
  "Rewrite one op-form for analysis: qualified head -> (do head args...),
   bare head -> (do args...), when-row -> its own rewrite."
  [node]
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

(defn threading-macro
  "Hook for item->, coll->, table->, layers->."
  [{:keys [node]}]
  (let [[_macro-sym data-ref & ops] (:children node)
        rewritten-ops (mapv rewrite-op-form ops)
        new-children (concat [(api/token-node 'do)]
                             (when data-ref [data-ref])
                             rewritten-ops)
        new-node (with-meta (api/list-node new-children) (meta node))]
    {:node new-node}))
