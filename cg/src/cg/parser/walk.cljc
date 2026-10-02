;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.walk
  "Tree-sitter Node helpers for a GraalVM Value (JVM) or a JS object (squint).
   They assume tree-sitter-clojure node types. Spans are 1-indexed. Must stay
   free of eval: do not require cg.dsl, cg.macros, cg.run or cg.codegen."
  (:refer-clojure :exclude [descendants])
  #?(:clj (:import [org.graalvm.polyglot Value])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defn- value-get [^Value v ^String member]
     (when (and v (.hasMember v member))
       (.getMember v member))))

#?(:clj
   (defn- value-invoke [^Value v ^String method & args]
     (.invokeMember v method (object-array args))))

#?(:clj
   (defn- value->long [^Value v]
     (when v (.asLong v))))

#?(:clj
   (defn- value->str [^Value v]
     (when v (.asString v))))

#?(:clj
   (defn- value->seq
     "Coerce a tree-sitter children array (a polyglot Value) to a Clojure
     seq of Node Values."
     [^Value arr]
     (when (and arr (.hasArrayElements arr))
       (let [n (.getArraySize arr)]
         (loop [i 0 acc (transient [])]
           (if (< i n)
             (recur (inc i) (conj! acc (.getArrayElement arr i)))
             (persistent! acc)))))))

(defn node-type
  "Returns the tree-sitter node type as a string (for example \"list_lit\")."
  [node]
  #?(:clj  (value->str (value-get node "type"))
     :cljs (.-type node)))

(defn node-text
  "Returns the source text of this node, with its whitespace and its
   delimiters (quotes, parens)."
  [node]
  #?(:clj  (value->str (value-get node "text"))
     :cljs (.-text node)))

(defn named-children-raw
  "Returns a vector of the named children of this node, comments included.
   Almost every caller wants `named-children`."
  [node]
  #?(:clj  (or (value->seq (value-get node "namedChildren")) [])
     :cljs (vec (.-namedChildren ^js node))))

(defn named-children
  "Returns the named children of this node without comments.
   tree-sitter-clojure counts a `;;` comment as a named child, and a comment
   inside a form would move each later child by one."
  [node]
  (vec (remove (fn [c] (= "comment" (node-type c)))
               (named-children-raw node))))

(defn child-at
  "Returns the i-th named child of this node, or nil if out of range."
  [node i]
  (let [cs (named-children node)]
    (when (< i (count cs))
      (nth cs i))))

(defn field
  "Returns the named field of this node (for example `meta` on a map_lit
   with metadata), or nil."
  [node field-name]
  #?(:clj  (let [^Value v (value-invoke node "childForFieldName" field-name)]
             (when (and v (not (.isNull v))) v))
     :cljs (.childForFieldName ^js node field-name)))

#?(:clj
   (defn- pos->row-col-1
     "GraalVM Value with shape {row, column} (0-indexed) -> [line col]
     (1-indexed)."
     [^Value pos]
     (when pos
       [(inc (value->long (value-get pos "row")))
        (inc (value->long (value-get pos "column")))])))

#?(:cljs
   (defn- pos->row-col-1 [pos]
     (when pos
       [(inc (.-row ^js pos)) (inc (.-column ^js pos))])))

(defn node->span
  "Returns the source span of this node as a map with 1-indexed line and
   column fields."
  [node]
  (let [start #?(:clj (value-get node "startPosition")
                 :cljs (.-startPosition ^js node))
        end   #?(:clj (value-get node "endPosition")
                 :cljs (.-endPosition ^js node))
        [sl sc] (pos->row-col-1 start)
        [el ec] (pos->row-col-1 end)]
    {:start-line sl :start-column sc :end-line el :end-column ec}))

(defn type=
  "Returns true when node's type matches t."
  [node t]
  (= (node-type node) t))

(defn list-form?
  "Returns true when node is a list_lit (any (...) form)."
  [node]
  (type= node "list_lit"))

(defn map-form?
  "Returns true when node is a map_lit ({...} form)."
  [node]
  (type= node "map_lit"))

(defn vec-form?
  "Returns true when node is a vec_lit ([...] form)."
  [node]
  (type= node "vec_lit"))

(defn set-form?
  "Returns true when node is a set_lit (#{...} form)."
  [node]
  (type= node "set_lit"))

(defn nil-lit?
  "True when node is a nil_lit. The grammar gives nil, true and false their
   own types, not sym_lit."
  [node]
  (type= node "nil_lit"))

(defn sym?
  "Returns true when node is a sym_lit."
  [node]
  (type= node "sym_lit"))

(defn kwd?
  "Returns true when node is a kwd_lit."
  [node]
  (type= node "kwd_lit"))

(defn num?
  "Returns true when node is a num_lit."
  [node]
  (type= node "num_lit"))

(defn str-lit?
  "Returns true when node is a str_lit."
  [node]
  (type= node "str_lit"))

(defn bool-lit?
  "Returns true when node is a bool_lit (true / false)."
  [node]
  (type= node "bool_lit"))

(defn quoting?
  "Returns true when node is a quoting_lit ('<form>)."
  [node]
  (type= node "quoting_lit"))

(defn comment?
  "Returns true when node is a comment line."
  [node]
  (type= node "comment"))

(defn sym-name
  "The name of a sym_lit after any namespace slash, or nil when node is not a
   sym_lit."
  [node]
  (when (sym? node)
    (or (some-> (first (filter #(type= % "sym_name") (named-children node)))
                node-text)
        (node-text node))))

(defn first-sym-text
  "Returns the text of the first named child if it is a sym_lit, else nil:
   the head of a list_lit form, for example \"def\"."
  [node]
  (let [c0 (child-at node 0)]
    (when (and c0 (sym? c0))
      (node-text c0))))

(defn list-head=
  "Returns true when node is a list_lit whose first named child is a
   sym_lit with text matching head."
  [node head]
  (and (list-form? node)
       (= (first-sym-text node) head)))

(defn list-head-in?
  "True when node is a list_lit whose head sym_lit text is in heads (a set of
   strings, or a map with string keys)."
  [node heads]
  (and (list-form? node)
       (contains? heads (first-sym-text node))))

(defn root
  "Returns the rootNode of a tree-sitter Tree, the value backends
   produce from cg.parser.core/parse."
  [tree]
  #?(:clj  (value-get tree "rootNode")
     :cljs (.-rootNode ^js tree)))

(defn top-level-forms
  "Returns a vector of the named children of the source root, in source
   order, comments included."
  [tree]
  (named-children-raw (root tree)))

(defn descendants
  "Lazily yields every named-descendant of node (depth-first, pre-order),
   excluding node itself."
  [node]
  (lazy-seq
   (mapcat (fn [c] (cons c (descendants c)))
           (named-children node))))

(defn descendants-of-type
  "Returns a seq of every descendant node whose tree-sitter type
   matches t, in pre-order."
  [node t]
  (filter #(type= % t) (descendants node)))
