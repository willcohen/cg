;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dsl.signatures
  "Operation type signatures for the cg.parser.types checker: the one source
   of the typed surface of the DSL, as pure data. :level on an output is
   independent of :type: a `group-by` output has :type :dataset and :level
   :groups. An unknown type string becomes :any.

   This namespace must stay free of eval: it must not require cg.dsl,
   cg.macros, cg.run or cg.codegen (cg.dsl.signatures-generated is pure
   data)."
  (:require [cg.dsl.signatures-generated :as gen]))

#?(:clj (set! *warn-on-reflection* true))

(def level-kinds
  "The closed set of data levels of an op output: the unit that a value
   travels as along a pipeline. group-by changes :dataset to :groups, and
   into changes :groups to :map."
  #{:feature :collection :dataset :groups :map})

(def ^:private type-str->kw
  "A static string -> keyword lookup for the type names of an OperationDecl.
   It replaces a runtime (keyword <string>), which squint-cljs/core does not
   have."
  {"feature" :feature
   "dataset" :dataset
   "number"  :number
   "string"  :string
   "boolean" :boolean
   "bool"    :boolean
   "keyword" :keyword
   "any"     :any})

(defn type-kind
  "The keyword for a type-name string. An unknown string gives :any: the
   parser already reports :op-malformed for a malformed :type."
  [type-str]
  (get type-str->kw type-str :any))

(def ^:private role-str->kw
  "The same lookup for input roles. :table is the role of an input that is a
   whole dataset, which the body runs on one time."
  {"row" :row
   "broadcast" :broadcast
   "reducer" :reducer
   "table" :table})

(defn role-kind
  "The keyword for a role value. The role is a keyword on the JVM and a
   string on squint."
  [role]
  (cond
    (nil? role) nil
    (contains? #{:row :broadcast :reducer :table} role) role
    :else (get role-str->kw role)))

(def operation-kinds
  "The closed set of operation kinds. A built-in declares its kind. A user op
   is a :transformer, because the parser reads no kind metadata."
  #{:source :transformer :extractor :predicate :sink})

(def ^:private kind-str->kw
  {"source" :source
   "transformer" :transformer
   "extractor" :extractor
   "predicate" :predicate
   "sink" :sink})

(defn kind-kw
  "The keyword for an operation kind value (a keyword on the JVM, a string on
   squint). nil and an unknown value give :transformer."
  [k]
  (cond
    (nil? k) :transformer
    (contains? operation-kinds k) k
    :else (or (get kind-str->kw k) :transformer)))

(def predicate-kinds
  "The closed set of predicate names: the keys of the predicate map of a
   `(when-row {...} body)` rung, and the :predicate slot of
   `filter-by-column`. The names are the source-text spellings. The parser
   checks membership in the full set only: it does not separate the row
   comparators from the dataset gates (has-column?, crs-equals,
   row-count-gt)."
  #{"equals" "not-equals" "gt" "lt" "gte" "lte" "in" "matches-regex"
    "nil?" "some?" "has-column?" "crs-equals" "row-count-gt"})

(def option-keys
  "Source-text spellings of the option keys in the predicate map of a
   `(when-row {...} body)` rung. cg.macros separates the map entries into
   predicate keys and these option keys."
  #{"on-miss"})

(def option-defaults
  "The default value string of each option. `on-miss` says what a when-row
   does with a row whose predicate is false: `pass-through` (the default)
   emits the row unchanged, and `drop` removes it."
  {"on-miss" "pass-through"})

(defn- input
  "Builds a TypedInputSig. The defaults are no role and not optional."
  ([name type] (input name type nil false))
  ([name type role] (input name type role false))
  ([name type role optional?]
   {:name name :type type :role role :optional optional?}))

(defn output
  "Builds a TypedOutputSig. A last `:level <kw>` pair adds the data level of
   the output (see level-kinds)."
  ([name type] {:name name :type type})
  ([name type _level-key level]
   {:name name :type type :level level}))

(defn- sig
  "Builds a signature map. The optional opts map carries `:flags`, `:kind`
   (from `operation-kinds`, default :transformer) and `:threading-modes`."
  ([qualified-name inputs outputs] (sig qualified-name inputs outputs {}))
  ([qualified-name inputs outputs {:keys [flags kind threading-modes]
                                   :or {flags #{}
                                        kind :transformer
                                        threading-modes #{}}}]
   {:qualified-name qualified-name
    :inputs (vec inputs)
    :outputs (vec outputs)
    :flags flags
    :kind (kind-kw kind)
    :threading-modes threading-modes
    :source :built-in
    :location nil}))

(def built-ins-manual
  "Hand-written signatures of the built-in operations that
   `bb gen:signatures` does not generate: qualified-name string -> signature
   map. `built-ins` merges the generated map on top. geo/* is here because
   cg.geo has no register-operation! entries. ds/load-csv and
   gcrs/create-transform have no runtime defn."
  {"ds/load-csv"
   (sig "ds/load-csv"
        [(input "path" :string)]
        [(output "dataset" :dataset :level :dataset)]
        {:kind :source})

   "gcrs/create-transform"
   (sig "gcrs/create-transform"
        [(input "source-epsg" :number)
         (input "target-epsg" :number)
         (input "dataset" :any)]
        [(output "transform" :any)])

   "geo/buffer"
   (sig "geo/buffer"
        [(input "feature" :feature :row)
         (input "distance" :number)
         (input "opts" :any nil true)]
        [(output "feature" :feature)]
        {:threading-modes #{:item}})

   "geo/simplify"
   (sig "geo/simplify"
        [(input "feature" :feature :row)
         (input "tolerance" :number)
         (input "opts" :any nil true)]
        [(output "feature" :feature)]
        {:threading-modes #{:item}})

   "geo/area"
   (sig "geo/area"
        [(input "feature" :feature :row)]
        [(output "area" :number)]
        {:threading-modes #{:item}})

   "geo/centroid"
   (sig "geo/centroid"
        [(input "feature" :feature :row)]
        [(output "feature" :feature)]
        {:threading-modes #{:item}})

   "geo/distance"
   (sig "geo/distance"
        [(input "feature-a" :feature :row)
         (input "feature-b" :feature)]
        [(output "distance" :number)]
        {:threading-modes #{:item}})

   ;; A binary geometry op has one signature for both modes. Because item->
   ;; gives the second feature as an arg and coll-> folds the collection with
   ;; no arg, the second feature is optional. No pass checks the arity for
   ;; each mode.
   "geo/union"
   (sig "geo/union"
        [(input "feature-a" :feature :row)
         (input "feature-b" :feature nil true)]
        [(output "feature" :feature)]
        {:threading-modes #{:item :coll}})

   "geo/intersection"
   (sig "geo/intersection"
        [(input "feature-a" :feature :row)
         (input "feature-b" :feature nil true)]
        [(output "feature" :feature)]
        {:threading-modes #{:item :coll}})

   "geo/difference"
   (sig "geo/difference"
        [(input "feature-a" :feature :row)
         (input "feature-b" :feature nil true)]
        [(output "feature" :feature)]
        {:threading-modes #{:item :coll}})

   "geo/intersects?"
   (sig "geo/intersects?"
        [(input "feature-a" :feature :row)
         (input "feature-b" :feature)]
        [(output "intersects?" :boolean)]
        {:threading-modes #{:item}})

   "geo/contains?"
   (sig "geo/contains?"
        [(input "feature-a" :feature :row)
         (input "feature-b" :feature)]
        [(output "contains?" :boolean)]
        {:threading-modes #{:item}})

   ;; clojure.core/merge in a def that combines spec maps. Not a rung.
   "merge"
   (sig "merge"
        [(input "map" :any)]
        [(output "map" :any)]
        {:flags #{:variadic}})

   "aggregate"
   (sig "aggregate"
        [(input "dataset" :dataset :row)
         (input "config" :any)]
        [(output "dataset" :dataset)]
        {:threading-modes #{:table}})

   ;; coalesce gives a default for a nil value. Both slots are :any, because
   ;; the checker has no generics.
   "coalesce"
   (sig "coalesce"
        [(input "value" :any :row)
         (input "default" :any)]
        [(output "value" :any)]
        {:threading-modes #{:item}})
   })

(def ^:private built-in-namespaces
  "The namespace of each built-in alias. A .cg file can write a rung head in
   full (cg.dataset/transform-crs), because the threading macro resolves an
   alias only through a top-level require. The full name is the same op."
  {"ds" "cg.dataset" "geo" "cg.geo"})

(defn- with-full-names
  "`sigs` plus each aliased op under its full namespace name as well."
  [sigs]
  (into sigs
        (keep (fn [[k sig]]
                (when-let [[_ alias op] (re-find #"^([^/]+)/(.+)$" k)]
                  (when-let [n (get built-in-namespaces alias)]
                    [(str n "/" op) sig])))
              sigs)))

(def built-ins
  "The built-in registry of the parser: the generated signatures
   (cg.dsl.signatures-generated, from `bb gen:signatures`) merged on top of
   built-ins-manual. A generated entry wins on a collision. Each op of an
   alias in built-in-namespaces is also under its full name."
  (with-full-names (merge built-ins-manual gen/built-ins)))

(defn lookup
  "Look up a built-in signature by its source-text head spelling (for example
   \"geo/buffer\" or \"filter\"). nil when the op is not registered."
  [qualified-name]
  (get built-ins qualified-name))

(defn- project-input
  [typed-input]
  {:name (:name typed-input)
   :type (type-kind (:type typed-input))
   :role (role-kind (:role typed-input))
   :optional (boolean (:optional typed-input))})

(defn- project-output
  [typed-output]
  {:name (:name typed-output)
   :type (type-kind (:type typed-output))})

(defn- flag-set
  "Flags are stored on OperationDecl as a vector of strings. Maps the
   recognized ones to a set of canonical keywords."
  [flag-strs]
  (->> (or flag-strs [])
       (keep #(get {"idempotent" :idempotent
                    "pure"       :pure
                    "parallelisable" :parallelisable
                    ;; a defn (cg.parser.extract/extract-defn), and one
                    ;; with ^:op, which a rung can name
                    "defn"       :defn
                    "op"         :op
                    "variadic"   :variadic} %))
       set))

(defn from-operation-decl
  "Project an OperationDecl (from cg.parser.extract) into a signature map. A
   user op has no threading modes and the kind :transformer, because an op
   cannot declare a kind."
  [op-decl]
  {:qualified-name (:name op-decl)
   :inputs (mapv project-input (:inputs op-decl))
   :outputs (mapv project-output (:outputs op-decl))
   :flags (flag-set (:flags op-decl))
   :kind :transformer
   :threading-modes #{}
   :source :user
   :location (:location op-decl)})

(defn- alias-index
  "{required-namespace -> [alias ...]} from the :requires of a CgFlow. Only a
   require with an alias takes part."
  [cg-flow]
  (reduce (fn [m r]
            (if-let [a (:alias r)]
              (update m (:namespace r) (fn [v] (conj (or v []) a)))
              m))
          {}
          (:requires cg-flow)))

(defn from-cg-flow
  "Project each OperationDecl of a CgFlow into a {qualified-name ->
   signature} map. An op from a followed module (tagged :source-module, with
   the :ns of the module) is also keyed under the namespace of the module and
   under each alias that the requires of the flow give it. Both
   \"cg.parcel/prepare-parcel\" and \"parcel/prepare-parcel\" then resolve.
   .cg files write the full name at their call sites."
  [cg-flow]
  (let [aliases (alias-index cg-flow)]
    (into {}
          (mapcat (fn [op]
                    (let [sig    (from-operation-decl op)
                          mod-ns (:ns (:source-module op))]
                      (cons [(:name op) sig]
                            (for [n (when mod-ns
                                      (cons mod-ns (get aliases mod-ns)))]
                              [(str n "/" (:name op)) sig])))))
          (:operations cg-flow))))

(defn merge-signatures
  "Merge a base registry (usually `built-ins`) with one or more extras maps.
   A later map wins on a collision, as with `merge`."
  ([base] base)
  ([base extras]
   (merge (or base {}) (or extras {})))
  ([base extras & more]
   (reduce merge-signatures (merge-signatures base extras) more)))

(defn registry-for
  "The signature registry that a CgFlow sees: `built-ins` with the
   OperationDecls of the flow on top."
  [cg-flow]
  (merge-signatures built-ins (from-cg-flow cg-flow)))
