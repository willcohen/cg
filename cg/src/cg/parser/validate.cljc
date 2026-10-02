;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.validate
  "Whole-file checks of an extracted CgFlow: duplicate names, name collisions,
   declared inputs and component names. Must stay free of eval: do not
   require cg.dsl, cg.macros, cg.run or cg.codegen."
  (:require [cg.parser.extract :as extract]
            [clojure.string :as str]))

#?(:clj (set! *warn-on-reflection* true))

(defn- diag
  ([code severity message location]
   {:code code :severity severity :message message :location location})
  ([code message location]
   (diag code :error message location)))

(defn- duplicate-name-diagnostics
  "A Diagnostic for each name that occurs more than one time in `items`, at
   the :location of each occurrence after the first."
  [items code message-prefix]
  (let [grouped (group-by :name items)]
    (->> grouped
         (filter (fn [[_ vs]] (> (count vs) 1)))
         (mapcat (fn [[name vs]]
                   (map (fn [v]
                          (diag code
                                (str message-prefix " '" name "' is defined more than once")
                                (:location v)))
                        (rest vs))))
         vec)))

(defn- check-duplicate-parameters [cg-flow]
  (duplicate-name-diagnostics (:parameters cg-flow)
                              :duplicate-parameter-name
                              "parameter"))

(defn- check-duplicate-bindings [cg-flow]
  (duplicate-name-diagnostics (:bindings cg-flow)
                              :duplicate-binding-name
                              "binding"))

(defn- check-duplicate-operations [cg-flow]
  (duplicate-name-diagnostics (:operations cg-flow)
                              :duplicate-operation-name
                              "operation"))

(defn- check-name-collisions
  "`:name-collision` diagnostics for a Binding that has the name of a
   Parameter or an OperationDecl, and for an OperationDecl that has the name
   of a Parameter. Two Bindings with one name get `:duplicate-binding-name`."
  [cg-flow]
  (let [param-names (into #{} (map :name) (:parameters cg-flow))
        op-names    (into #{} (map :name) (:operations cg-flow))
        binding-collisions
        (->> (:bindings cg-flow)
             (keep (fn [b]
                     (let [n (:name b)
                           hits (cond-> []
                                  (contains? param-names n) (conj (str "parameter '" n "'"))
                                  (contains? op-names n)    (conj (str "operation '" n "'")))]
                       (when (seq hits)
                         (diag :name-collision
                               (str "binding '" n "' collides with " (str/join " and " hits))
                               (:location b)))))))
        op-collisions
        (->> (:operations cg-flow)
             (keep (fn [op]
                     (let [n (:name op)
                           hits (cond-> []
                                  (contains? param-names n) (conj (str "parameter '" n "'")))]
                       (when (seq hits)
                         (diag :name-collision
                               (str "operation '" n "' collides with " (str/join " and " hits))
                               (:location op)))))))]
    (vec (concat binding-collisions op-collisions))))

(defn- check-inputs-declared
  "When the file declares :inputs, the source keyword of each template
   Binding (`:is-template`) must be in it. With no :inputs, a template can
   use any keyword. A Binding whose source is not a keyword is not checked."
  [cg-flow]
  (let [declared (:inputs cg-flow)]
    (if (empty? declared)
      []
      (let [declared-names (into #{} (map :name declared))]
        (->> (extract/threading-macro-bindings cg-flow)
             (filter :is-template)
             (remove (fn [b] (contains? declared-names (:input b))))
             (mapv (fn [b]
                     (diag :input-not-declared
                           (str "binding '" (:name b)
                                "' uses input :" (:input b)
                                " which is not in the file's ^:inputs declaration")
                           (:location b)))))))))

(defn- component-bearing-ops
  "Each op that can carry a :component: the :ops of bindings and sinks, and
   the body-graph nodes of operations. extract puts the name on :component
   and does not resolve it."
  [cg-flow]
  (concat (mapcat :ops (:bindings cg-flow))
          (mapcat :ops (:sinks cg-flow))
          (mapcat (comp :nodes :body-graph) (:operations cg-flow))))

(defn- check-map-components
  "Each op that names a component must name a declared op: an unknown name
   gives :unknown-component. A run-component call names its component in a
   string, and no other tool catches a typo there."
  [cg-flow]
  (let [op-names (into #{} (map :name) (:operations cg-flow))]
    (->> (component-bearing-ops cg-flow)
         (filter :component)
         (remove (fn [op] (contains? op-names (:component op))))
         (mapv (fn [op]
                 (diag :unknown-component
                       (str "component '" (:component op)
                            "' does not name a declared op")
                       (:location op)))))))

;; cg.parser.extract reports a second overlay itself, and :overlay holds one
;; value. This check fires only for a CgFlow that code builds with a vector
;; of overlays.
(defn- check-single-overlay [cg-flow]
  (when (vector? (:overlay cg-flow))
    (when (> (count (:overlay cg-flow)) 1)
      (mapv (fn [o]
              (diag :single-overlay
                    "at most one ^:cgproj overlay per file"
                    (:location o)))
            (rest (:overlay cg-flow))))))

(defn validate
  "Run the whole-file checks and return the CgFlow with the new :diagnostics
   added. A second call adds them again."
  [cg-flow]
  (let [extra (concat (check-duplicate-parameters cg-flow)
                      (check-duplicate-bindings cg-flow)
                      (check-duplicate-operations cg-flow)
                      (check-name-collisions cg-flow)
                      (check-inputs-declared cg-flow)
                      (check-map-components cg-flow)
                      (or (check-single-overlay cg-flow) []))]
    (update cg-flow :diagnostics into extra)))
