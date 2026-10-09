;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.parser.types
  "Static type checker for the CG DSL. Runs after cg.parser.validate and adds
   :unresolved-op, :rung-not-op, :op-arity-mismatch, :type-mismatch,
   :pipeline-type-mismatch and :op-output-mismatch. Must stay free of eval:
   do not require cg.dsl, cg.macros, cg.run or cg.codegen (cg.dsl.signatures
   is pure data)."
  (:require [cg.dsl.signatures :as sigs]
            [cg.parser.extract :as extract :refer [diag]]))

#?(:clj (set! *warn-on-reflection* true))

(defn- t-name [t] (some-> t name))

(defn- literal-type
  "The type keyword of a LiteralValue: the :kind of a scalar, else :any."
  [lit]
  (let [k (:kind lit)]
    (if (contains? #{:number :string :keyword :boolean} k) k :any)))

(defn- parameter-type
  "The type of a parameter_ref OpArg, from the :parameters of the CgFlow. A
   missing parameter gives :any, and no validate pass reports it."
  [param-name cg-flow]
  (literal-type (:value (first (filter #(= (:name %) param-name) (:parameters cg-flow))))))

(defn- binding-output-type
  "The output type of the top-level Binding with this name: the first output
   of the signature of its head. :any when the registry does not resolve the
   binding."
  [binding-name cg-flow registry]
  (let [b (first (filter #(= (:name %) binding-name) (:bindings cg-flow)))
        sig (when b (get registry (:head-sym b)))
        out (first (:outputs sig))]
    (if out (:type out) :any)))

(defn- op-arg-type [arg cg-flow registry]
  (case (:kind arg)
    :literal        (literal-type (:literal arg))
    :inline_map     :any
    :parameter_ref  (parameter-type (:parameter-name arg) cg-flow)
    :symbol_ref     (let [text (:symbol-text arg)]
                      (if (or (nil? text) (>= (.indexOf ^String text "/") 0))
                        :any
                        (binding-output-type text cg-flow registry)))
    :any))

(defn- type-compatible?
  "Two type kinds are compatible when they match or either side is
   :any (the escape hatch)."
  [arg-type sig-type]
  (or (= arg-type :any)
      (= sig-type :any)
      (= arg-type sig-type)))

(defn- explicit-slots
  "The inputs of a signature that explicit args fill, in order. In
   :function-call context, all inputs. In :threading context the threaded
   value fills the :row or :table input, and the other inputs remain."
  [sig-inputs context]
  (case context
    :function-call (vec sig-inputs)
    :threading     (vec (remove #(contains? #{:row :table} (:role %)) sig-inputs))))

(defn- slot-bounds
  "{:min N :max M} for an ordered list of input slots. :min counts the
   required slots before the first optional slot. A required slot after an
   optional one is not modeled."
  [slots]
  (let [n (count slots)
        req (count (take-while (fn [s] (not (:optional s))) slots))]
    {:min req :max n}))

(defn- check-call
  "Check one call site: resolution, then arity, then the type of each arg.
   Returns a vector of diagnostics. A failed step stops the later steps."
  [{:keys [qualified-name args location context]} registry cg-flow]
  (let [sig (get registry qualified-name)]
    (cond
      (nil? sig)
      [(diag :unresolved-op
             (str "operation '" qualified-name
                  "' is not in the built-in registry or any user op")
             location)]

      (and (= :threading context)
           (contains? (:flags sig) :defn)
           (not (contains? (:flags sig) :op)))
      [(diag :rung-not-op
             (str "'" qualified-name "' is a plain defn. Put ^:op on its name "
                  "to use it as a rung.")
             location)]

      :else
      (let [slots (explicit-slots (:inputs sig) context)
            {req-count :min max-count :max} (slot-bounds slots)
            arg-count (count args)]
        (cond
          (< arg-count req-count)
          [(diag :op-arity-mismatch
                 (str "operation '" qualified-name "' expects at least "
                      req-count " argument" (when (not= 1 req-count) "s")
                      " but got " arg-count)
                 location)]

          (and (> arg-count max-count)
               (not (contains? (:flags sig) :variadic)))
          [(diag :op-arity-mismatch
                 (str "operation '" qualified-name "' accepts at most "
                      max-count " argument" (when (not= 1 max-count) "s")
                      " but got " arg-count)
                 location)]

          :else
          (vec
            (keep-indexed
              (fn [i arg]
                (let [slot (nth slots i nil)
                      sig-t (:type slot)
                      arg-t (op-arg-type arg cg-flow registry)]
                  (when (and slot
                             (not (type-compatible? arg-t sig-t)))
                    (diag :type-mismatch
                          (str "operation '" qualified-name
                               "' argument '" (:name slot)
                               "' expects type " (t-name sig-t)
                               " but got " (t-name arg-t))
                          (:location arg)))))
              args)))))))

(defn- binding-call-site [b]
  {:qualified-name (:head-sym b)
   :args (vec (:args b))
   :location (:location b)
   :context :function-call})

(defn- threading-op-call-site
  "Projects one OpCall (from a Model or a decoded pipeline body form)
   into a checkable call site in :threading context."
  [opcall]
  {:qualified-name (:qualified-name opcall)
   :args (vec (:args opcall))
   :location (:location opcall)
   :context :threading})

(defn- model-call-sites [m]
  (mapv threading-op-call-site (:ops m)))

(defn- pipeline-body-form
  "The OpBodyForm when it is a :pipeline, else nil."
  [body-form]
  (when (= :pipeline (:kind body-form))
    body-form))

(defn- operation-decl-call-sites [op-decl]
  (mapcat (comp model-call-sites pipeline-body-form) (:body op-decl)))

(defn- call-sites
  "Each call site in a CgFlow that the checker can inspect: the heads of
   function-call Bindings, the :ops of threading-macro Bindings and of
   SinkPipelines, and the OpCalls in OperationDecl bodies."
  [cg-flow]
  ;; An accessor, a control form, a collection and an alias binding have no
  ;; signature. A check of one gives a false :unresolved-op, and extract
  ;; already reports the control form as :control-form-binding.
  (concat (map binding-call-site
               (remove (fn [b] (or (:is-accessor b) (:is-control b)
                                   (:is-collection b) (:is-alias b)))
                       (extract/function-call-bindings cg-flow)))
          (mapcat model-call-sites (extract/threading-macro-bindings cg-flow))
          (mapcat operation-decl-call-sites (:operations cg-flow))
          (mapcat model-call-sites (:sinks cg-flow))))

(defn- sig-output-type
  "Returns the type of a signature's first declared output, or :any
   when the signature declares no outputs."
  [sig]
  (or (-> sig :outputs first :type) :any))

(defn- sig-row-input-type
  "The type of the first :row input of a signature: the slot that the
   threaded value fills. Falls back to the first input, then to :any."
  [sig]
  (let [inputs (:inputs sig)
        row    (first (filter #(= :row (:role %)) inputs))]
    (or (:type row)
        (:type (first inputs))
        :any)))

(defn- check-pipeline-flow
  "Check that the first output of each op in a decoded pipeline fits the :row
   input of the next op. A keyword source has no type and is not checked."
  [pipeline registry]
  (let [ops (:ops pipeline)]
    (keep (fn [[prev cur]]
            (let [prev-sig (get registry (:qualified-name prev))
                  cur-sig  (get registry (:qualified-name cur))]
              (when (and prev-sig cur-sig)
                (let [prev-out (sig-output-type prev-sig)
                      cur-in   (sig-row-input-type cur-sig)]
                  (when-not (type-compatible? prev-out cur-in)
                    (diag :pipeline-type-mismatch
                          (str "operation '" (:qualified-name cur)
                               "' expects " (t-name cur-in)
                               " on its row input but the previous "
                               "op '" (:qualified-name prev)
                               "' returns " (t-name prev-out))
                          (:location cur)))))))
          (map vector ops (rest ops)))))

(defn- all-pipelines
  "Each pipeline in a CgFlow that the checker can inspect: threading-macro
   Bindings, the :pipeline OpBodyForms in OperationDecl bodies, and
   SinkPipelines."
  [cg-flow]
  (concat (extract/threading-macro-bindings cg-flow)
          (mapcat (fn [op-decl]
                    (keep pipeline-body-form (:body op-decl)))
                  (:operations cg-flow))
          (:sinks cg-flow)))

(defn- pass-4-pipeline-flow [cg-flow registry]
  (mapcat (fn [p] (check-pipeline-flow p registry)) (all-pipelines cg-flow)))

(defn- terminal-op-output-type
  "The :type of the last OpCall's first output in a decoded pipeline,
   or nil when undetermined."
  [pipeline registry]
  (let [last-op (last (:ops pipeline))
        sig (when last-op (get registry (:qualified-name last-op)))]
    (when sig (sig-output-type sig))))

(defn- check-op-output [op-decl registry]
  (let [outputs (:outputs op-decl)
        body (:body op-decl)]
    (cond
      ;; Only an op with one output and one body form is checked: no
      ;; convention maps more than one output to columns.
      (not= 1 (count outputs)) []
      (not= 1 (count body))    []

      :else
      (let [term (pipeline-body-form (first body))
            term-type (when term (terminal-op-output-type term registry))
            decl-out-type (sigs/type-kind (-> outputs first :type))]
        (cond
          ;; No terminal pipeline, or a last op that does not resolve.
          ;; Another pass reports the op that does not resolve.
          (nil? term-type) []

          (type-compatible? term-type decl-out-type)
          []

          :else
          [(diag :op-output-mismatch
                 (str "op '" (:name op-decl)
                      "' declares output type " (t-name decl-out-type)
                      " but its body's terminal op returns " (t-name term-type))
                 (:location op-decl))])))))

(defn- pass-5-op-output [cg-flow registry]
  (mapcat (fn [op-decl] (check-op-output op-decl registry))
          (:operations cg-flow)))

(defn check
  "Type-check a CgFlow and return it with the new :diagnostics added. A second
   call adds them again: use recheck. Without `registry`, builds one from the
   built-ins and the OperationDecls of the flow."
  ([cg-flow]
   (check cg-flow (sigs/registry-for cg-flow)))
  ([cg-flow registry]
   (let [per-call (mapcat (fn [site] (check-call site registry cg-flow))
                          (call-sites cg-flow))
         flow-diags (pass-4-pipeline-flow cg-flow registry)
         output-diags (pass-5-op-output cg-flow registry)]
     (update cg-flow :diagnostics into
             (concat per-call flow-diags output-diags)))))

(def ^:private checker-codes
  "Each diagnostic code that this namespace emits. recheck removes these
   before it runs the checker again."
  #{:unresolved-op :op-arity-mismatch :type-mismatch
    :pipeline-type-mismatch :op-output-mismatch :rung-not-op})

(defn recheck
  "Remove the diagnostics of this namespace and check again, after a caller
   added :operations to the flow. A :source-module op resolves under the
   require aliases of the flow."
  [cg-flow]
  (-> cg-flow
      (update :diagnostics
              (fn [ds]
                (vec (remove (fn [d] (contains? checker-codes (:code d))) ds))))
      check))
