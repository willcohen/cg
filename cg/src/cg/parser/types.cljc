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

(defn- literal-type [lit]
  (let [k (:kind lit)]
    (if (contains? #{:number :string :keyword :boolean} k) k :any)))

(defn- sig-output-type [sig]
  (or (-> sig :outputs first :type) :any))

(defn- binding-output-type
  "The output type of the top-level Binding with this name: the first output
   of the signature of its head. :any when the registry does not resolve the
   binding."
  [binding-name cg-flow registry]
  (let [b (first (filter #(= (:name %) binding-name) (:bindings cg-flow)))]
    (sig-output-type (when b (get registry (:head-sym b))))))

(defn- parameter-type
  "The type of a parameter_ref OpArg: of the Parameter with this name, else
   of the Binding with this name (extract makes each local symbol a
   parameter_ref), else :any."
  [param-name cg-flow registry]
  (if-let [p (first (filter #(= (:name %) param-name) (:parameters cg-flow)))]
    (literal-type (:value p))
    (binding-output-type param-name cg-flow registry)))

(defn- op-arg-type [arg cg-flow registry]
  (case (:kind arg)
    :literal        (literal-type (:literal arg))
    :parameter_ref  (parameter-type (:parameter-name arg) cg-flow registry)
    :any))

(defn- type-compatible? [arg-type sig-type]
  (or (= arg-type :any)
      (= sig-type :any)
      (= arg-type sig-type)))

(defn- explicit-slots
  "The inputs of a signature that explicit args fill, in order. In
   :function-call context, all inputs. In :threading context the threaded
   value fills the :row or :table input of a built-in, and the first input of
   a user op, as cg.run calls it. The other inputs remain."
  [{:keys [inputs source]} context]
  (case context
    :function-call (vec inputs)
    :threading     (if (= :user source)
                     (vec (rest inputs))
                     (vec (remove #(contains? #{:row :table} (:role %)) inputs)))))

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
      (let [slots (explicit-slots sig context)
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

(defn- call-site [context qualified-name {:keys [args location]}]
  {:qualified-name qualified-name :args (vec args) :location location :context context})

(defn- model-call-sites [m]
  (mapv #(call-site :threading (:qualified-name %) %) (:ops m)))

(defn- pipeline-body-form [body-form]
  (when (= :pipeline (:kind body-form))
    body-form))

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

(defn- call-sites
  "Each call site in a CgFlow that the checker can inspect: the heads of
   function-call Bindings, and the ops of each pipeline of all-pipelines."
  [cg-flow]
  ;; An accessor, a control form, a collection and an alias binding have no
  ;; signature. A check of one gives a false :unresolved-op, and extract
  ;; already reports the control form as :control-form-binding.
  (concat (map #(call-site :function-call (:head-sym %) %)
               (remove (fn [b] (or (:is-accessor b) (:is-control b)
                                   (:is-collection b) (:is-alias b)))
                       (extract/function-call-bindings cg-flow)))
          (mapcat model-call-sites (all-pipelines cg-flow))))

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
   input of the next op. The source is not an op and is not checked."
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

(defn- pass-4-pipeline-flow [cg-flow registry]
  (mapcat (fn [p] (check-pipeline-flow p registry)) (all-pipelines cg-flow)))

(defn- terminal-op-output-type
  "The :type of the last OpCall's first output in a decoded pipeline,
   or nil when undetermined."
  [pipeline registry]
  (let [last-op (last (:ops pipeline))
        sig (when last-op (get registry (:qualified-name last-op)))]
    (when sig (sig-output-type sig))))

(defn- check-op-output [{:keys [outputs body] :as op-decl} registry]
  ;; Only an op with one output and one body form is checked: no convention
  ;; maps more than one output to columns. A nil term-type is no terminal
  ;; pipeline, or a last op that does not resolve, which another pass reports.
  (when (and (= 1 (count outputs)) (= 1 (count body)))
    (let [term-type (terminal-op-output-type (pipeline-body-form (first body)) registry)
          decl-out-type (sigs/type-kind (-> outputs first :type))]
      (when (and term-type (not (type-compatible? term-type decl-out-type)))
        [(diag :op-output-mismatch
               (str "op '" (:name op-decl)
                    "' declares output type " (t-name decl-out-type)
                    " but its body's terminal op returns " (t-name term-type))
               (:location op-decl))]))))

(defn- pass-5-op-output [cg-flow registry]
  (mapcat (fn [op-decl] (check-op-output op-decl registry))
          (:operations cg-flow)))

(defn check
  "Type-check a CgFlow and return it with the new :diagnostics added. A second
   call adds them again: use recheck. The registry holds the built-ins and
   the OperationDecls of the flow."
  [cg-flow]
  (let [registry (sigs/registry-for cg-flow)
        per-call (mapcat (fn [site] (check-call site registry cg-flow))
                         (call-sites cg-flow))
        flow-diags (pass-4-pipeline-flow cg-flow registry)
        output-diags (pass-5-op-output cg-flow registry)]
    (update cg-flow :diagnostics into
            (concat per-call flow-diags output-diags))))

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
