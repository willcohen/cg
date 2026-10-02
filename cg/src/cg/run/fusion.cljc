;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; cg.run.fusion decides which adjacent operations of a Model become one fused
;; step fn for cg.run/model->flow. Consecutive per-row ops become one proc,
;; whose body maps the composed row fns over the compute pool. Whole-dataset,
;; source and terminal ops break the chain. The parallelism is in the body of
;; the step fn, not in extra flow primitives.

(ns cg.run.fusion
  (:require [cg.feature :as f]
            [clojure.string :as str])
  #?(:clj
     (:import [org.locationtech.jts.geom Geometry TopologyException]
              [org.locationtech.jts.geom.util GeometryFixer])))

#?(:clj (set! *warn-on-reflection* true))

;; :op without :fn is the node shape of the fusion unit tests.
(defn node->op-key
  "The registry key of `node`, from :fn, else :op. A bare name gets its namespace from
   bare-op-namespaces, as in cg.run/operation->step."
  [node]
  (let [raw (or (:fn node) (:op node))
        s (cond
            (nil? raw) nil
            (keyword? raw) (subs (str raw) 1)
            :else (str raw))]
    (cond
      (nil? s) nil
      (str/includes? s "/") s
      :else (or (when-let [ns-part (f/get-bare-op-namespace s)]
                  (str ns-part "/" s))
                s))))

(defn- per-row?
  "True when the registered op of `node` has `:row-shape :per-row`. An op
   with no registered row shape is not fusable and runs as its own step."
  [node]
  (= :per-row (:row-shape (f/get-operation-info (node->op-key node)))))

(defn plan-fusion
  "Group consecutive per-row nodes of `model` into `{:groups [[node-id ...] ...]}`.
   `(:nodes model)` must be in topological order: the edges are not read."
  [model]
  (let [nodes (:nodes model)
        groups (reduce (fn [acc node]
                         (let [pr? (per-row? node)
                               last-group (peek acc)
                               last-id (peek last-group)
                               last-node (when last-id
                                           (some #(when (= last-id (:id %)) %)
                                                 nodes))
                               last-pr? (and last-node (per-row? last-node))]
                           (if (and pr? last-pr?)
                             (conj (pop acc) (conj last-group (:id node)))
                             (conj acc [(:id node)]))))
                       []
                       nodes)]
    {:groups groups}))

#?(:clj
   (defn- pool-size
     "The pool size of `exec`: the maximum pool size of a ThreadPoolExecutor,
      or the processor count for any other ExecutorService."
     [^java.util.concurrent.ExecutorService exec]
     (if (instance? java.util.concurrent.ThreadPoolExecutor exec)
       (.getMaximumPoolSize ^java.util.concurrent.ThreadPoolExecutor exec)
       (.. Runtime getRuntime availableProcessors))))

#?(:clj
   ;; One chunk per pool worker, not one task per row: fewer tasks contend less on the shared queue.
   (defn parallel-map
     "Apply `f` to each element of `coll` on ExecutorService `exec`, results in input order. JVM only."
     [^java.util.concurrent.ExecutorService exec f coll]
     (let [v (vec coll)
           n (count v)]
       (if (zero? n)
         []
         (let [workers (pool-size exec)
               chunk-size (max 1 (quot (+ n workers -1) workers))
               chunks (partition-all chunk-size v)
               callables (mapv (fn [chunk]
                                 ^java.util.concurrent.Callable
                                 (fn [] (mapv f chunk)))
                               chunks)
               futures (.invokeAll exec callables)]
           (into [] cat
                 (map (fn [^java.util.concurrent.Future fut] (.get fut))
                      futures)))))))

#?(:clj
   ;; A row fn can return a persistent map, and the assoc! of the next step fails on one.
   (defn- ensure-transient
     "Return `r` as a transient row map."
     [r]
     (cond
       (instance? clojure.lang.ITransientCollection r) r
       (instance? clojure.lang.IEditableCollection r) (transient r)
       :else (transient (into {} r)))))

#?(:clj
   (defn- retry-after-topology-exception
     "Fix each JTS Geometry under the keys `reads` of row `r` with GeometryFixer, then call `f` again.
      Returns a transient row: the retry result, or the repaired row with `:error` when it throws."
     [r f reads]
     (let [pr (persistent! r)
           repaired (reduce (fn [acc k]
                              (let [v (get acc k)]
                                (if (and v (instance? Geometry v))
                                  (assoc acc k (GeometryFixer/fix v))
                                  acc)))
                            pr
                            reads)
           rt (transient repaired)]
       (try
         (ensure-transient (f rt))
         (catch Throwable t
           (assoc! rt :error (str "post-repair: " (ex-message t))))))))

(def default-row-id-keys
  "The ordered row keys that name a row in the trace and the DIAG lines.
   The :row-id-keys opt of `cg.run/model->flow` overrides them."
  [:id])

(defn row-id
  "The value of the first key in `row-id-keys` that has a value in
   `row`, or nil."
  [row row-id-keys]
  (some #(get row %) row-id-keys))

#?(:clj
   (defn- diag-row-id
     "The id of `r` for a DIAG line. The keys come from the :row-id-keys
      metadata of the row-fn `f`."
     [r f]
     (or (row-id r (or (:row-id-keys (meta f)) default-row-id-keys)) "?")))

;; One bad row must not fail the chunk. A row fn can assoc! onto the transient row or return a new map.
(defn apply-row-chain
  "Apply `row-fns` in order to `row` and return the final persistent map. A row with `:error`
   skips the rest, and an exception in a fn becomes `:error` (its message) on the row.
   JVM: fn meta :row-reads names the geometry keys to repair, :row-id-keys the DIAG id keys."
  [row row-fns]
  (persistent!
   (reduce (fn [r [#_{:clj-kondo/ignore [:unused-binding]} idx f]]
             (if (get r :error)
               r
               (try
                 #?(:clj  (ensure-transient (f r))
                    :cljs (f r))
                 #?@(:clj
                     [(catch TopologyException te
                        ;; GeometryFixer can repair most JTS topology errors.
                        ;; Repair the geometries under the :row-reads of the op
                        ;; and retry one time. register-op! registers #{} for an
                        ;; op that declares no reads; that op gets :geometry
                        ;; repaired.
                        (let [reads (or (not-empty (:row-reads (meta f)))
                                        #{:geometry})]
                          (println "DIAG apply-row-chain TopologyException retry"
                                   "row-id=" (diag-row-id r f)
                                   "op-idx=" idx
                                   "reads=" reads
                                   "msg=" (.getMessage te))
                          (retry-after-topology-exception r f reads)))])
                 (catch #?(:clj Throwable :cljs :default) t
                   #?(:clj
                      (println "DIAG apply-row-chain :error"
                               "row-id=" (diag-row-id r f)
                               "op-idx=" idx
                               "exc=" (.getName (class t))
                               "msg=" (.getMessage t)))
                   (assoc! r :error (ex-message t))))))
           ;; A FastStruct row of tech.v3.dataset is not an
           ;; IEditableCollection, and (transient row) on it throws
           ;; ClassCastException. Copy it with (into {} row) first. A Clojure
           ;; map needs no copy.
           #?(:clj
              (if (instance? clojure.lang.IEditableCollection row)
                (transient row)
                (transient (into {} row)))
              :cljs (transient row))
           (map-indexed vector row-fns))))
