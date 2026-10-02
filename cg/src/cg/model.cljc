;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; A model is a map {:nodes [...] :edges [...] :config {...}}: the graph of a
;; computation, between the threading macros and the run. A node has :type
;; :source, :operation or :sink. An edge has :type :data-flow (the default) or
;; :materialization.

(ns cg.model)

#?(:clj (set! *warn-on-reflection* true))

(defn create-model
  "Create a model from a map of :nodes, :edges and :config. A missing key
  gets an empty default."
  [{:keys [nodes edges config] :as model}]
  (merge {:nodes (or nodes [])
          :edges (or edges [])
          :config (or config {})}
         model))

#?(:cljs
   (defonce ^:private staged-models (atom {:n 0 :by-id {}})))

#?(:cljs
   (defn stage-model!
     "Store a model under a new integer id and return the id."
     [m]
     (let [id (:n (swap! staged-models update :n inc))]
       (swap! staged-models assoc-in [:by-id id] m)
       id)))

#?(:cljs
   (defn- fill-arg-refs
     "`x` with each placeholder cg$arg$<i> (cg.macros/lift-rung-args)
      replaced by the i-th of `args`."
     [x args]
     (cond
       (and (string? x) (.startsWith x "cg$arg$")) (aget args (js/parseInt (subs x 7) 10))
       (vector? x) (mapv (fn [v] (fill-arg-refs v args)) x)
       (map? x) (into {} (map (fn [[k v]] [k (fill-arg-refs v args)])) x)
       :else x)))

#?(:cljs
   (defn get-staged-model
     "Return the model staged under `id` by stage-model!. With `args`, return a
      copy with each cg$arg$<i> placeholder replaced by the i-th arg."
     ([id]
      (get-in @staged-models [:by-id id]))
     ([id args]
      ;; The whole node, because a group holds its params again in its
      ;; :expansion.
      (update (get-staged-model id) :nodes fill-arg-refs args))))

(defn model?
  "True when x is a model. On JS, x can be a map or a plain JavaScript
  object."
  [x]
  #?(:clj (and (map? x) (:nodes x) (:edges x))
     :cljs (and x
                (or (map? x) (object? x))
                (or (get x :nodes) (aget x "nodes"))
                (or (get x :edges) (aget x "edges")))))
