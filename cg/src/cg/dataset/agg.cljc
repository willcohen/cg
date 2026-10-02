;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.dataset.agg
  "Per-group reducers for `aggregate`: each takes one group sub-dataset and returns a scalar.
   Each has :role :reducer, which cg.feature/reducer? reads. JVM only."
  (:refer-clojure :exclude [count min max])
  #?(:clj (:require [cg.dataset :as ds]
                    [cg.feature :as f])))

#?(:clj (set! *warn-on-reflection* true))

#?(:clj
   (defn count
     "Row count of a sub-dataset."
     [sub-ds]
     (ds/row-count sub-ds)))

#?(:clj
   (defn sum
     "Sum of the `over` column in a sub-dataset."
     [sub-ds over]
     (reduce + 0 (ds/column sub-ds over))))

#?(:clj
   (defn mean
     "Arithmetic mean of the `over` column. nil on empty sub-dataset."
     [sub-ds over]
     (let [n (ds/row-count sub-ds)]
       (when (pos? n)
         (/ (sum sub-ds over) (double n))))))

#?(:clj
   (defn min
     "Minimum of the `over` column. nil on empty sub-dataset."
     [sub-ds over]
     (let [vals (ds/column sub-ds over)]
       (when (seq vals)
         (reduce clojure.core/min vals)))))

#?(:clj
   (defn max
     "Maximum of the `over` column. nil on empty sub-dataset."
     [sub-ds over]
     (let [vals (ds/column sub-ds over)]
       (when (seq vals)
         (reduce clojure.core/max vals)))))

#?(:clj
   (do
     (f/register-operation! "cg.dataset.agg/count"
                            {:type :transformer
                             :role :reducer
                             :params []})
     (f/register-operation! "cg.dataset.agg/sum"
                            {:type :transformer
                             :role :reducer
                             :params [{:name :over :required true :type :keyword}]})
     (f/register-operation! "cg.dataset.agg/mean"
                            {:type :transformer
                             :role :reducer
                             :params [{:name :over :required true :type :keyword}]})
     (f/register-operation! "cg.dataset.agg/min"
                            {:type :transformer
                             :role :reducer
                             :params [{:name :over :required true :type :keyword}]})
     (f/register-operation! "cg.dataset.agg/max"
                            {:type :transformer
                             :role :reducer
                             :params [{:name :over :required true :type :keyword}]})))
