;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.projection-test
  "A wait on a running projection init settles on ready and on error."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/projection.mjs" :as proj]))

(defn- ^:async settle [p]
  (try
    #js {:ok (await p)}
    (catch :default e #js {:error (.-message e)})))

(deftest ^:async a-wait-during-init-rejects-when-the-init-fails
  (reset! proj/proj-state {:status "loading" :error nil :modules nil})
  (let [p (proj/ensure-init!)]
    (swap! proj/proj-state assoc :status "error" :error "no backproj")
    (let [r (await (settle p))]
      (is (nil? (.-ok r)) "the wait must reject")
      (is (.includes (str (.-error r)) "no backproj"))))
  (reset! proj/proj-state {:status "idle" :error nil :modules nil}))

(deftest ^:async a-wait-during-init-resolves-when-the-init-is-ready
  (reset! proj/proj-state {:status "loading" :error nil :modules nil})
  (let [p (proj/ensure-init!)]
    (swap! proj/proj-state assoc :status "ready")
    (let [r (await (settle p))]
      (is (true? (.-ok r)))
      (is (nil? (.-error r)))))
  (reset! proj/proj-state {:status "idle" :error nil :modules nil}))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
