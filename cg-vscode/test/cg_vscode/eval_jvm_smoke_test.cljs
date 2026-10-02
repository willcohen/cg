;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.eval-jvm-smoke-test
  "The JVM eval wiring against a running cg-app backend. Set CG_JVM_PORT to the
   port in cg-app/.cg-backend-port; with no reachable backend, the test skips."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/runtime/jvm_client.mjs" :as jvm]
            ["fs" :as fs]
            ["http" :as http]))

(def ^:private mi-path "../cg/test/fixtures/model-inspection.cg")

(def ^:private jvm-port
  (let [p (aget (.-env js/process) "CG_JVM_PORT")]
    (when p (js/parseInt p 10))))

(defn- backend-up? [port]
  (js/Promise.
   (fn [resolve _reject]
     (-> (.request http
                   #js {:hostname "localhost" :port port :method "GET"
                        :path "/api/health" :timeout 1000}
                   (fn [^js res]
                     (.resume res)
                     (resolve (= 200 (.-statusCode res)))))
         (.on "error" (fn [_e] (resolve false)))
         (.on "timeout" (fn [] (resolve false)))
         (.end)))))

(deftest ^:async jvm-eval-against-running-backend
  (try
    (let [up? (and jvm-port (await (backend-up? jvm-port)))]
      (if-not up?
        (do (println "[skip] no reachable backend (set CG_JVM_PORT=<bound port> after `bb backend:dev`)")
            (is true))
        (let [text (.toString (fs/readFileSync mi-path))
              {:keys [result error]} (await (jvm/eval-cg jvm-port text "model-inspection.cg"))]
          (when error
            (println "[jvm-eval] error:" error))
          (println "[jvm-eval] result:" (pr-str result))
          (is (nil? error))
          (is (some? result))
          (is (= "ok" (:status result)))
          (let [vars (:vars result)]
            (is (vector? vars))
            (is (= 1 (count vars)))
            (is (= "my-model" (-> vars first :name)))))))
    (catch :default e
      (println "[jvm-eval] unexpected:" (.-message e))
      (is false (str "unexpected: " (.-message e))))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
