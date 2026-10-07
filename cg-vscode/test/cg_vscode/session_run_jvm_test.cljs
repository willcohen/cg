;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.session-run-jvm-test
  "jvm-client/run-in-session and start-repl against a running cg-app backend:
   the trace, the layers that the CG Map tab draws, and the nREPL. Set
   CG_JVM_PORT to the bound port; with no reachable backend, the test skips."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/runtime/jvm_client.mjs" :as jvm]
            ["../../../cg-app/shared/squint/src/cg_gui/run/jvm.mjs" :as api]
            ["../../../cg-app/shared/squint/src/cg_gui/run/js.mjs" :as run-js]
            ["net" :as net]))

(def ^:private jvm-port
  (let [p (aget (.-env js/process) "CG_JVM_PORT")]
    (when p (js/parseInt p 10))))

(def ^:private send (when jvm-port (jvm/send-to jvm-port nil)))

(def ^:private layer-models
  "(require '[cg.dataset :as ds])
(require '[cg.feature :as f])
(require '[cg.geo :as geo])
(require '[cg.macros :refer [table->]])
(def shapes (ds/->dataset {:geometry [(f/point 0 0) (f/point 1 1)] :name [\"a\" \"b\"]}))
(def rings (table-> :shapes (geo/buffer 1) (map-layer {:name \"rings\" :kind :fill})))
(def dots (table-> :shapes (map-layer {:name \"dots\" :kind :point})))")

(deftest ^:async a-session-run-gives-the-trace-and-the-layers
  (if-not (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (println "[skip] no reachable backend (set CG_JVM_PORT=<bound port> after `bb backend:dev`)")
    (let [sid (await (api/create-session send))
          !events (atom [])
          r (await (jvm/run-in-session jvm-port sid #js {:code layer-models :filename "layers.cg"}
                                       (fn [evt] (swap! !events conj evt))))
          res (:result r)
          layers (when res (run-js/run-result->layers nil res))]
      (is (nil? (:error r)) (str "error: " (:error r)))
      (is (pos? (count @!events)) "trace events before the end")
      (is (= "dots" (when res (aget res "name"))))
      (is (= ["rings" "dots"] (when layers (mapv (fn [l] (aget l "var")) layers))))
      (is (= "fill" (when layers (aget (aget (first layers) "spec") "kind")))))))

(deftest ^:async an-unknown-session-is-expired
  (when (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (is (:expired (await (jvm/run-in-session jvm-port "no-such-session"
                                             #js {:code layer-models} (fn [_] nil)))))))

(defn- accepts? [port]
  (js/Promise.
   (fn [resolve _]
     (let [^js sock (net/connect #js {:host "127.0.0.1" :port port})]
       (.on sock "connect" (fn [] (.destroy sock) (resolve true)))
       (.on sock "error" (fn [_] (resolve false)))))))

(deftest ^:async the-repl-of-a-session-starts-one-time
  (when (and jvm-port (await (api/health? (jvm/send-to jvm-port 1000))))
    (let [sid (await (api/create-session send))
          r1 (await (api/start-repl send sid))
          r2 (await (api/start-repl send sid))]
      (is (pos? (:port r1)) (str "start-repl: " r1))
      (is (= (:port r1) (:port r2)))
      (is (await (accepts? (:port r1))) "the nREPL takes a connection")
      (is (:expired (await (api/start-repl send "no-such-session")))))))

(.then (js/Promise.resolve (t/run-tests)) #(js/process.exit (if (t/successful? %) 0 1)))
