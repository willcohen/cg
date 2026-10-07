;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.js-repl-test
  "The nREPL server of the JS REPL over a real socket, with a stub eval."
  (:require [cljs.test :as t :refer [deftest is]]
            ["net" :as net]
            ["../../src/cg_vscode/repl/bencode.mjs" :as b]
            ["../../src/cg_vscode/repl/js_repl.mjs" :as repl]))

(defn- stub-eval [code]
  (js/Promise.resolve
   (if (= code "(boom)")
     #js {:err "boom" :out ""}
     #js {:value (str "v" code) :out "o\n"})))

(defn- done? [m]
  (some #(= "done" %) (or (aget m "status") #js [])))

(defn- exchange [port msg]
  (js/Promise.
   (fn [resolve reject]
     (let [^js sock (net/connect #js {:host "127.0.0.1" :port port})
           !buf (atom (js/Buffer.alloc 0))
           !got (atom [])]
       (.on sock "error" reject)
       (.on sock "connect" (fn [] (.write sock (b/encode msg))))
       (.on sock "data"
            (fn [chunk]
              (let [[msgs tail] (b/decode-all (js/Buffer.concat #js [@!buf chunk]))]
                (reset! !buf tail)
                (swap! !got into msgs)
                (when (some done? msgs)
                  (.destroy sock)
                  (resolve @!got)))))))))

(defn- field [msgs k] (some #(aget % k) msgs))

(deftest ^:async an-eval-gives-the-output-the-value-and-done
  (let [srv (await (repl/start-server! stub-eval))
        msgs (await (exchange (aget srv "port") #js {:op "eval" :code "(+ 1 2)" :id "e1" :session "s"}))]
    (is (= "o\n" (field msgs "out")))
    (is (= "v(+ 1 2)" (field msgs "value")))
    (is (= "user" (field msgs "ns")))
    (is (every? #(= "e1" (aget % "id")) msgs) "each response names the request")
    (is (every? #(= "s" (aget % "session")) msgs))
    (is (done? (last msgs)))
    (await (repl/stop-server! srv))))

(deftest ^:async a-failed-eval-gives-the-error
  (let [srv (await (repl/start-server! stub-eval))
        msgs (await (exchange (aget srv "port") #js {:op "eval" :code "(boom)" :id "e2"}))]
    (is (= "boom\n" (field msgs "err")))
    (is (some #(= "eval-error" %) (vec (field msgs "status"))))
    (await (repl/stop-server! srv))))

(deftest ^:async clone-describe-and-an-unknown-op
  (let [srv (await (repl/start-server! stub-eval))
        port (aget srv "port")]
    (is (string? (field (await (exchange port #js {:op "clone" :id "c"})) "new-session")))
    (is (some? (aget (field (await (exchange port #js {:op "describe" :id "d"})) "ops") "eval")))
    (is (some #(= "unknown-op" %)
              (vec (field (await (exchange port #js {:op "nope" :id "u"})) "status"))))
    (await (repl/stop-server! srv))))

(deftest ^:async load-file-evaluates-the-file
  (let [srv (await (repl/start-server! stub-eval))
        msgs (await (exchange (aget srv "port") #js {:op "load-file" :file "(f)" :id "l"}))]
    (is (= "v(f)" (field msgs "value")))
    (await (repl/stop-server! srv))))

(.then (js/Promise.resolve (t/run-tests)) #(js/process.exit (if (t/successful? %) 0 1)))
