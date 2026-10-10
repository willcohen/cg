;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.jvm-client-test
  "The SSE parser and the trace stream of the JVM client. No backend."
  (:require [cljs.test :as t :refer [deftest is]]
            ["child_process" :as cp]
            ["http" :as http]
            ["../../src/cg_vscode/runtime/jvm_client.mjs" :as jvm]))

(defn- kinds [events] (mapv (fn [e] (aget e "kind")) events))

(deftest complete-frames-give-events-and-the-tail-waits
  (let [[events tail] (jvm/sse-events "data: {\"kind\":\"a\"}\n\ndata: {\"kind\":\"b\"}\n\ndata: {\"ki")]
    (is (= ["a" "b"] (kinds events)))
    (is (= "data: {\"ki" tail) "a chunk can end in the middle of a frame")))

(deftest a-frame-that-does-not-parse-is-skipped
  (let [[events tail] (jvm/sse-events "data: {bad\n\n: comment\n\ndata: {\"kind\":\"c\"}\n\n")]
    (is (= ["c"] (kinds events)))
    (is (= "" tail))))

;; With network inspection on, node:inspector throws on a response chunk that
;; is a string, not a Buffer.
(def ^:private network-inspection-script
  (str "import inspector from 'node:inspector';"
       "import http from 'node:http';"
       "const jvm = await import('" (.-href (js/URL. "../../src/cg_vscode/runtime/jvm_client.mjs"
                                                     js/import.meta.url)) "');"
       "const s = new inspector.Session(); s.connect(); s.post('Network.enable');"
       "const frame = Buffer.from('data: {\"kind\":\"output\",\"text\":\"caf\\u00e9\"}\\n\\n');"
       "const cut = frame.indexOf(0xa9);"
       "const srv = http.createServer((req, res) => {"
       "  res.writeHead(200, {'Content-Type': 'text/event-stream'});"
       "  res.write(frame.subarray(0, cut)); res.write(frame.subarray(cut));"
       "  setTimeout(() => res.end('data: {\"kind\":\"execution-ended\",\"outcome\":\"success\"}\\n\\n'), 20);"
       "});"
       "srv.listen(0, async () => {"
       "  const seen = [];"
       "  try {"
       "    const end = await jvm.stream_trace(srv.address().port, 'x', (e) => seen.push(e.text));"
       "    console.log('OK ' + end.outcome + ' ' + seen.join(','));"
       "  } catch (e) { console.log('FAIL ' + e.message); }"
       "  srv.close(); process.exit(0);"
       "});"))

(deftest the-trace-stream-reads-under-network-inspection
  (let [r (cp/spawnSync js/process.execPath
                        #js ["--experimental-network-inspection" "--input-type=module"
                             "-e" network-inspection-script]
                        #js {:encoding "utf8" :timeout 20000})]
    (is (= "OK success café" (.trim (.-stdout r))) (.-stderr r))))

(defn- outcome [p]
  (js/Promise.race
   #js [(.then p (fn [_] "resolved") (fn [e] (.-message e)))
        (js/Promise. (fn [resolve] (js/setTimeout #(resolve "pending") 2000)))]))

(deftest ^:async a-response-that-the-backend-cuts-rejects
  (let [srv (http/createServer (fn [_ ^js res]
                                 (.writeHead res 200 #js {"Content-Type" "text/event-stream"})
                                 (.flushHeaders res)
                                 (.destroy (.-socket res))))
        _ (await (js/Promise. (fn [resolve] (.listen srv 0 resolve))))
        port (.-port (.address srv))]
    (is (= "the trace stream ended before the execution (backend gone?)"
           (await (outcome (jvm/stream-trace port "x" (fn [_] nil))))))
    (is (= "the backend closed the response"
           (await (outcome ((jvm/send-to port nil) "GET" "/" #js {} nil)))))
    (.close srv)))

(.then (js/Promise.resolve (t/run-tests)) #(js/process.exit (if (t/successful? %) 0 1)))
