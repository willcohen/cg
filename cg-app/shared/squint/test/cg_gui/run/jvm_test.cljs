;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run.jvm-test
  "The client of the JVM backend with a stub transport: the request of each
   call, and the outcome of each status."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_gui/run/jvm.mjs" :as jvm]))

(defn- stub-send
  "A send that records each request in `calls` and answers from `answers`, a
   map of \"METHOD path\" to #js {:status :body}. Any other request gets 500."
  [calls answers]
  (fn [method path headers body]
    (.push calls #js {:method method :path path :headers headers :body body})
    (js/Promise.resolve (or (get answers (str method " " path))
                            #js {:status 500 :body #js {:error "no stub"}}))))

(defn- ok [body] #js {:status 200 :body body})

(def ^:private gone #js {:status 404 :body #js {:error "Session not found or expired"}})

(deftest ^:async health-is-true-on-200-and-false-on-a-transport-error
  (is (true? (await (jvm/health? (stub-send #js [] {"GET /api/health" (ok #js {:status "ok"})})))))
  (is (false? (await (jvm/health? (fn [_ _ _ _] (js/Promise.reject (js/Error. "ECONNREFUSED"))))))))

(deftest ^:async create-session-gives-the-id-or-throws
  (is (= "s-1" (await (jvm/create-session
                       (stub-send #js [] {"POST /api/session/create" (ok #js {:session-id "s-1"})})))))
  (is (= "failed"
         (try (await (jvm/create-session (stub-send #js [] {})))
              "made one"
              (catch :default _ "failed")))))

(deftest ^:async a-session-call-sends-the-session-id-and-tags-the-outcome
  (let [calls #js []
        send (stub-send calls {"POST /api/session/load" (ok #js {:vars #js [#js {:name "a"}]
                                                                 :files #js ["a.cg"]})
                               "POST /api/session/repl" gone
                               "POST /api/execution/start"
                               #js {:status 409 :body #js {:error "busy" :live-execution-id "e-0"}}})
        loaded (await (jvm/load-session send "s-1" "(def a 1)" "a.cg" "/w/proj"))]
    (is (= "a" (aget (aget (:ok loaded) 0) "name")))
    (is (= ["a.cg"] (vec (:files loaded))))
    (is (= "s-1" (aget (aget (aget calls 0) "headers") "X-Session-Id")))
    (is (= "(def a 1)" (aget (aget (aget calls 0) "body") "code")))
    (is (= "/w/proj" (aget (aget (aget calls 0) "body") "root")))
    (is (= {:expired true} (await (jvm/start-repl send "s-1"))))
    (is (= {:error "busy (live execution: e-0)"}
           (await (jvm/start-execution send "s-1" #js {:code "x"}))))))

(deftest ^:async a-run-gives-its-id-then-its-result
  (let [send (stub-send #js [] {"POST /api/execution/start" (ok #js {:execution-id "e-1"})
                                "GET /api/execution/e-1/result"
                                (ok #js {:status "complete" :result #js {:count 2}})})]
    (is (= {:id "e-1"} (await (jvm/start-execution send "s-1" #js {:code "x"}))))
    (is (= 2 (aget (await (jvm/execution-result send "e-1")) "count")))))

(deftest ^:async the-datasets-of-a-session
  (let [calls #js []
        send (stub-send calls {"GET /api/session/vars"
                               (ok #js {:vars #js [#js {:name "parcels" :type "dataset"}
                                                   #js {:name "m" :type "map"}]})
                               "POST /api/data/parcels" (ok #js {:type "FeatureCollection"})})]
    (is (= ["parcels"] (vec (:names (await (jvm/dataset-var-names send "s-1"))))))
    (is (= "FeatureCollection"
           (aget (:fc (await (jvm/fetch-dataset send "s-1" "parcels" {:bbox #js [0 0 1 1] :limit 10})))
                 "type")))
    (is (= 10 (aget (aget (aget calls 1) "body") "limit")))
    (is (= [0 0 1 1] (vec (aget (aget (aget calls 1) "body") "bbox"))))))

(deftest ^:async with-session-creates-a-session-then-reuses-it
  (let [!sid (atom nil)
        calls #js []
        send (stub-send calls {"POST /api/session/create" (ok #js {:session-id "s-1"})})]
    (is (= "s-1" (await (jvm/with-session send !sid (fn [sid] (js/Promise.resolve sid))))))
    (is (= "s-1" (await (jvm/with-session send !sid (fn [sid] (js/Promise.resolve sid))))))
    (is (= 1 (.-length calls)))))

(deftest ^:async with-session-tries-again-in-a-new-session-on-expiry
  (let [!sid (atom "old")
        seen #js []
        send (stub-send #js [] {"POST /api/session/create" (ok #js {:session-id "new"})})
        r (await (jvm/with-session send !sid
                   (fn [sid]
                     (.push seen sid)
                     (js/Promise.resolve (if (= sid "old") {:expired true} {:port 7})))))]
    (is (= ["old" "new"] (vec seen)))
    (is (= {:port 7} r))
    (is (= "new" @!sid))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
