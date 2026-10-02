;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.session-test
  "The session: each probe result feeds runtime resolution, and a URL change
   probes the new URL at once."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/session.mjs" :as session]
            ["../../../src/cg_app/ui/runtime.mjs" :as rt]
            ["../../../src/cg_app/ui/state.mjs" :as state]))

(defn- sleep [ms] (js/Promise. (fn [res _] (js/setTimeout res ms))))

(def !urls (atom []))
(def !calls (atom 0))

(defn- mock-fetch!
  "Install a fetch stub. It reads ok?-atom on each call, which lets a test flip it."
  [ok?-atom]
  (set! (.-fetch js/globalThis)
        (fn [url _opts]
          (swap! !calls inc)
          (swap! !urls conj (str url))
          (if @ok?-atom
            (js/Promise.resolve #js {:ok true})
            (js/Promise.reject (js/Error. "ECONNREFUSED"))))))

(deftest ^:async probe-feeds-re-resolution-and-observes-down-transition
  (when-let [ls (.-localStorage js/globalThis)]
    (.removeItem ls "cg-runtime")
    (.removeItem ls "cg-backend-url"))
  (rt/init!)
  (let [ok? (atom true)]
    (mock-fetch! ok?)
    (rt/set-choice! "auto" false)
    (is (= "js" (rt/resolved-runtime)))
    (is (true? (await (session/probe-now!))))
    (is (= "jvm" (rt/resolved-runtime)))
    (is (true? (:backend-available @state/app-state)))
    (reset! ok? false)
    (is (false? (await (session/probe-now!))))
    (is (= "js" (rt/resolved-runtime)))
    (is (false? (:backend-available @state/app-state)))
    (rt/set-choice! "jvm" false)
    (await (session/probe-now!))
    (is (= "jvm" (rt/resolved-runtime)))))

(deftest ^:async health-loop-probes-every-interval
  (let [ok? (atom true)]
    (mock-fetch! ok?)
    (reset! !calls 0)
    (session/start-health-loop! 10)
    (await (sleep 60))
    (session/stop-health-loop!)
    (let [n @!calls]
      (is (>= n 3))
      (await (sleep 30))
      (is (= n @!calls)))))

(deftest ^:async set-backend-url-probes-immediately-at-new-url
  (let [ok? (atom false)]
    (mock-fetch! ok?)
    (reset! !urls [])
    (await (session/set-backend-url! "http://localhost:9999"))
    (is (= "http://localhost:9999" (rt/backend-url)))
    (is (= "http://localhost:9999" (:backend-url @state/app-state)))
    (is (pos? (count @!urls)))
    (is (= 0 (.indexOf (str (last @!urls)) "http://localhost:9999")))
    ;; the test files that run later need the default
    (await (session/set-backend-url! nil))))

(deftest ^:async set-runtime-auto-probes-immediately
  (let [ok? (atom true)]
    (mock-fetch! ok?)
    (reset! !calls 0)
    (await (session/set-runtime! "auto"))
    (is (pos? @!calls))
    (is (= "jvm" (rt/resolved-runtime)))
    (is (= "auto" (:runtime-choice @state/app-state)))
    (await (session/set-runtime! "js"))
    (is (= "js" (rt/resolved-runtime)))))

(def !reqs (atom []))

(defn- mock-json-fetch!
  "Fetch stub recording [url opts] and answering from a route->body map
   (JS object keyed by 'METHOD path-suffix'); unmatched routes 500."
  [routes]
  (set! (.-fetch js/globalThis)
        (fn [url opts]
          (swap! !reqs conj [(str url) opts])
          (let [hit (.find (js/Object.keys routes)
                           (fn [r]
                             (let [sp (.indexOf r " ")]
                               (and (= (subs r 0 sp) (aget opts "method"))
                                    (.endsWith (str url) (subs r (inc sp)))))))]
            (if hit
              (js/Promise.resolve
               #js {:ok true
                    :json (fn [] (js/Promise.resolve (aget routes hit)))})
              (js/Promise.resolve #js {:ok false :status 500}))))))

(deftest ^:async ensure-session-creates-once
  (reset! session/!session-id nil)
  (reset! !reqs [])
  (mock-json-fetch! #js {"POST /api/session/create" #js {:session-id "s-1"}})
  (is (= "s-1" (await (session/ensure-session!))))
  (is (= "s-1" (await (session/ensure-session!))))
  (is (= 1 (count @!reqs))))

(deftest ^:async load-session-code-lands-vars-with-header
  (reset! session/!session-id "s-1")
  (reset! !reqs [])
  (state/set-session-vars! #js [])
  (mock-json-fetch!
   #js {"POST /api/session/load"
        #js {:status "ok"
             :vars #js [#js {:name "parcels" :type "dataset" :row-count 12}]}})
  (await (session/load-session-code! "(def parcels 1)" "a.cg"))
  (let [[url opts] (first @!reqs)
        body (js/JSON.parse (aget opts "body"))]
    (is (.endsWith url "/api/session/load"))
    (is (= "s-1" (aget (aget opts "headers") "X-Session-Id")))
    (is (= "(def parcels 1)" (aget body "code")))
    (is (= "a.cg" (aget body "filename"))))
  (is (= 1 (.-length (:session-vars @state/app-state))))
  (is (false? (:session-loading? @state/app-state))))

(deftest ^:async load-failure-leaves-vars-and-clears-loading
  (reset! session/!session-id "s-1")
  (mock-json-fetch! #js {})
  (state/set-session-vars! #js [#js {:name "keepme" :type "dataset"}])
  (await (session/load-session-code! "(boom)" "b.cg"))
  (is (= "keepme" (aget (aget (:session-vars @state/app-state) 0) "name")))
  (is (false? (:session-loading? @state/app-state))))

(deftest ^:async fetch-var-data-posts-limit
  (reset! session/!session-id "s-1")
  (reset! !reqs [])
  (mock-json-fetch!
   #js {"POST /api/data/parcels" #js {:type "FeatureCollection" :features #js []}})
  (let [fc (await (session/fetch-var-data! "parcels"))]
    (is (= "FeatureCollection" (aget fc "type"))))
  (let [[url opts] (first @!reqs)]
    (is (.endsWith url "/api/data/parcels"))
    (is (= 5000 (aget (js/JSON.parse (aget opts "body")) "limit")))))

(deftest ^:async js-only-start-never-probes
  (let [ok? (atom true)]
    (mock-fetch! ok?)
    (reset! !calls 0)
    (set! (.-__CG_JS_ONLY__ js/globalThis) true)
    (rt/init!)
    (await (session/start! 10))
    (await (sleep 60))
    (session/stop-health-loop!)
    (js-delete js/globalThis "__CG_JS_ONLY__")
    (rt/init!)
    (is (zero? @!calls) "a JS-only page never fetches the backend")))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
