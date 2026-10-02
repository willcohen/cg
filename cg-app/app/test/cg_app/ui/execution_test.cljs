;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.execution-test
  "The execution router. The JVM backend streams the SSE trace to the canvas
   bus, the JS backend runs the model in process, and explicit jvm with no backend refuses."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/execution.mjs" :as ex]
            ["../../../../shared/squint/src/cg_gui/run/js.mjs" :as rc]
            ["../../../src/cg_app/ui/runtime.mjs" :as rt]
            ["../../../src/cg_app/ui/state.mjs" :as state]
            ["../../../src/cg_app/ui/loader.mjs" :as loader]))

(def flow-fixture
  #js {:bindings #js [#js {:name "parcels" :body-shape "function_call"}
                      #js {:name "buffered" :body-shape "threading_macro"
                           :ops #js [#js {:qualified-name "geo/buffer"}]}]})

(deftest model-binding-name-picks-last-threading-macro
  (is (= "buffered" (rc/model-binding-name flow-fixture)))
  (is (nil? (rc/model-binding-name #js {:bindings #js [#js {:name "x" :body-shape "function_call"}]})))
  (is (nil? (rc/model-binding-name #js {}))))

(deftest model-names-lists-the-threading-macro-bindings-in-order
  (is (= ["a" "b"]
         (vec (rc/model-names
               #js {:bindings #js [#js {:name "a" :body-shape "threading_macro"}
                                   #js {:name "x" :body-shape "function_call"}
                                   #js {:name "b" :body-shape "threading_macro"}]}))))
  (is (= [] (vec (rc/model-names #js {})))))

(defn- mock-module
  "Mocks the cg module surface that execute-js! uses, with a trace_js listener
   registry. __listener_count lets tests check that a listener is removed."
  [!runs run-result]
  (let [!listeners (atom [])
        emit-1 (fn [evt] (doseq [f @!listeners] (f evt)))]
    #js {:metadata #js {:split_code_and_metadata
                        (fn [t] #js {:code (str "STRIPPED:" t) :metadata #js {}})}
         :run #js {:run_file (fn [code opts]
                               (swap! !runs conj [code (aget opts "model")])
                               (let [pid (str (aget opts "model") "__buffer-1")]
                                 (emit-1 #js {:kind "proc-start" :pid pid
                                              :v 1 :seq 1 :ts 0 :worker-id "js"})
                                 (emit-1 #js {:kind "proc-complete" :pid pid :ms 1
                                              :v 1 :seq 2 :ts 0 :worker-id "js"})
                                 (js/Promise.resolve #js {:model (aget opts "model")
                                                          :result run-result})))}
         :trace_js #js {:add_trace_listener (fn [f] (swap! !listeners conj f) f)
                        :remove_trace_listener (fn [f]
                                                 (swap! !listeners
                                                        (fn [ls] (filterv (fn [g] (not (identical? g f))) ls)))
                                                 true)
                        :__listener_count (fn [] (count @!listeners))}}))

(deftest ^:async execute-js-runs-the-stripped-code
  (let [!runs (atom [])
        r (await (rc/execute-js! (mock-module !runs #js [1 2 3]) "TEXT" flow-fixture nil))]
    (is (= 3 (:count r)))
    (is (= [["STRIPPED:TEXT" "buffered"]] @!runs))))

(defn- stub-window! []
  (let [!posts (atom [])]
    (aset js/globalThis "window"
          #js {:postMessage (fn [msg _] (swap! !posts conj msg))})
    !posts))

(defn- install-mock-es!
  "new EventSource(url) captures itself at globalThis.__es; tests drive
   .onmessage/.onerror by hand and assert .closed."
  []
  (aset js/globalThis "__es" nil)
  (aset js/globalThis "EventSource"
        (js* "function(url){ this.url = url; this.closed = false; this.close = function(){ this.closed = true; }; globalThis.__es = this; }")))

(defn- sleep [ms] (js/Promise. (fn [res _] (js/setTimeout res ms))))

(defn ^:async wait-for-es
  "Poll until the mock EventSource captures itself, after two awaits in
   execute-jvm!. One arity only: a multi-arity ^:async fn is unsafe in squint."
  [i]
  (if-let [es (aget js/globalThis "__es")]
    es
    (if (> i 100)
      (throw (js/Error. "EventSource never opened"))
      (do (await (sleep 5))
          (await (wait-for-es (inc i)))))))

(defn- frame [m] #js {:data (js/JSON.stringify m)})
(defn- post-types [posts] (mapv (fn [m] (aget m "type")) posts))

(deftest ^:async execute-js-forwards-trace-events-with-named-pids
  (let [!seen (atom [])
        mod (mock-module (atom []) #js [1 2 3])
        r (await (rc/execute-js! mod "TEXT" flow-fixture
                                 (fn [evt] (swap! !seen conj evt))))]
    (is (= 3 (:count r)))
    (is (= ["proc-start" "proc-complete"] (mapv (fn [e] (aget e "kind")) @!seen)))
    (let [evt (first @!seen)]
      (is (= "buffered__buffer-1" (aget evt "pid")))
      (is (= "js" (aget evt "worker-id"))))
    (is (= 0 ((aget (aget mod "trace_js") "__listener_count"))))))

(deftest ^:async execute-jvm-streams-trace-to-canvas-bus
  (let [!posts (stub-window!)
        !req (atom nil)]
    (install-mock-es!)
    (set! (.-fetch js/globalThis)
          (fn [url opts]
            (reset! !req {:url (str url) :body (js/JSON.parse (aget opts "body"))})
            (js/Promise.resolve
             #js {:ok true :status 200
                  :json (fn [] (js/Promise.resolve #js {:execution-id "abc-123"}))})))
    (rt/set-backend-url! "http://localhost:3000")
    (let [p (ex/execute-jvm! "RAW TEXT" flow-fixture)
          es (await (wait-for-es 0))]
      (is (= "http://localhost:3000/api/execution/start" (:url @!req)))
      (is (= "RAW TEXT" (aget (:body @!req) "code")))
      (is (= "buffered" (aget (aget (:body @!req) "opts") "name")))
      (is (= "buffered" (aget (aget (:body @!req) "opts") "model"))
          "the backend runs the model the GUI names")
      (is (= "http://localhost:3000/api/execution/abc-123/trace" (aget es "url")))
      (is (= ["cg/run-started"] (post-types @!posts)))
      (is (= "abc-123" (aget (first @!posts) "execution-id")))
      ((aget es "onmessage") (frame #js {:kind "proc-start" :pid "buffered__buffer-1" :seq 2 :v 1}))
      ((aget es "onmessage") (frame #js {:kind "proc-complete" :pid "buffered__buffer-1" :seq 3 :ms 4}))
      ((aget es "onmessage") (frame #js {:kind "execution-ended" :outcome "success" :count 2}))
      (let [r (await p)]
        (is (= 2 (:count r))))
      (is (= true (aget es "closed")))
      (is (= ["cg/run-started" "cg/trace-event" "cg/trace-event" "cg/run-ended"]
             (post-types @!posts)))
      (let [evt (aget (nth @!posts 1) "event")]
        (is (= "proc-start" (aget evt "kind")))
        (is (= "buffered__buffer-1" (aget evt "pid"))))
      (is (= 2 (aget (nth @!posts 3) "count"))))))

(deftest ^:async execute-jvm-error-sentinel-rejects-and-posts-run-error
  (let [!posts (stub-window!)]
    (install-mock-es!)
    (set! (.-fetch js/globalThis)
          (fn [_ _] (js/Promise.resolve
                     #js {:ok true :status 200
                          :json (fn [] (js/Promise.resolve #js {:execution-id "e1"}))})))
    (let [p (ex/execute-jvm! "X" flow-fixture)
          es (await (wait-for-es 0))]
      ((aget es "onmessage") (frame #js {:kind "execution-ended" :outcome "error" :message "boom"}))
      (try (await p) (is false "should have thrown")
           (catch :default e (is (= "boom" (.-message e)))))
      (is (= true (aget es "closed")))
      (is (= ["cg/run-started" "cg/run-error"] (post-types @!posts))))))

(deftest ^:async execute-jvm-409-surfaces-without-touching-canvas
  (let [!posts (stub-window!)]
    (install-mock-es!)
    (set! (.-fetch js/globalThis)
          (fn [_ _] (js/Promise.resolve
                     #js {:ok false :status 409
                          :json (fn [] (js/Promise.resolve
                                        #js {:error "Another execution is already streaming; trace events are global, so executions serialize"
                                             :live-execution-id "live-1"}))})))
    (try (await (ex/execute-jvm! "X" flow-fixture)) (is false "should have thrown")
         (catch :default e
           (is (.includes (.-message e) "already streaming"))
           (is (.includes (.-message e) "live-1"))))
    (is (nil? (aget js/globalThis "__es")))
    (is (= [] (post-types @!posts)))))

(deftest ^:async execute-jvm-stream-break-rejects-loudly
  (let [!posts (stub-window!)]
    (install-mock-es!)
    (set! (.-fetch js/globalThis)
          (fn [_ _] (js/Promise.resolve
                     #js {:ok true :status 200
                          :json (fn [] (js/Promise.resolve #js {:execution-id "e2"}))})))
    (let [p (ex/execute-jvm! "X" flow-fixture)
          es (await (wait-for-es 0))]
      ((aget es "onmessage") (frame #js {:kind "proc-start" :pid "buffered__buffer-1" :seq 2}))
      ((aget es "onerror") #js {})
      (try (await p) (is false "should have thrown")
           (catch :default e (is (.includes (.-message e) "interrupted"))))
      (is (= true (aget es "closed")))
      (is (= ["cg/run-started" "cg/trace-event" "cg/run-error"] (post-types @!posts))))))

(deftest ^:async execute!-refuses-explicit-jvm-when-unreachable
  (when-let [ls (.-localStorage js/globalThis)] (.removeItem ls "cg-runtime"))
  (swap! state/app-state assoc
         :loaded-file {:name "f.cg" :text "T"} :flow flow-fixture
         :module-status "loaded" :backend-available false)
  (rt/set-choice! "jvm" false)
  (set! (.-fetch js/globalThis) (fn [_ _] (throw (js/Error. "must not fetch on refusal"))))
  (await (ex/execute!))
  (let [e (:exec @state/app-state)]
    (is (= "refused" (:status e)))
    (is (= "jvm" (:runtime e)))
    (is (string? (:message e)))))

(deftest ^:async execute!-js-backend-through-loader-module
  (aset js/globalThis "wasmts" #js {:geom #js {}})   ; satisfy ensure-wasmts!
  (let [!evals (atom [])
        !posts (stub-window!)]
    (reset! loader/cg-module (mock-module !evals #js [1 2]))
    (swap! state/app-state assoc
           :loaded-file {:name "f.cg" :text "T"} :flow flow-fixture
           :module-status "loaded" :backend-available false)
    (rt/set-choice! "js" false)
    (await (ex/execute!))
    (let [e (:exec @state/app-state)]
      (is (= "ok" (:status e)))
      (is (= "js" (:runtime e)))
      (is (= 2 (:count e))))
    (is (= ["cg/run-started" "cg/trace-event" "cg/trace-event" "cg/run-ended"]
           (post-types @!posts)))
    (is (= 2 (aget (nth @!posts 3) "count")))))

(deftest ^:async execute!-runs-the-selected-model
  (aset js/globalThis "wasmts" #js {:geom #js {}})
  (let [!runs (atom [])]
    (stub-window!)
    (reset! loader/cg-module (mock-module !runs #js [1]))
    (swap! state/app-state assoc
           :loaded-file {:name "f.cg" :text "T"} :flow flow-fixture
           :module-status "loaded" :backend-available false :run-model "other")
    (rt/set-choice! "js" false)
    (await (ex/execute!))
    (swap! state/app-state assoc :run-model nil)
    (is (= "other" (second (first @!runs))))))

(deftest ^:async execute-jvm-sends-the-model-it-is-given
  (stub-window!)
  (install-mock-es!)
  (let [!req (atom nil)]
    (set! (.-fetch js/globalThis)
          (fn [_url opts]
            (reset! !req (js/JSON.parse (aget opts "body")))
            (js/Promise.resolve
             #js {:ok true :status 200
                  :json (fn [] (js/Promise.resolve #js {:execution-id "m-1"}))})))
    (let [p (ex/execute-jvm! "RAW" flow-fixture "other")
          es (await (wait-for-es 0))]
      ((aget es "onmessage") (frame #js {:kind "execution-ended" :outcome "success" :count 1}))
      (await p)
      (is (= "other" (aget (aget @!req "opts") "model")))
      (is (= "other" (aget (aget @!req "opts") "name"))))))

(deftest ^:async execute!-routes-a-scalar-result-to-the-badge
  (aset js/globalThis "wasmts" #js {:geom #js {}})
  (let [!posts (stub-window!)]
    (reset! loader/cg-module (mock-module (atom []) 42))
    ;; a report from an earlier run must not survive under this one
    (state/set-report-result! #js {:title "stale" :columns #js [] :rows #js []})
    (swap! state/app-state assoc
           :loaded-file {:name "f.cg" :text "T"} :flow flow-fixture
           :module-status "loaded" :backend-available false)
    (rt/set-choice! "js" false)
    (await (ex/execute!))
    (is (= "ok" (:status (:exec @state/app-state))))
    (let [sc (:scalar-result @state/app-state)]
      (is (= "42" (aget sc "text")))
      (is (= "buffered" (aget sc "name"))))
    (is (nil? (:report-result @state/app-state)))
    (is (= ["cg/run-started" "cg/trace-event" "cg/trace-event" "cg/run-ended"]
           (post-types @!posts)))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
