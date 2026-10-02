;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.run-status-test
  "The run status of the extension: the run messages go to the canvas and
   through the shared reducer to the status bar item and the Run Status tab."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_vscode/run_status.mjs" :as run-status]
            ["../../src/cg_vscode/status_html.mjs" :as status-html]
            ["../../../cg-app/shared/squint/src/cg_gui/run_status.mjs" :as rs]))

(defn- fake-bar []
  (let [bar #js {:text "" :tooltip "" :shown false}]
    (aset bar "show" (fn [] (aset bar "shown" true)))
    (aset bar "hide" (fn [] (aset bar "shown" false)))
    bar))

(defn- fake-panel [posted]
  #js {:webview #js {:html "" :postMessage (fn [m] (.push posted m))}})

(defn- fresh-state [posted]
  (atom {:run-status/bar (fake-bar)
         :run-status/panel (fake-panel posted)
         :run-status/nonce "n0"
         :editor/panels {"file:///w/a.cg" (fake-panel posted)}}))

(defn- trace [kind pid] #js {:type "cg/trace-event" :event #js {:kind kind :pid pid}})

(deftest a-run-goes-to-the-canvas-and-ends-on-the-status-bar
  (let [posted #js []
        !state (fresh-state posted)]
    (run-status/post! !state "file:///w/a.cg" #js {:type "cg/run-started" :file "/w/a.cg"})
    (run-status/post! !state "file:///w/a.cg" (trace "flow-start" nil))
    (run-status/post! !state "file:///w/a.cg" (trace "proc-start" "drawn__map-layer-1"))
    (run-status/draw! !state (js/Date.now))
    (let [^js bar (:run-status/bar @!state)]
      (is (.startsWith (aget bar "text") "$(sync~spin) "))
      (is (true? (aget bar "shown"))))
    (run-status/post! !state "file:///w/a.cg" (trace "proc-complete" "drawn__map-layer-1"))
    (run-status/post! !state "file:///w/a.cg" #js {:type "cg/run-ended" :outcome "success" :count 2})
    (let [^js bar (:run-status/bar @!state)
          html (.. ^js (:run-status/panel @!state) -webview -html)]
      (is (= 5 (.-length posted)) "each message reaches the canvas")
      (is (.startsWith (aget bar "text") "$(check) Finished in"))
      (is (.includes (aget bar "tooltip") "2 results"))
      (is (.includes html "Finished in"))
      (is (.includes html "nonce-n0")))
    (run-status/stop! !state)))

(deftest a-failed-run-shows-the-failure
  (let [posted #js []
        !state (fresh-state posted)]
    (run-status/post! !state "file:///w/a.cg" #js {:type "cg/run-started"})
    (run-status/post! !state "file:///w/a.cg" #js {:type "cg/run-error" :message "boom"})
    (let [^js bar (:run-status/bar @!state)]
      (is (.startsWith (aget bar "text") "$(error) Run failed"))
      (is (= "boom" (aget bar "tooltip"))))
    (run-status/stop! !state)))

(deftest the-output-of-a-run-does-not-draw
  (let [posted #js []
        !state (fresh-state posted)
        ^js panel (:run-status/panel @!state)]
    (run-status/post! !state "file:///w/a.cg" #js {:type "cg/run-started"})
    (aset (.-webview panel) "html" "unchanged")
    (run-status/post! !state "file:///w/a.cg" #js {:type "cg/run-output" :stream "stdout" :text "x\n"})
    (is (= 2 (.-length posted)) "the output still reaches the canvas")
    (is (= "unchanged" (.. panel -webview -html)))
    (run-status/stop! !state)))

(deftest a-message-for-a-closed-canvas-still-counts
  (let [posted #js []
        !state (fresh-state posted)]
    (run-status/post! !state "file:///w/closed.cg" #js {:type "cg/run-started"})
    (run-status/post! !state "file:///w/closed.cg" #js {:type "cg/run-ended" :outcome "success" :count 1})
    (is (= 0 (.-length posted)))
    (is (= "ok" (:phase (:run-status/run @!state))))))

(deftest the-tab-lists-the-steps-and-the-workers
  (let [t0 1000
        run (reduce (fn [r [m i]] (rs/run-reducer r m (+ t0 (* 100 i))))
                    rs/idle-run
                    [[#js {:type "cg/run-started"} 0]
                     [(trace "flow-start" nil) 1]
                     [(trace "proc-start" "m__parse-row-1") 2]
                     [#js {:type "cg/trace-event"
                           :event #js {:kind "worker-busy-start" :worker-id "1" :handler "rows"
                                       :label "row 7" :pid "m__parse-row-1"}} 3]])
        html (status-html/body-html run 2000)]
    (is (.includes html "Steps"))
    (is (.includes html "parse-row"))
    (is (.includes html "Workers"))
    (is (.includes html "row 7"))))

(deftest the-tab-escapes-text
  (let [run (rs/run-reducer (rs/run-reducer rs/idle-run #js {:type "cg/run-started"} 0)
                            #js {:type "cg/run-error" :message "<script>x</script>"} 10)]
    (is (not (.includes (status-html/body-html run 20) "<script>")))))

(deftest an-idle-tab-says-that-nothing-runs
  (is (.includes (status-html/body-html rs/idle-run 0) "Nothing runs")))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err (or (get results "error") 0)]
           (js/process.exit (if (pos? (+ fail err)) 1 0)))))
