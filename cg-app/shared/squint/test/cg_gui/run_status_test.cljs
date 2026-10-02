;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run-status-test
  "The run status: the reducer over the run messages of the bus, and the text
   that the status bar shows for each state."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../src/cg_gui/run_status.mjs" :as rs]))

(defn- trace [evt] #js {:type "cg/trace-event" :event evt})

(defn- stage
  ([label state] (trace #js {:kind "stage" :label label :state state}))
  ([label state n total] (trace #js {:kind "stage" :label label :state state :n n :total total})))

(defn- proc [kind pid] (trace #js {:kind kind :pid pid}))

(defn- fold
  "The state after each of `msgs`, one second apart, from `run` at `t0`."
  [run t0 msgs]
  (.reduce msgs
           (fn [acc m i] (rs/run-reducer acc m (+ t0 (* 1000 (inc i)))))
           run))

(def ^:private chain-pids
  #js ["analyze-parcels-body__prepare-parcel-1"
       "analyze-parcels-body__preliminary-checks-2"
       "analyze-parcels-body__determine-effective-boundary-3"
       "analyze-parcels-body__analyze-parcel-4"
       "analyze-parcels-body__get-primary-zoning-info-5"
       "analyze-parcels-body__analyze-parcel-conformity-6"
       "analyze-parcels-body__analyze-parcel-impervious-7"])

(defn- started [] (rs/run-reducer rs/idle-run #js {:type "cg/run-started"} 1000))

(deftest step-label-drops-the-model-and-the-number
  (is (= "analyze-parcel" (rs/step-label "analyze-parcels-body__analyze-parcel-3")))
  (is (= "transform-batch" (rs/step-label "anon__transform-batch-12")))
  (is (= "buffer" (rs/step-label "buffer-1"))))

(deftest a-run-starts-with-a-clock
  (let [run (started)
        v (rs/run-view run {:status "running" :runtime "js"} 8000)]
    (is (= "running" (:phase run)))
    (is (= "running" (.-kind v)))
    (is (= "Running" (.-title v)))
    (is (= "0:07" (.-clock v)))))

(deftest a-trace-event-with-no-run-changes-nothing
  (let [run (rs/run-reducer rs/idle-run (proc "proc-start" "m__buffer-1") 5)]
    (is (= "idle" (:phase run)))
    (is (nil? (rs/run-view run nil 5)))))

(deftest a-stage-before-the-run-message-starts-the-clock
  (let [early (rs/run-reducer rs/idle-run (stage "start worker pool" "start") 1000)
        run (fold early 1000 #js [(stage "start worker pool" "done")
                                  #js {:type "cg/run-started"}
                                  (stage "fetch parcels" "start")])
        v (rs/run-view run {:status "running"} 9000)]
    (is (= "Start worker pool" (.-title (rs/run-view early {:status "running"} 3000))))
    (is (= "0:02" (.-clock (rs/run-view early {:status "running"} 3000))))
    (is (= 1000 (:started run)) "cg/run-started keeps the clock of the early stage")
    (is (= "Fetch parcels" (.-title v)))
    (is (= "0:08" (.-clock v)))))

(deftest a-late-stage-end-starts-no-run
  (let [done (rs/run-reducer (started) #js {:type "cg/run-ended" :outcome "success" :count 1} 2000)
        late (rs/run-reducer done (stage "fetch parcels" "done") 2500)]
    (is (= "ok" (:phase late)))))

(deftest a-stage-shows-its-label-and-its-count
  (let [run (fold (started) 1000 #js [(stage "fetch FY26_Parcels_Detailed" "start")
                                      (stage "fetch FY26_Parcels_Detailed" "progress" 2000 6334)])
        v (rs/run-view run nil 4000)]
    (is (= "Fetch FY26_Parcels_Detailed" (.-title v)))
    (is (= 2000 (.-n v)))
    (is (= 6334 (.-total v)))
    (is (= "2,000 of 6,334" (.-detail v)))))

(deftest overlapping-stages-take-turns-in-the-order-of-their-starts
  (let [run (fold (started) 1000 #js [(stage "fetch parcels" "start")
                                      (stage "fetch zoning" "start")
                                      (stage "fetch buildings" "start")
                                      (trace #js {:kind "stage" :label "fetch parcels"
                                                  :state "progress" :n 2000})])
        v (rs/run-view run nil 6000)
        v2 (rs/run-view (rs/show-next run) nil 6000)
        run3 (fold run 6000 #js [(stage "fetch buildings" "done")
                                 (stage "fetch parcels" "done")])
        v3 (rs/run-view run3 nil 9000)]
    (is (= "Fetch parcels" (.-title v)) "a progress event keeps the place of its stage")
    (is (= "(1/3)" (.-position v)))
    (is (= "2,000 so far" (.-detail v)))
    (is (nil? (.-total v)) "no count, no progress bar")
    (is (= "Fetch zoning" (.-title v2)))
    (is (= "(2/3)" (.-position v2)))
    (is (= "Fetch zoning" (.-title v3)))
    (is (nil? (.-position v3)) "one item has no marker")
    (is (nil? (.-detail v3)))
    (is (= 2 (:stages-done run3)))))

(deftest a-step-shows-its-place-in-the-flow
  (let [run (fold (started) 1000
                  #js [(trace #js {:kind "flow-start" :flow "analyze-parcels-body" :pids chain-pids})
                       (proc "proc-start" (aget chain-pids 0))
                       (proc "proc-complete" (aget chain-pids 0))
                       (proc "proc-start" (aget chain-pids 3))])
        v (rs/run-view run nil 9000)]
    (is (= "Step 4 of 7: analyze-parcel" (.-title v)))
    (is (nil? (.-detail v)))))

(deftest the-second-pass-of-a-step-is-the-second-batch
  (let [run (fold (started) 1000
                  #js [(trace #js {:kind "flow-start" :flow "analyze-parcels-body" :pids chain-pids})
                       (proc "proc-start" (aget chain-pids 0))
                       (proc "proc-complete" (aget chain-pids 0))
                       (proc "proc-start" (aget chain-pids 1))
                       (proc "proc-complete" (aget chain-pids 1))
                       (proc "proc-start" (aget chain-pids 0))])
        v (rs/run-view run nil 9000)]
    (is (= "Step 1 of 7: prepare-parcel, batch 2" (.-title v)))))

(deftest a-stage-and-a-step-with-rows-take-turns
  (let [run (fold (started) 1000
                  #js [(stage "analyze 6334 parcels" "progress" 2 7)
                       (trace #js {:kind "flow-start" :flow "analyze-parcels-body" :pids chain-pids})
                       (proc "proc-start" (aget chain-pids 6))
                       (trace #js {:kind "rows-progress" :pid (aget chain-pids 6) :done 250 :total 1000})])
        v (rs/run-view run nil 9000)
        v2 (rs/run-view (rs/show-next run) nil 9000)]
    (is (= "Analyze 6334 parcels" (.-title v)))
    (is (= "2 of 7" (.-detail v)))
    (is (= 2 (.-n v)))
    (is (= 7 (.-total v)))
    (is (= "(1/2)" (.-position v)))
    (is (= "Step 7 of 7: analyze-parcel-impervious" (.-title v2)))
    (is (= "250 of 1,000 rows" (.-detail v2)))
    (is (= 250 (.-n v2)))
    (is (= 1000 (.-total v2)))
    (is (= "(2/2)" (.-position v2)))))

(deftest a-step-with-no-rows-shows-only-when-nothing-else-runs
  (let [run (fold (started) 1000
                  #js [(trace #js {:kind "flow-start" :flow "anon" :pids #js ["anon__transform-batch-1"]})
                       (stage "reproject to EPSG:2249" "start" 0 20026)
                       (proc "proc-start" "anon__transform-batch-1")])
        ended (fold run 4000 #js [(stage "reproject to EPSG:2249" "done" 20026 20026)])]
    (is (= 1 (.-length (rs/run-items run))))
    (is (= "Reproject to EPSG:2249" (.-title (rs/run-view run nil 5000))))
    (is (= "Step 1 of 1: transform-batch" (.-title (rs/run-view ended nil 5000))))))

(defn- parallel-run
  "Two reprojections with one label, and a step of the row chain that counts
   rows. The first reprojection reports last."
  []
  (fold (started) 1000
        #js [(stage "reproject to EPSG:2249" "start" 0 20026)
             (stage "reproject to EPSG:2249" "start" 0 30287)
             (stage "reproject to EPSG:2249" "progress" 4000 30287)
             (trace #js {:kind "flow-start" :flow "analyze-parcels-body" :pids chain-pids})
             (proc "proc-start" (aget chain-pids 0))
             (trace #js {:kind "rows-progress" :pid (aget chain-pids 0) :done 250 :total 1000})
             (stage "reproject to EPSG:2249" "progress" 9000 20026)]))

(defn- shown
  "What the status bar shows: [title bar-value bar-total detail marker]."
  [run]
  (let [v (rs/run-view run nil 20000)]
    [(.-title v) (.-n v) (.-total v) (.-detail v) (.-position v)]))

(defn- turns
  "`run` and the runs after each of `n` turns."
  [run n]
  (.reduce (.from js/Array #js {:length n})
           (fn [acc _] (.concat acc #js [(rs/show-next (aget acc (dec (.-length acc))))]))
           #js [run]))

(deftest the-title-the-bar-and-the-marker-name-one-item
  (let [ts (turns (parallel-run) 3)]
    (is (= ["Reproject to EPSG:2249" 9000 20026 "9,000 of 20,026" "(1/3)"] (shown (aget ts 0))))
    (is (= ["Reproject to EPSG:2249" 4000 30287 "4,000 of 30,287" "(2/3)"] (shown (aget ts 1))))
    (is (= ["Step 1 of 7: prepare-parcel" 250 1000 "250 of 1,000 rows" "(3/3)"] (shown (aget ts 2))))
    (is (= (shown (aget ts 0)) (shown (aget ts 3))) "the turn after the last is the first")
    (is (rs/taking-turns? (parallel-run)))
    (is (not (rs/taking-turns? (rs/run-reducer (parallel-run)
                                               #js {:type "cg/run-ended" :outcome "success" :count 1}
                                               30000)))
        "the end of the run stops the turns")))

(deftest the-shown-item-keeps-its-turn-when-items-come-and-go
  (let [on-step (aget (turns (parallel-run) 2) 2)
        more (fold on-step 20000 #js [(stage "fetch Impervious_Surface" "start")])
        gone (fold more 21000 #js [(proc "proc-complete" (aget chain-pids 0))])]
    (is (= ["Step 1 of 7: prepare-parcel" 250 1000 "250 of 1,000 rows" "(4/4)"] (shown more))
        "a new stage goes before the steps, and the step still shows")
    (is (= ["Reproject to EPSG:2249" 9000 20026 "9,000 of 20,026" "(1/3)"] (shown gone))
        "the first item shows when the shown item ends")
    (is (= ["Reproject to EPSG:2249" 4000 30287 "4,000 of 30,287" "(2/3)"]
           (shown (rs/show-next gone))))))

(deftest the-rows-of-a-step-sit-beside-the-name-of-that-step
  (let [run (fold (started) 1000
                  #js [(trace #js {:kind "flow-start" :flow "analyze-parcels-body" :pids chain-pids})
                       (proc "proc-start" (aget chain-pids 0))
                       (proc "proc-start" (aget chain-pids 2))
                       (trace #js {:kind "rows-progress" :pid (aget chain-pids 0) :done 250 :total 1000})])
        both (fold run 5000 #js [(trace #js {:kind "rows-progress" :pid (aget chain-pids 2)
                                             :done 100 :total 500})])]
    (is (= ["Step 1 of 7: prepare-parcel" 250 1000 "250 of 1,000 rows" nil] (shown run))
        "the step that started last counts no rows, and the bar stays with its own step")
    (is (= ["Step 1 of 7: prepare-parcel" 250 1000 "250 of 1,000 rows" "(1/2)"] (shown both)))
    (is (= ["Step 3 of 7: determine-effective-boundary" 100 500 "100 of 500 rows" "(2/2)"]
           (shown (rs/show-next both))))))

(deftest a-reprojection-that-ends-closes-only-its-own-stage
  (let [run (fold (parallel-run) 20000 #js [(stage "reproject to EPSG:2249" "done" 20026 20026)])]
    (is (= ["Reproject to EPSG:2249" 4000 30287 "4,000 of 30,287" "(1/2)"] (shown run)))))

(deftest one-item-takes-no-turns
  (let [run (fold (started) 1000 #js [(stage "fetch parcels" "progress" 2000 6334)])]
    (is (not (rs/taking-turns? run)))
    (is (nil? (.-position (rs/run-view run nil 3000))))
    (is (= "Fetch parcels" (.-title (rs/run-view (rs/show-next run) nil 3000))))))

(deftest rows-give-the-bar-when-no-stage-has-a-count
  (let [run (fold (started) 1000
                  #js [(proc "proc-start" "m__buffer-1")
                       (trace #js {:kind "rows-progress" :pid "m__buffer-1" :done 30 :total 120})])
        v (rs/run-view run nil 3000)]
    (is (= "Step 1 of 1: buffer" (.-title v)))
    (is (= "30 of 120 rows" (.-detail v)))
    (is (= 30 (.-n v)))
    (is (= 120 (.-total v)))))

(deftest a-flow-end-clears-its-step
  (let [run (fold (started) 1000
                  #js [(trace #js {:kind "flow-start" :flow "anon" :pids #js ["anon__transform-batch-1"]})
                       (proc "proc-start" "anon__transform-batch-1")
                       (trace #js {:kind "flow-complete" :flow "anon"})])]
    (is (zero? (.-length (rs/run-items run))))
    (is (= 1 (.-length (:history run))))
    (is (= "Running" (.-title (rs/run-view run nil 5000))))))

(deftest a-stage-count-with-no-total-reads-so-far
  (let [run (fold (started) 1000 #js [(trace #js {:kind "stage" :label "fetch Boston_Buildings"
                                                  :state "progress" :n 4000})])
        v (rs/run-view run nil 3000)]
    (is (= "4,000 so far" (.-detail v)))
    (is (nil? (.-total v)))))

(defn- chain-run
  "A run in the second batch of the row chain, at step 4."
  []
  (fold (started) 1000
        (.concat #js [(trace #js {:kind "flow-start" :flow "analyze-parcels-body" :pids chain-pids})]
                 (.flatMap chain-pids
                           (fn [pid] #js [(proc "proc-start" pid)
                                          (trace #js {:kind "proc-complete" :pid pid :ms 2000})]))
                 (.flatMap (.slice chain-pids 0 3)
                           (fn [pid] #js [(proc "proc-start" pid)
                                          (trace #js {:kind "proc-complete" :pid pid :ms 2000})]))
                 #js [(trace #js {:kind "proc-start" :pid (aget chain-pids 3) :ts 100000})
                      (trace #js {:kind "rows-progress" :pid (aget chain-pids 3) :done 250 :total 1000})
                      (trace #js {:kind "flow-snapshot" :flow "analyze-parcels-body"
                                  :procs #js [#js {:pid "analyze-parcels-body__source" :status "running"
                                                   :count 7 :in-queued 0 :out-queued 0}
                                              #js {:pid (aget chain-pids 3) :status "running"
                                                   :count 1 :in-queued 2 :out-queued 0}
                                              #js {:pid (aget chain-pids 4) :status "paused"
                                                   :count 1 :in-queued 0 :out-queued 0}]})])))

(deftest the-steps-of-the-running-flow-have-one-row-each
  (let [rows (rs/step-rows (chain-run) 105000)
        r1 (aget rows 0)
        r4 (aget rows 3)
        r7 (aget rows 6)]
    (is (= 7 (.-length rows)) "the source of the snapshot is no step")
    (is (= "prepare-parcel" (.-label r1)))
    (is (= "done" (.-state r1)))
    (is (= "batch 2" (.-count r1)))
    (is (= "4.0 s" (.-time r1)))
    (is (= "analyze-parcel" (.-label r4)))
    (is (= "running" (.-state r4)))
    (is (= "batch 2" (.-count r4)))
    (is (= "250 of 1,000 rows" (.-rows r4)))
    (is (= 250 (.-done r4)))
    (is (= 1000 (.-total r4)))
    (is (= "2 queued" (.-queued r4)))
    (is (= "7.0 s" (.-time r4)) "2 s of batch 1, and 5 s since the event stamp of batch 2")
    (is (= "done" (.-state r7)))
    (is (nil? (.-count r7)))
    (is (nil? (.-queued r7)))
    (is (= "analyze-parcels-body" (rs/current-flow-label (chain-run))))))

(deftest steps-with-one-op-name-share-a-row
  (let [pids (.map #js [1 2 3 4] (fn [i] (str "anon__transform-batch-" i)))
        run (fold (started) 1000
                  #js [(trace #js {:kind "flow-start" :flow "anon" :pids pids})
                       (proc "proc-start" (aget pids 0))
                       (proc "proc-complete" (aget pids 0))
                       (proc "proc-start" (aget pids 1))])
        rows (rs/step-rows run 9000)]
    (is (= 1 (.-length rows)))
    (is (= "transform-batch" (.-label (aget rows 0))))
    (is (= "running" (.-state (aget rows 0))))
    (is (= "1 of 4 done" (.-count (aget rows 0))))
    (is (= "transform-batch" (rs/current-flow-label run)))))

(deftest a-flow-with-no-name-takes-the-label-of-its-stage
  (let [run (fold (started) 1000
                  #js [(trace #js {:kind "flow-start" :flow "anon" :pids #js ["anon__transform-batch-1"]})
                       (stage "reproject to EPSG:2249" "start")
                       (proc "proc-start" "anon__transform-batch-1")])
        ended (fold run 9000 #js [(trace #js {:kind "flow-complete" :flow "anon" :ms 4000})])]
    (is (= "reproject to EPSG:2249" (rs/current-flow-label run)))
    (is (= "reproject to EPSG:2249" (.-label (aget (rs/flow-rows ended) 0))))))

(deftest a-finished-flow-goes-to-the-history
  (let [run (fold (chain-run) 200000
                  #js [(trace #js {:kind "flow-complete" :flow "analyze-parcels-body" :ms 441000})
                       (trace #js {:kind "flow-start" :flow "anon" :pids #js ["anon__transform-batch-1"]})])
        rows (rs/flow-rows run)]
    (is (= 1 (.-length rows)))
    (is (= "analyze-parcels-body" (.-label (aget rows 0))))
    (is (= "done" (.-state (aget rows 0))))
    (is (= "7 steps · 11 runs" (.-detail (aget rows 0))))
    (is (= "7:21" (.-time (aget rows 0))))
    (is (= 1 (.-length (rs/step-rows run 0))) "the steps are those of the next flow")))

(deftest a-run-error-closes-the-open-flow-as-failed
  (let [run (rs/run-reducer (chain-run) #js {:type "cg/run-error" :message "x"} 300000)
        rows (rs/flow-rows run)]
    (is (= "error" (.-state (aget rows 0))))
    (is (= 7 (.-length (rs/step-rows run 0))) "the steps stay, to show where it failed")
    (is (= "error" (.-state (aget (rs/step-rows run 0) 3))) "the step that ran failed")
    (is (= "analyze-parcels-body" (rs/current-flow-label run)))))

(defn- busy [kind id handler label item pid]
  (trace #js {:kind kind :worker-id id :handler handler :label label :item-id item :pid pid}))

(deftest each-worker-shows-its-work
  (let [run (fold (started) 1000
                  #js [(busy "worker-busy-start" "0" "gdal" "parse page 2 of FY26_Parcels_Detailed" "a" nil)
                       (busy "worker-busy-start" "main" "main" "analyze-parcel: 1000 rows" "b"
                             "analyze-parcels-body__analyze-parcel-4")
                       (busy "worker-busy-start" "2" "wasmts" "reproject 1000 features to EPSG:2249" "c"
                             "anon__transform-batch-3")
                       (busy "worker-busy-end" "2" "wasmts" "reproject 1000 features to EPSG:2249" "c"
                             "anon__transform-batch-3")])
        cells (rs/worker-cells run)
        main (aget cells 0)
        w0 (aget cells 1)
        w2 (aget cells 2)]
    (is (= 3 (.-length cells)))
    (is (= "Main thread" (.-name main)))
    (is (= "busy" (.-state main)))
    (is (= "analyze-parcel: 1000 rows" (.-label main)))
    (is (= "analyze-parcel" (.-step main)))
    (is (= "Worker 0" (.-name w0)))
    (is (= "gdal" (.-handler w0)))
    (is (= "busy" (.-state w0)))
    (is (nil? (.-step w0)))
    (is (= "Worker 2" (.-name w2)))
    (is (= "idle" (.-state w2)))
    (is (= "wasmts" (.-handler w2)))
    (is (= 1 (.-jobs w2)))))

(deftest a-late-end-does-not-idle-a-worker-with-new-work
  (let [run (fold (started) 1000
                  #js [(busy "worker-busy-start" "1" "wasmts" "first" "a" nil)
                       (busy "worker-busy-start" "1" "wasmts" "second" "b" nil)
                       (busy "worker-busy-end" "1" "wasmts" "first" "a" nil)])
        w (aget (rs/worker-cells run) 0)]
    (is (= "busy" (.-state w)))
    (is (= "second" (.-label w)))))

(deftest the-end-of-the-run-idles-each-worker
  (let [run (fold (started) 1000
                  #js [(busy "worker-busy-start" "1" "wasmts" "first" "a" nil)
                       #js {:type "cg/run-ended" :outcome "success" :count 1}])]
    (is (= "idle" (.-state (aget (rs/worker-cells run) 0))))))

(deftest a-run-ends-with-its-time-and-its-count
  (let [run (rs/run-reducer (started) #js {:type "cg/run-ended" :outcome "success" :count 3} 1400)
        v (rs/run-view run {:status "ok" :runtime "js"} 99999)]
    (is (= "ok" (:phase run)))
    (is (= "ok" (.-kind v)))
    (is (= "Finished in 0.4 s" (.-title v)))
    (is (= "3 results · ran in this browser (js)" (.-detail v)))))

(deftest a-long-run-ends-with-a-clock-time
  (let [run (rs/run-reducer (started) #js {:type "cg/run-ended" :outcome "success" :count 1} 392000)
        v (rs/run-view run {:status "ok" :runtime "js"} 0)]
    (is (= "Finished in 6:31" (.-title v)))
    (is (= "1 result · ran in this browser (js)" (.-detail v)))))

(deftest a-failed-run-names-the-step-and-the-message
  (let [run (fold (started) 1000
                  #js [(trace #js {:kind "flow-start" :flow "m" :pids #js ["m__buffer-1"]})
                       (proc "proc-start" "m__buffer-1")
                       (trace #js {:kind "proc-error" :pid "m__buffer-1" :message "bad geometry"})
                       (trace #js {:kind "flow-error" :flow "m" :message "flow failed"})
                       #js {:type "cg/run-error" :message "bad geometry in row 4"}])
        v (rs/run-view run {:status "error" :runtime "js"} 0)]
    (is (= "error" (:phase run)))
    (is (= "error" (.-kind v)))
    (is (= "Run failed after 5.0 s, in step buffer" (.-title v)))
    (is (= "bad geometry in row 4" (.-detail v)))))

(deftest the-exec-state-covers-what-the-bus-does-not-carry
  (let [refused (rs/run-view rs/idle-run {:status "refused" :message "backend unreachable"} 0)
        early (rs/run-view rs/idle-run {:status "running" :runtime "js"} 0)
        failed (rs/run-view rs/idle-run {:status "error" :message "cg module not loaded yet"} 0)]
    (is (= "refused" (.-kind refused)))
    (is (= "backend unreachable" (.-detail refused)))
    (is (= "running" (.-kind early)))
    (is (= "Starting the engine" (.-title early)))
    (is (= "error" (.-kind failed)))
    (is (= "cg module not loaded yet" (.-detail failed)))))

(deftest the-exec-end-state-wins-over-a-late-bus-message
  (let [v (rs/run-view (started) {:status "ok" :runtime "js" :message "3 result(s)"} 1400)]
    (is (= "ok" (.-kind v)))
    (is (= "Finished in 0.4 s" (.-title v)))
    (is (= "3 result(s) · ran in this browser (js)" (.-detail v)))))

(deftest a-new-run-drops-the-state-of-the-run-before
  (let [done (rs/run-reducer (started) #js {:type "cg/run-error" :message "x"} 2000)
        again (rs/run-reducer done #js {:type "cg/run-started"} 5000)]
    (is (= "running" (:phase again)))
    (is (nil? (:message again)))
    (is (= 5000 (:started again)))))

(deftest clock-and-duration-text
  (is (= "0:00" (rs/clock-text 0)))
  (is (= "2:07" (rs/clock-text 127900)))
  (is (= "1:02:03" (rs/clock-text 3723000)))
  (is (= "9.9 s" (rs/duration-text 9949)))
  (is (= "0:10" (rs/duration-text 10000))))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
