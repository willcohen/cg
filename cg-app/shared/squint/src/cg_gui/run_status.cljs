;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.run-status
  "The run status: run-reducer folds the run messages into one state, and the view
   fns give the text of the status bar and the Status panel. No React, no host.")

(def idle-run {:phase "idle"})

(defn step-label
  "The op name of a trace pid: `<model>__<op>-<n>` -> `<op>`."
  [pid]
  (let [s (str pid)
        i (.indexOf s "__")
        tail (if (>= i 0) (.slice s (+ i 2)) s)]
    (.replace tail #"-\d+$" "")))

(defn- event-time
  "The time of a trace event. The stamp of the event wins over the arrival
   time, because events arrive in bursts when the run holds the main thread."
  [evt now]
  (or (aget evt "ts") now))

(defn- stage-key [label total]
  (if (some? total) (str label " of " total) label))

(defn- stage-index
  "The place in `stages` of the stage that an event names, or -1. Two
   reprojections that run at the same time have one label, and their totals
   keep them apart. A start with a new total is a new stage. Another event
   with no stage of its total names the one open stage of its label."
  [stages label k start?]
  (let [exact (.findIndex stages (fn [s] (= k (aget s "key"))))
        labeled (.filter stages (fn [s] (= label (aget s "label"))))]
    (cond
      (>= exact 0) exact
      (and (not start?) (= 1 (.-length labeled))) (.indexOf stages (aget labeled 0))
      :else -1)))

(defn- on-stage
  "A stage event: start and progress add or update the stage, and done removes
   it. Stages can overlap, because fetches run at the same time. They keep the
   order of their starts, which is the order of their turns in the status bar."
  [run evt]
  (let [label (str (aget evt "label"))
        total (aget evt "total")
        k (stage-key label total)
        state (aget evt "state")
        stages (or (:stages run) #js [])
        i (stage-index stages label k (= "start" state))]
    (if (= "done" state)
      (assoc run :stages (.filter stages (fn [_ j] (not= i j)))
             :stages-done (inc (or (:stages-done run) 0)))
      (let [stage #js {:key k :label label :n (aget evt "n") :total total}]
        (assoc run :stages (if (neg? i)
                             (.concat stages #js [stage])
                             (.map stages (fn [s j] (if (= i j) stage s))))
               :flow-stage (when (:flow run) label))))))

(defn- new-step [pid]
  #js {:pid pid :label (step-label pid) :state "waiting" :runs 0 :done-runs 0 :ms 0})

(defn- with-step
  "`run` with the step of `pid` replaced by (f step). Adds a step that no
   flow-start named."
  [run pid f]
  (let [steps (js/Object.assign #js {} (:steps run))
        known? (some? (aget steps pid))
        step (js/Object.assign #js {} (or (aget steps pid) (new-step pid)))]
    (aset steps pid (f step))
    (assoc run :steps steps
           :pids (if known? (:pids run) (.concat (or (:pids run) #js []) #js [pid])))))

(defn- on-flow-start [run evt now]
  (let [pids (or (aget evt "pids") #js [])
        steps #js {}]
    (.forEach pids (fn [pid] (aset steps pid (new-step pid))))
    (assoc run :flow (or (aget evt "flow") "anon") :flow-started (event-time evt now)
           :flow-stage nil :failed-flow nil :pids pids :steps steps)))

(defn- on-proc-start
  "A step starts. :runs counts the starts of the same pid in this flow: a row
   chain runs each of its steps one time for each batch."
  [run evt now]
  (with-step run (aget evt "pid")
    (fn [step]
      (aset step "state" "running")
      (aset step "runs" (inc (aget step "runs")))
      (aset step "since" (event-time evt now))
      (aset step "done" nil)
      (aset step "total" nil)
      step)))

(defn- on-proc-end [run evt now state]
  (with-step run (aget evt "pid")
    (fn [step]
      (let [since (aget step "since")
            ms (or (aget evt "ms") (if since (- (event-time evt now) since) 0))]
        (aset step "state" state)
        (aset step "ms" (+ (aget step "ms") ms))
        (aset step "since" nil)
        (when (= "done" state)
          (aset step "done-runs" (inc (aget step "done-runs"))))
        step))))

(defn- on-rows [run evt]
  (with-step run (aget evt "pid")
    (fn [step]
      (aset step "done" (aget evt "done"))
      (aset step "total" (aget evt "total"))
      step)))

(defn- on-snapshot
  "The queue lengths of each step, from a flow-snapshot. The source and the
   sink of the flow are not steps, and they are left out."
  [run evt]
  (let [steps (js/Object.assign #js {} (:steps run))]
    (.forEach (or (aget evt "procs") #js [])
              (fn [p]
                (when-let [step (aget steps (aget p "pid"))]
                  (aset steps (aget p "pid")
                        (js/Object.assign #js {} step
                                          #js {:in-queued (aget p "in-queued")
                                               :out-queued (aget p "out-queued")})))))
    (assoc run :steps steps)))

(defn- on-failure [run evt]
  (assoc run :failed #js {:label (when (aget evt "pid") (step-label (aget evt "pid")))
                          :message (aget evt "message")}))

(defn- step-list [run]
  (.map (or (:pids run) #js []) (fn [pid] (aget (:steps run) pid))))

(defn- distinct-labels [steps]
  (.filter (.map steps (fn [s] (aget s "label")))
           (fn [l i all] (= i (.indexOf all l)))))

(defn flow-label
  "The name of a flow for the reader. A flow with no name of its own shows
   its stage, or else the names of its steps."
  [flow-name stage steps]
  (cond
    (and flow-name (not= "anon" flow-name)) (str flow-name)
    stage (str stage)
    :else (let [labels (distinct-labels steps)]
            (cond
              (zero? (.-length labels)) "flow"
              (> (.-length labels) 2) (str (aget labels 0) " and " (dec (.-length labels)) " more")
              :else (.join labels ", ")))))

(def ^:private history-max 20)

(defn- stopped-steps
  "The steps of a failed flow: a step that still ran failed with the flow."
  [run]
  (let [steps #js {}]
    (.forEach (or (:pids run) #js [])
              (fn [pid]
                (let [step (aget (:steps run) pid)]
                  (aset steps pid
                        (if (= "running" (aget step "state"))
                          (js/Object.assign #js {} step #js {:state "error" :since nil})
                          step)))))
    steps))

(defn- flow-ended
  "Moves the running flow to :history, with `state` \"done\" or \"error\". The
   steps of a failed flow stay, because they show where it failed."
  [run evt now state]
  (if-not (:flow run)
    run
    (let [steps (step-list run)
          label (flow-label (:flow run) (:flow-stage run) steps)
          entry #js {:label label
                     :state state
                     :steps (.-length steps)
                     :runs (.reduce steps (fn [n s] (+ n (aget s "runs"))) 0)
                     :ms (or (aget evt "ms") (- (event-time evt now) (:flow-started run)))}
          history (.slice (.concat (or (:history run) #js []) #js [entry]) (- history-max))
          run (assoc run :flow nil :flow-stage nil :history history)]
      (if (= "error" state)
        (assoc run :steps (stopped-steps run) :failed-flow label)
        (assoc run :pids nil :steps nil)))))

(defn- on-worker
  "A worker-busy-start or worker-busy-end. An end with another item id than
   the item in work is late, and it changes nothing."
  [run evt now busy?]
  (let [id (str (aget evt "worker-id"))
        workers (js/Object.assign #js {} (:workers run))
        prev (or (aget workers id) #js {:id id :jobs 0})]
    (cond
      busy?
      (do (aset workers id #js {:id id :state "busy" :handler (aget evt "handler")
                                :label (aget evt "label") :pid (aget evt "pid")
                                :item (aget evt "item-id") :since (event-time evt now)
                                :jobs (aget prev "jobs")})
          (assoc run :workers workers))

      (and (aget prev "item") (aget evt "item-id") (not= (aget prev "item") (aget evt "item-id")))
      run

      :else
      (do (aset workers id #js {:id id :state "idle" :handler (aget evt "handler")
                                :jobs (inc (aget prev "jobs"))})
          (assoc run :workers workers)))))

(defn- on-trace [run evt now]
  (case (aget evt "kind")
    "stage"             (on-stage run evt)
    "flow-start"        (on-flow-start run evt now)
    "proc-start"        (on-proc-start run evt now)
    "proc-complete"     (on-proc-end run evt now "done")
    "rows-progress"     (on-rows run evt)
    "flow-snapshot"     (on-snapshot run evt)
    "worker-busy-start" (on-worker run evt now true)
    "worker-busy-end"   (on-worker run evt now false)
    "flow-complete"     (flow-ended run evt now "done")
    "proc-error"        (on-failure (on-proc-end run evt now "error") evt)
    "flow-error"        (flow-ended (if (:failed run) run (on-failure run evt)) evt now "error")
    run))

(defn- idle-workers [run]
  (let [workers #js {}]
    (.forEach (js/Object.keys (or (:workers run) #js {}))
              (fn [id]
                (let [w (aget (:workers run) id)]
                  (aset workers id #js {:id id :state "idle" :handler (aget w "handler")
                                        :jobs (aget w "jobs")}))))
    workers))

(defn- ended
  "The state at the end of the run. A flow that is still open failed with the
   run."
  [run now]
  (assoc (flow-ended run #js {} now "error")
         :ended now :stages #js [] :workers (idle-workers run)))

(defn- stage-start? [evt]
  (and (= "stage" (aget evt "kind")) (= "start" (aget evt "state"))))

(defn- on-trace-message
  "A trace event counts only in a run. A stage with no run starts one, because
   the host posts its own stages before cg/run-started."
  [run evt now]
  (cond
    (= "running" (:phase run)) (on-trace run evt now)
    (stage-start? evt) (on-trace {:phase "running" :started now :early true} evt now)
    :else run))

(defn run-reducer
  "The run status after the bus message `m`, which arrived at `now` (ms)."
  [run m now]
  (let [t (when m (aget m "type"))]
    (cond
      (= t "cg/run-started") (if (and (= "running" (:phase run)) (:early run))
                               (assoc run :early false)
                               {:phase "running" :started now})
      (= t "cg/trace-event") (if (aget m "event")
                               (on-trace-message run (aget m "event") now)
                               run)
      (= t "cg/run-ended")   (if (= "error" (aget m "outcome"))
                               (assoc (ended run now) :phase "error")
                               (assoc (ended run now) :phase "ok" :count (aget m "count")))
      (= t "cg/run-error")   (assoc (ended run now) :phase "error"
                                    :message (aget m "message"))
      :else run)))

(defn- pad2 [n] (if (< n 10) (str "0" n) (str n)))

(defn clock-text
  "ms -> \"0:07\", \"6:31\" or \"1:02:03\"."
  [ms]
  (let [s (js/Math.floor (/ (max 0 ms) 1000))
        h (js/Math.floor (/ s 3600))
        m (js/Math.floor (/ (mod s 3600) 60))
        sec (mod s 60)]
    (if (pos? h)
      (str h ":" (pad2 m) ":" (pad2 sec))
      (str m ":" (pad2 sec)))))

(defn duration-text
  "ms -> \"0.4 s\" under ten seconds, else the clock text."
  [ms]
  (if (< ms 10000)
    (str (.toFixed (/ (max 0 ms) 1000) 1) " s")
    (clock-text ms)))

(defn- sentence
  "`s` with its first letter in upper case."
  [s]
  (let [t (str s)]
    (str (.toUpperCase (.slice t 0 1)) (.slice t 1))))

(defn- num-text [n] (.toLocaleString n "en-US"))

(defn- count-text [n word]
  (str (num-text n) " " word (when (not= 1 n) "s")))

(defn- step-text
  "\"step 4 of 7: buffer, batch 2\" for a step of :steps. The batch shows
   from the second pass of a step."
  [run step]
  (let [pids (:pids run)
        pass (aget step "runs")]
    (str "step " (inc (.indexOf pids (aget step "pid"))) " of " (.-length pids) ": "
         (aget step "label")
         (when (> pass 1) (str ", batch " pass)))))

(defn- rows-text [done total]
  (when total
    (str (num-text (or done 0)) " of " (count-text total "row"))))

(defn- stage-count-text
  "\"2,000 of 6,334\" with a total, \"2,000 so far\" with none."
  [stage]
  (let [n (aget stage "n")
        total (aget stage "total")]
    (cond
      total (str (num-text (or n 0)) " of " (num-text total))
      (some? n) (str (num-text n) " so far")
      :else nil)))

(defn- lane-text [runtime]
  (case runtime
    "js"  "ran in this browser (js)"
    "jvm" "ran on the JVM backend (jvm)"
    nil))

(defn- join-parts [parts]
  (let [ps (.filter parts (fn [p] (and (some? p) (not= "" p))))]
    (when (pos? (.-length ps)) (.join ps " · "))))

(defn- stage-item [stage]
  (let [total (aget stage "total")]
    #js {:key (str "stage " (aget stage "key"))
         :title (sentence (aget stage "label"))
         :n (when total (or (aget stage "n") 0))
         :total total
         :detail (stage-count-text stage)}))

(defn- step-item [run step]
  (let [total (aget step "total")]
    #js {:key (str "step " (aget step "pid"))
         :title (sentence (step-text run step))
         :n (when total (or (aget step "done") 0))
         :total total
         :detail (rows-text (aget step "done") total)}))

(defn run-items
  "One #js {:key :title :n :total :detail} for each piece of work that runs:
   the open stages in the order of their starts, then the running steps of the
   flow that count rows. A step that counts no rows shows only when nothing
   else does, because a stage or a row count says more."
  [run]
  (let [running (.filter (step-list run) (fn [s] (= "running" (aget s "state"))))
        counted (.filter running (fn [s] (some? (aget s "total"))))
        items (.concat (.map (or (:stages run) #js []) stage-item)
                       (.map counted (fn [s] (step-item run s))))]
    (if (pos? (.-length items))
      items
      (.map running (fn [s] (step-item run s))))))

(defn- shown-index
  "The place in `items` of the item with key `k`, or 0 when no item has it."
  [items k]
  (max 0 (.findIndex items (fn [item] (= k (aget item "key"))))))

(defn show-next
  "`run` with :shown at the key of the item after the one that the status bar
   shows now."
  [run]
  (let [items (run-items run)
        n (.-length items)]
    (if (pos? n)
      (assoc run :shown (aget (aget items (mod (inc (shown-index items (:shown run))) n)) "key"))
      run)))

(defn taking-turns?
  "True while the run has more than one item to show. The end of a run closes
   each item."
  [run]
  (> (.-length (run-items run)) 1))

(defn- running-view
  "The title, the bar and the detail all come from one item, and :position
   gives its place, \"(2/3)\", when more than one item runs."
  [run now]
  (let [items (run-items run)
        n (.-length items)
        i (shown-index items (:shown run))
        item (when (pos? n) (aget items i))]
    #js {:kind "running"
         :title (if item (aget item "title") "Running")
         :position (when (> n 1) (str "(" (inc i) "/" n ")"))
         :detail (when item (aget item "detail"))
         :n (when item (aget item "n"))
         :total (when item (aget item "total"))
         :clock (clock-text (- now (:started run)))}))

(defn- took
  "The run time as text, or nil for a run that never started. A run with no
   end yet ends at `now`."
  [run now]
  (when (:started run)
    (duration-text (- (or (:ended run) now) (:started run)))))

(defn- ok-view [run exec now]
  (let [t (took run now)]
    #js {:kind "ok"
         :title (if t (str "Finished in " t) "Finished")
         :detail (join-parts #js [(when (some? (:count run)) (count-text (:count run) "result"))
                                  (when (and (nil? (:count run)) (:message exec)) (:message exec))
                                  (lane-text (:runtime exec))])}))

(defn- error-view [run exec now]
  (let [failed (:failed run)
        label  (when failed (aget failed "label"))
        t      (took run now)]
    #js {:kind "error"
         :title (str "Run failed"
                     (when t (str " after " t))
                     (when label (str ", in step " label)))
         :detail (or (:message run) (when failed (aget failed "message")) (:message exec))}))

(defn run-view
  "#js {:kind :title :position :detail :n :total :clock} for the status bar,
   or nil. The end states of `exec` win, because the app sets them before the
   bus ends the run."
  [run exec now]
  (let [phase (:phase run)
        es    (:status exec)]
    (cond
      (= "refused" es)    #js {:kind "refused" :title "Run refused" :detail (:message exec)}
      (or (= "ok" es) (and (nil? es) (= "ok" phase)))       (ok-view run exec now)
      (or (= "error" es) (and (nil? es) (= "error" phase))) (error-view run exec now)
      (= "running" phase) (running-view run now)
      (= "running" es)    #js {:kind "running" :title "Starting the engine"
                               :detail "The first run loads the geometry engine."}
      :else nil)))

(defn- group-state [steps]
  (let [has? (fn [st] (.some steps (fn [s] (= st (aget s "state")))))]
    (cond
      (has? "error")   "error"
      (has? "running") "running"
      (.every steps (fn [s] (= "done" (aget s "state")))) "done"
      (has? "done")    "running"
      :else            "waiting")))

(defn- sum [steps k]
  (.reduce steps (fn [n s] (+ n (or (aget s k) 0))) 0))

(defn- group-count-text
  "\"12 of 20 done\" for a row of many steps, \"batch 3\" for one step that
   ran more than one time."
  [steps]
  (let [n (.-length steps)
        runs (sum steps "runs")]
    (cond
      (> n 1) (str (.-length (.filter steps (fn [s] (= "done" (aget s "state")))))
                   " of " n " done")
      (> runs 1) (str "batch " runs)
      :else nil)))

(defn- step-row [steps now]
  (let [running (.find steps (fn [s] (= "running" (aget s "state"))))
        queued (sum steps "in-queued")
        ms (+ (sum steps "ms")
              (if (and running (aget running "since")) (max 0 (- now (aget running "since"))) 0))]
    #js {:label (aget (aget steps 0) "label")
         :state (group-state steps)
         :count (group-count-text steps)
         :done (when running (aget running "done"))
         :total (when running (aget running "total"))
         :rows (when running (rows-text (aget running "done") (aget running "total")))
         :queued (when (pos? queued) (str queued " queued"))
         :time (when (pos? ms) (duration-text ms))}))

(defn step-rows
  "One #js {:label :state :count :done :total :rows :queued :time} for each step
   of the running flow, in flow order. Steps with the same op name share a row."
  [run now]
  (let [steps (step-list run)]
    (.map (distinct-labels steps)
          (fn [label]
            (step-row (.filter steps (fn [s] (= label (aget s "label")))) now)))))

(defn- worker-name [id]
  (if (or (= "main" id) (= "js" id)) "Main thread" (str "Worker " id)))

(defn- worker-order [a b]
  (let [rank (fn [w] (if (= "Main thread" (worker-name (aget w "id"))) -1 (js/Number (aget w "id"))))]
    (- (rank a) (rank b))))

(defn worker-cells
  "One #js {:id :name :state :handler :label :step :jobs} for each worker
   that sent an event, the main thread first."
  [run]
  (let [workers (or (:workers run) #js {})]
    (.map (.sort (.map (js/Object.keys workers) (fn [id] (aget workers id))) worker-order)
          (fn [w]
            #js {:id (aget w "id")
                 :name (worker-name (aget w "id"))
                 :state (aget w "state")
                 :handler (aget w "handler")
                 :label (aget w "label")
                 :step (when (aget w "pid") (step-label (aget w "pid")))
                 :jobs (aget w "jobs")}))))

(defn flow-rows
  "One #js {:label :state :detail :time} for each flow of this run that
   ended, the oldest first."
  [run]
  (.map (or (:history run) #js [])
        (fn [f]
          #js {:label (aget f "label")
               :state (aget f "state")
               :detail (join-parts #js [(count-text (aget f "steps") "step")
                                        (when (> (aget f "runs") (aget f "steps"))
                                          (count-text (aget f "runs") "run"))])
               :time (duration-text (aget f "ms"))})))

(defn current-flow-label
  "The label of the flow that step-rows shows: the running flow, or the flow
   that failed, or nil."
  [run]
  (if (:flow run)
    (flow-label (:flow run) (:flow-stage run) (step-list run))
    (:failed-flow run)))
