;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; cg.run.flow runs a flow config and emits its trace events. It does not know
;; models, because cg.io runs flows too, and cg.run requires cg.io through
;; cg.dataset. cg.run/run-model compiles a model with model->flow and gives
;; the result to run-flow.

(ns cg.run.flow
  (:require #?(:clj [clojure.core.async :as async])
            #?(:clj [clojure.core.async.flow :as flow])
            #?(:cljs ["cljc-flow/flow" :as flow])
            #?(:cljs ["cljc-flow/core" :as ca])
            [cg.pool :as cg-pool]
            #?(:clj [cg.trace :as trace]
               :cljs [cg.trace-js :as trace])))

#?(:clj (set! *warn-on-reflection* true))

(defn pid
  "The pid `<flow>__<id>` of a proc, which is the id convention of the
  canvas."
  [flow-name id]
  (str flow-name "__" (if (keyword? id) (name id) (str id))))

#?(:clj
   (defn instrument-flow-step
     "Wrap the step fn `step-fn` to emit `proc-start`, `proc-complete` and
     `proc-error` trace events with `pid` (`<model>__<node-id>`)."
     [pid step-fn]
     (fn instrumented
       ([] (step-fn))
       ([state] (step-fn state))
       ([state arg] (step-fn state arg))
       ([state in-id msg]
        (let [t0 (System/currentTimeMillis)]
          (trace/emit! "proc-start" {:pid pid :in in-id})
          (try
            (let [result (step-fn state in-id msg)
                  t1 (System/currentTimeMillis)
                  out (second result)
                  out-count (when (map? out)
                              (reduce + 0 (map count (vals out))))]
              (trace/emit! "proc-complete"
                           (cond-> {:pid pid :ms (- t1 t0)}
                             out-count (assoc :out-count out-count)))
              result)
            (catch Throwable e
              (let [t1 (System/currentTimeMillis)]
                (trace/emit! "proc-error"
                             {:pid pid :ms (- t1 t0)
                              :message (.getMessage e)}))
              (throw e))))))))

#?(:cljs
   (defn instrument-flow-step
     "CLJS twin of the JVM instrument-flow-step, through cg.trace-js listeners.
     For a Promise result (an async wasmts op), the event goes out when it settles."
     [pid step-fn]
     (fn instrumented
       ([] (step-fn))
       ([state] (step-fn state))
       ([state arg] (step-fn state arg))
       ([state in-id msg]
        (let [t0 (js/Date.now)
              complete! (fn [result]
                          (let [out (second result)
                                out-count (when (map? out)
                                            (reduce + 0 (map count (vals out))))]
                            (trace/emit! "proc-complete"
                                         (cond-> {:pid pid :ms (- (js/Date.now) t0)}
                                           out-count (assoc :out-count out-count)))))
              error! (fn [e]
                       (trace/emit! "proc-error"
                                    {:pid pid :ms (- (js/Date.now) t0)
                                     :message (or (.-message e) (str e))}))]
          (trace/emit! "proc-start" {:pid pid :in in-id})
          (trace/set-step-pid! pid)
          (try
            (let [result (step-fn state in-id msg)]
              (if (and result (.-then result))
                (.then result
                       (fn [resolved] (complete! resolved) resolved)
                       (fn [e] (error! e) (js/Promise.reject e)))
                (do (complete! result) result)))
            (catch :default e
              (error! e)
              (throw e))
            (finally
              (trace/set-step-pid! nil))))))))

(defn make-process
  "flow/process for `step-fn`, with the :workload and :compute-timeout-ms of
   its describe map as opts."
  [step-fn]
  ;; flow/process reads :compute-timeout-ms only from its opts (default 5000
  ;; ms), and ignores the value in the describe map.
  (let [desc (step-fn)
        opts (cond-> {}
               (:workload desc) (assoc :workload (:workload desc))
               (:compute-timeout-ms desc) (assoc :compute-timeout-ms (:compute-timeout-ms desc)))]
    (if (seq opts)
      (flow/process step-fn opts)
      (flow/process step-fn))))

#?(:cljs
   (defn- chan-queued
     "The messages in the buffer of a cljc-flow channel. The blocked puts do
      not count: their queue also holds the dead handlers of each alts."
     [ch]
     (let [buf  (when ch (.-buf ch))
           ring (when buf (.-buf buf))]
       (if (and ring (number? (.-length ring))) (.-length ring) 0))))

#?(:cljs
   (defn- queued [chans]
     (reduce + 0 (map chan-queued (vals (or chans {}))))))

#?(:cljs
   (defn- snapshot-watch
     "The flow-snapshot events of a flow: {:ping-all! :start! :stop!}. A proc
      answers a ping only between two steps; with no fresh answer it is \"busy\"."
     [flow-obj flow-name ids]
     (let [seen    #js {}
           at      #js {}
           waiting #js {}
           timers  #js {:tick nil :emit nil}
           ping!   (fn [id]
                     (when-not (aget waiting id)
                       (aset waiting id true)
                       (.then (flow/ping-proc flow-obj id :timeout-ms 250)
                              (fn [reply]
                                (aset waiting id false)
                                (when reply
                                  (aset seen id reply)
                                  (aset at id (js/Date.now)))
                                nil)
                              (fn [_] (aset waiting id false) nil))))
           proc    (fn [id]
                     (let [reply (aget seen id)
                           fresh? (and reply (< (- (js/Date.now) (aget at id)) 600))]
                       {:pid (pid flow-name id)
                        :status (if fresh?
                                  (get reply :clojure.core.async.flow/status)
                                  "busy")
                        :count (if reply (get reply :clojure.core.async.flow/count) 0)
                        :in-queued (queued (when reply (get reply :clojure.core.async.flow/ins)))
                        :out-queued (queued (when reply (get reply :clojure.core.async.flow/outs)))}))
           emit!   (fn []
                     (trace/emit! "flow-snapshot" {:flow flow-name :procs (mapv proc ids)}))
           tick!   (fn []
                     (doseq [id ids] (ping! id))
                     (aset timers "emit" (js/setTimeout emit! 300)))]
       {:ping-all! (fn [] (js/Promise.all (to-array (map ping! ids))))
        :start!    (fn [] (aset timers "tick" (js/setInterval tick! 1000)))
        :stop!     (fn []
                     (js/clearInterval (aget timers "tick"))
                     (js/clearTimeout (aget timers "emit")))})))

(defn- flow-error-ex
  "The ex-info for a report from the flow error channel, with the report as
   ex-data and, on the JVM, the underlying exception as cause."
  [err]
  (let [pid (get err :clojure.core.async.flow/pid)
        op  (get err :clojure.core.async.flow/op)
        n   (get err :clojure.core.async.flow/count)
        ex  (get err :clojure.core.async.flow/ex)
        msg (str "Flow execution error"
                 (when pid (str " in " pid))
                 (when op (str " during " op))
                 (when n (str " (message " n ")"))
                 (when ex
                   (str ": "
                        #?(:clj (.getName (class ex))
                           :cljs (or (.-name ex) "Error"))
                        (when-let [m (ex-message ex)]
                          (str " - " m)))))]
    #?(:clj (ex-info msg {:type :flow-error :error err} ex)
       :cljs (ex-info msg {:type :flow-error :error err}))))

(defn create-pool-flow [config]
  (flow/create-flow
   #?(:clj (assoc config :compute-exec (cg-pool/current-compute-exec))
      :cljs (cond-> config
              (cg-pool/current-pool)
              (assoc :compute-exec (cg-pool/joint-compute-pool))))))

#?(:clj
   (defn run-flow
     "Run the flow of `flow-def` ({:config :result-atom :completed}) until
      `completed` is true, and give the value of `result-atom`. `opts`: :name
      (default \"anon\"), :triggers (the procs that get [:start] on :trigger),
      :pids (the pids of flow-start), :timeout-ms (60000), :return-flow?
      ({:results :flow}) and :complete-data (a fn of the results that gives
      more keys for flow-complete). Throws ex-info :type :timeout or :flow-error."
     [{:keys [config result-atom completed]} opts]
     (let [flow-obj (create-pool-flow config)
           flow-name (or (:name opts) "anon")
           trace-session (trace/open-file-writer! flow-name)
           flow-t0 (System/currentTimeMillis)
           _ (trace/emit! "flow-start" {:flow flow-name :pids (vec (:pids opts))})]
       (try
         (let [timeout-ms (get opts :timeout-ms 60000)
               return-flow? (get opts :return-flow? false)

               ;; flow/start leaves the processes paused.
               {:keys [error-chan]} (flow/start flow-obj)
               _ (flow/resume flow-obj)

               _ (doseq [p (:triggers opts)]
                   (flow/inject flow-obj [p :trigger] [:start]))

               start-time (System/currentTimeMillis)
               results (loop []
                         (let [elapsed (- (System/currentTimeMillis) start-time)]
                           (cond
                             ;; A step that throws puts its error on
                             ;; error-chan before it reads the next message.
                             ;; Thus the error of a failed batch is in the
                             ;; chan when eos/eos completes the sink.
                             @completed
                             (if-let [err (async/poll! error-chan)]
                               (throw (flow-error-ex err))
                               @result-atom)

                             ;; The error check: this clause throws or falls through.
                             (let [err (async/poll! error-chan)]
                               (when err
                                 (throw (flow-error-ex err))))
                             false

                             (> elapsed timeout-ms)
                             (throw (ex-info "Flow execution timeout"
                                             {:type :timeout
                                              :timeout-ms timeout-ms
                                              :elapsed-ms elapsed}))

                             :else
                             (do
                               (Thread/sleep 100)
                               (recur)))))]

           (flow/stop flow-obj)

           (trace/emit! "flow-complete"
                        (merge {:flow flow-name
                                :ms (- (System/currentTimeMillis) flow-t0)}
                               (when-let [f (:complete-data opts)] (f results))))

           (if return-flow?
             {:results results :flow flow-obj}
             results))
         (catch Throwable e
           (trace/emit! "flow-error"
                        {:flow flow-name
                         :ms (- (System/currentTimeMillis) flow-t0)
                         :message (.getMessage e)})
           (throw e))
         (finally
           ;; Let the trace mult drain the events in flight before the file
           ;; writer closes.
           (Thread/sleep 50)
           (trace/close-file-writer! trace-session))))))

#?(:cljs
   (defn run-flow
     "Run the flow of `flow-def` ({:config :result-atom :completed}) until
      `completed` is true. Gives a Promise of the value of `result-atom`.
      `opts`: :name (default \"anon\"), :triggers (the procs that get [:start]
      on :trigger), :pids (the pids of flow-start), :timeout-ms (60000),
      :return-flow? ({:results :flow}) and :complete-data (as on the JVM).
      Rejects with ex-info :type :timeout or :flow-error."
     [{:keys [config result-atom completed]} opts]
     (let [flow-obj (create-pool-flow config)]
       ;; flow-start, flow-complete and flow-error mirror the JVM branch.
       ;; resolve and reject wrap the raw settlers, because each exit path
       ;; must emit its lifecycle event one time.
       (js/Promise.
        (fn [resolve* reject*]
          (let [flow-name (or (:name opts) "anon")
                flow-t0 (js/Date.now)
                _ (trace/emit! "flow-start" {:flow flow-name :pids (vec (:pids opts))})
                ;; The procs of the config, because a group node of the
                ;; model has no proc of its own.
                watch (snapshot-watch flow-obj flow-name (vec (keys (:procs config))))
                resolve (fn [v]
                          ((:stop! watch))
                          (trace/emit! "flow-complete"
                                       (merge {:flow flow-name
                                               :ms (- (js/Date.now) flow-t0)}
                                              (when-let [f (:complete-data opts)] (f v))))
                          (resolve* v))
                reject (fn [e]
                         ((:stop! watch))
                         (trace/emit! "flow-error"
                                      {:flow flow-name
                                       :ms (- (js/Date.now) flow-t0)
                                       :message (or (and e (.-message e)) (str e))})
                         (reject* e))
                timeout-ms (get opts :timeout-ms 60000)
                return-flow? (get opts :return-flow? false)
                {:keys [error-chan]} (flow/start flow-obj)]

            ;; The first ping finds the channels of each proc while the procs
            ;; are paused, because a proc in a step does not answer.
            ;; flow/resume gives a Promise. Inject the triggers only after it
            ;; resolves.
            (-> ((:ping-all! watch))
                (.then (fn []
                         ((:start! watch))
                         (flow/resume flow-obj)))
                (.then (fn []
                         (doseq [p (:triggers opts)]
                           (flow/inject flow-obj [p :trigger] [:start]))

                         (let [start-time (js/Date.now)
                               check-done (fn check-done []
                                            (let [elapsed (- (js/Date.now) start-time)
                                                  err (ca/poll! error-chan)]
                                              (cond
                                                ;; A step error goes to error-chan and the process
                                                ;; loop continues. Without this poll, an op that
                                                ;; throws gives a silent hang until the timeout.
                                                ;; It comes before @completed, because eos/eos
                                                ;; completes the sink after a failed batch too.
                                                (some? err)
                                                (do
                                                  (flow/stop flow-obj)
                                                  (reject (flow-error-ex err)))

                                                @completed
                                                (do
                                                  (flow/stop flow-obj)
                                                  ;; No clj->js here: a squint map or vector is a plain
                                                  ;; JS object or array, and clj->js recurses into
                                                  ;; WasmTS geometry proxies and throws.
                                                  (if return-flow?
                                                    (resolve {:results @result-atom :flow flow-obj})
                                                    (resolve @result-atom)))

                                                (> elapsed timeout-ms)
                                                (reject (ex-info "Flow execution timeout"
                                                                 {:type :timeout
                                                                  :timeout-ms timeout-ms
                                                                  :elapsed-ms elapsed}))

                                                :else
                                                (js/setTimeout check-done 100))))]
                           (check-done))))
                (.catch reject))))))))
