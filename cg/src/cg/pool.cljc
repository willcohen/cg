;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.pool
  "Shared registry for the run-time pools of cg: the joint worker pool for
   proj, wasmts and gdal, and on the JVM the :compute ExecutorService."
  #?(:cljs (:require ["ffi-wasm" :as ffi])))

#?(:clj (set! *warn-on-reflection* true))

#?(:cljs (do

           (defonce ^:private *registry (atom nil))

           (defn set-registry!
             "Install the joint-pool registry (clj-native workload-pool).
   cg.run/init-cg-workers! calls this."
             [registry]
             (when @*registry
               (throw (js/Error. "cg.pool/set-registry!: registry already set; clear-registry! first")))
             (reset! *registry registry))

           (defn clear-registry!
             "Drop the registry reference. terminate-cg-workers! calls this
   after wp/shutdown-pool! resolves."
             []
             (reset! *registry nil))

           (defn current-registry
             "Return the installed joint-pool registry, or nil before
   init-cg-workers!."
             []
             @*registry)

           (defn current-pool
             "Return the live pool, or nil before init-cg-workers! or after
   wp/shutdown-pool!."
             []
             (when-let [registry @*registry]
               (ffi/current-pool registry)))

           (defn require-pool
             "Return the live pool. Throws before init-cg-workers!."
             []
             (or (current-pool)
                 (throw (js/Error.
                         "cg.pool/require-pool: joint pool not initialized; call (cg.run/init-cg-workers! {...}) first"))))

           (defn claim-worker
             "Reserve one worker for a sequence of RPCs. Returns #js {:index N :release fn};
   call the release fn after the last RPC settles, on success and on failure."
             []
             (ffi/claim (require-pool)))

           (defn- drain-compute-queue!
             "Start queued jobs while fewer than pool-size run. Rejects the queued jobs
   when the pool of the adapter is not the registered pool."
             [p]
             (if (identical? (.-pool p) (current-pool))
               (let [the-pool (.-pool p)
                     size (.-size the-pool)]
                 (loop []
                   (when (and (< (.-active p) size)
                              (pos? (.-length (.-queue p))))
                     (let [entry (.shift (.-queue p))
                           job (.-job entry)
                           resolve (.-resolve entry)
                           reject (.-reject entry)
                           claim ((.-claim the-pool))]
                       (set! (.-active p) (inc (.-active p)))
                       (-> (js/Promise.resolve)
                           (.then (fn [] (job)))
                           (.then resolve reject)
                           (.finally (fn []
                                       ((.-release claim))
                                       (set! (.-active p) (dec (.-active p)))
                                       (drain-compute-queue! p))))
                       (recur)))))
               (loop []
                 (when (pos? (.-length (.-queue p)))
                   (let [entry (.shift (.-queue p))]
                     ((.-reject entry)
                      (js/Error. "cg.pool: joint pool terminated"))
                     (recur))))))

           ;; A compute thunk closes over live state and WasmTS proxies, which
           ;; cannot be structured-cloned. The jobs run on the main thread, and
           ;; the thunk reaches the workers through the joint-pool RPC proxies.
           (defn- submit-compute-job! [p job]
             (if (identical? (.-pool p) (current-pool))
               (js/Promise.
                (fn [resolve reject]
                  (.push (.-queue p) #js {:job job :resolve resolve :reject reject})
                  (drain-compute-queue! p)))
               (js/Promise.reject
                (js/Error. "cg.pool: joint pool terminated"))))

           (defonce ^:private *flow-exec (atom nil))

           (defn joint-compute-pool
             "Return the :compute-exec fn of create-flow for the registered joint pool:
   a fn of a job that returns a promise of its value."
             []
             (let [the-pool (require-pool)
                   cached @*flow-exec]
               (if (and cached (identical? (.-pool (:state cached)) the-pool))
                 (:exec cached)
                 (let [state #js {:pool the-pool :active 0 :queue #js []}
                       exec (fn [job] (submit-compute-job! state job))]
                   (reset! *flow-exec {:state state :exec exec})
                   exec))))))

#?(:clj
   (do

     (defonce ^:private *jvm-pool (atom nil))

     (defn set-jvm-pool!
       "Install the JVM workload-pool handle, a map with :registry and
        :compute-exec. cg.run/init-cg-workers! calls this."
       [handle]
       (when @*jvm-pool
         (throw (ex-info "cg.pool/set-jvm-pool!: pool already set; clear-jvm-pool! first" {})))
       (reset! *jvm-pool handle))

     (defn clear-jvm-pool!
       "Drop the JVM workload-pool handle. terminate-cg-workers! calls this."
       []
       (reset! *jvm-pool nil))

     (defn current-jvm-pool
       "Return the JVM workload-pool handle, or nil before init-cg-workers!."
       []
       @*jvm-pool)

     (defn current-compute-exec
       "Return the JVM :compute ExecutorService, or nil. cg.run passes it to
        create-flow as :compute-exec."
       []
       (:compute-exec @*jvm-pool))))
