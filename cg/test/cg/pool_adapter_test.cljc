;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; joint-compute-pool unit tests against a fake worker-router pool object:
;; no workers spawn, so the suite runs pool-free and fast. cljs-only,
;; because the adapter exists only on the cljs branch; the JVM sees
;; an empty namespace.
(ns cg.pool-adapter-test
  #?(:cljs (:require [cljs.test :refer [deftest is]]
                     ["../../src/cg/pool.mjs" :as cg-pool]
                     ["ffi-wasm" :as ffi]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

#?(:cljs
   (do
     (defn- fake-pool [size claims releases]
       #js {:size size
            :claim (fn []
                     (swap! claims inc)
                     #js {:index 0
                          :release (fn [] (swap! releases inc))})})

     (defn- install-fake-pool!
       "Adopt `fake` into a fresh workload-pool registry and install that
        as cg's registry — the real cg.run wiring minus the worker spawn,
        so the adapter sees the fake through cg.pool/current-pool."
       [fake]
       (let [reg (ffi/init-workload-pool! {})]
         (ffi/adopt-pool! reg fake)
         (cg-pool/set-registry! reg)
         reg))

     (defn- tick
       "One macrotask, so every pending microtask (job settlement, the
        adapter's release/decrement/drain chain) flushes first."
       []
       (js/Promise. (fn [resolve] (js/setTimeout resolve 0))))

     ;; The pool-ready invariant lives here: CLJS table-mode coord step
     ;; factories starve unless init-cg-workers! ran first, and this
     ;; throw is the starvation guard cg.dataset relies on. First in the
     ;; suite so it observes the no-registry state.
     (deftest require-pool-throws-before-init
       (is (nil? (cg-pool/current-pool)) "no registry installed yet")
       (let [outcome (try (cg-pool/require-pool)
                          :returned
                          (catch :default e (.-message e)))]
         (is (.includes (str outcome) "joint pool not initialized")
             "require-pool must throw, not return nil, before init")))

     (deftest a-submitted-job-resolves-with-its-value
       ((fn ^:async body []
          (install-fake-pool! (fake-pool 2 (atom 0) (atom 0)))
          (let [adapter (cg-pool/joint-compute-pool)
                v (await (adapter (fn [] 42)))]
            (is (= 42 v) "the submit promise carries the thunk's return")
            (cg-pool/clear-registry!)))))

     (deftest a-throwing-job-rejects-the-submit-promise
       ((fn ^:async body []
          (install-fake-pool! (fake-pool 2 (atom 0) (atom 0)))
          (let [adapter (cg-pool/joint-compute-pool)
                outcome (await (-> (adapter (fn [] (throw (js/Error. "boom"))))
                                   (.then (fn [_] :resolved)
                                          (fn [e] (.-message e)))))]
            (is (= "boom" outcome) "a throwing job rejects with its error")
            (cg-pool/clear-registry!)))))

     (deftest the-cap-limits-in-flight-jobs
       ((fn ^:async body []
          (let [claims (atom 0)
                releases (atom 0)]
            (install-fake-pool! (fake-pool 1 claims releases))
            (let [adapter (cg-pool/joint-compute-pool)
                  started (atom [])
                  resolvers (atom {})
                  make-job (fn [id]
                             (fn []
                               (swap! started conj id)
                               (js/Promise.
                                (fn [resolve]
                                  (swap! resolvers assoc id resolve)))))
                  p1 (adapter (make-job "a"))
                  p2 (adapter (make-job "b"))]
              (await (tick))
              (is (= ["a"] @started) "with size 1, the second job waits")
              (is (= 1 @claims) "one claim per in-flight job")
              ((get @resolvers "a") "va")
              (await (tick))
              (is (= ["a" "b"] @started) "finishing a job starts the next")
              ((get @resolvers "b") "vb")
              (is (= "va" (await p1)) "the first submit resolves in order")
              (is (= "vb" (await p2)) "and the queued one after it")
              (await (tick))
              (is (= 2 @claims) "each job claimed a worker")
              (is (= 2 @releases) "and every claim was released")
              (cg-pool/clear-registry!))))))

     (deftest clearing-the-pool-rejects-queued-jobs
       ((fn ^:async body []
          (let [claims (atom 0)
                releases (atom 0)]
            (install-fake-pool! (fake-pool 1 claims releases))
            (let [adapter (cg-pool/joint-compute-pool)
                  resolve-a (atom nil)
                  p1 (adapter (fn []
                                (js/Promise.
                                 (fn [resolve] (reset! resolve-a resolve)))))
                  p2 (adapter (fn [] :never-runs))]
              (await (tick))
              (cg-pool/clear-registry!)
              (@resolve-a :done)
              (let [outcome (await (-> p2
                                       (.then (fn [_] :resolved)
                                              (fn [e] (.-message e)))))]
                (is (= :done (await p1)) "the in-flight job still resolves")
                (is (= "cg.pool: joint pool terminated" outcome)
                    "a job queued behind it rejects at drain")
                (await (tick))
                (is (= 1 @claims)
                    "the rejected job never claims against the dead pool")
                (is (= 1 @releases) "and the in-flight claim was released")))))))

     (deftest a-cleared-pool-shuts-the-adapter-down
       ((fn ^:async body []
          (install-fake-pool! (fake-pool 1 (atom 0) (atom 0)))
          (let [adapter (cg-pool/joint-compute-pool)]
            (is (= 1 (await (adapter (fn [] 1)))) "a registered pool is live")
            (cg-pool/clear-registry!)
            (let [outcome (await (-> (adapter (fn [] 1))
                                     (.then (fn [_] :resolved)
                                            (fn [_e] :rejected))))]
              (is (= :rejected outcome)
                  "clearing the pool shuts the adapter down: submit rejects"))))))

     ;; terminate-cg-workers! awaits wp/shutdown-pool! BEFORE it drops the
     ;; registry, so there is a window where the registry is installed but
     ;; its :pool atom is empty. current-pool must read that atom live —
     ;; a snapshot taken at set-registry! time would keep serving the dead
     ;; pool through the whole shutdown walk.
     (deftest shutdown-empties-current-pool-through-a-live-registry
       ((fn ^:async body []
          (let [reg (install-fake-pool! (fake-pool 1 (atom 0) (atom 0)))
                adapter (cg-pool/joint-compute-pool)]
            (is (= 1 (await (adapter (fn [] 1)))) "live before shutdown")
            (await (ffi/shutdown-pool! reg))
            (is (nil? (cg-pool/current-pool))
                "current-pool goes nil when the registry's pool shuts down, registry still installed")
            (is (= :rejected (await (-> (adapter (fn [] 1))
                                        (.then (fn [_] :resolved)
                                               (fn [_e] :rejected)))))
                "the adapter sees the shutdown without cg dropping the registry")
            (cg-pool/clear-registry!)))))))

#?(:cljs (run_tests_and_exit_BANG_ "cg.pool-adapter-test"))
