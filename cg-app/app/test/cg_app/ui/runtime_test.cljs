;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.runtime-test
  "Runtime selection: auto follows reachability, and an explicit jvm or js
   choice stays. Explicit jvm with the backend down refuses."
  (:require [cljs.test :as t :refer [deftest is]]
            ["../../../src/cg_app/ui/runtime.mjs" :as rt]))

(deftest init-defaults-without-stored-values
  ;; node can have a localStorage. Clear both keys first.
  (when-let [ls (.-localStorage js/globalThis)]
    (.removeItem ls "cg-runtime")
    (.removeItem ls "cg-backend-url"))
  (is (= "auto" (rt/init!)))
  (is (= "http://localhost:3000" (rt/backend-url))))

(deftest init-prefers-injected-backend-url
  ;; A new ephemeral port must override a stale persisted one.
  (when-let [ls (.-localStorage js/globalThis)]
    (.setItem ls "cg-backend-url" "http://localhost:9999"))
  (set! (.-__CG_BACKEND_URL__ js/globalThis) "http://127.0.0.1:54321")
  (try
    (rt/init!)
    (is (= "http://127.0.0.1:54321" (rt/backend-url)))
    (finally
      (js-delete js/globalThis "__CG_BACKEND_URL__")
      (when-let [ls (.-localStorage js/globalThis)]
        (.removeItem ls "cg-backend-url"))
      nil)))

(deftest direct-edition-defaults-to-jvm
  ;; The Direct edition embeds a JVM. Its JS backend is not ready at boot,
  ;; because wasm must start first.
  (js-delete js/globalThis "__CG_NATIVE__")
  (is (false? (rt/direct-edition?)))
  (is (= "auto" (rt/default-choice)))
  (set! (.-__CG_NATIVE__ js/globalThis) #js {:platform "macos" :edition "direct"})
  (is (true? (rt/direct-edition?)))
  (is (= "jvm" (rt/default-choice)))
  (set! (.-__CG_NATIVE__ js/globalThis) #js {:platform "macos" :edition "appstore"})
  (is (false? (rt/direct-edition?)))
  (is (= "auto" (rt/default-choice)))
  (js-delete js/globalThis "__CG_NATIVE__")
  nil)

(deftest ready-gate-latches-on-first-ok
  ;; A later down-blip must not show "connecting to engine" again.
  (is (= :connecting (rt/ready-gate false false)))
  (is (= :ready (rt/ready-gate false true)))
  (is (= :ready (rt/ready-gate true false)))
  (is (= :ready (rt/ready-gate true true))))

(deftest observe-health-latches-first-ready
  (reset! rt/!ever-ready false)
  (is (= :connecting (rt/observe-health! false)))
  (is (false? (rt/ready?)))
  (is (= :ready (rt/observe-health! true)))
  (is (true? (rt/ready?)))
  (is (= :ready (rt/observe-health! false)) "latched: stays ready after a down-blip")
  (is (true? (rt/ready?)))
  (reset! rt/!ever-ready false))

;; Keep the mutating tests last: they write module state and storage.
(deftest set-choice-and-re-resolve-update-resolution
  (rt/set-choice! "auto" false)
  (is (= "js" (rt/resolved-runtime)))
  (rt/re-resolve! true)
  (is (= "jvm" (rt/resolved-runtime)))
  (rt/set-choice! "js" true)
  (is (= "js" (rt/resolved-runtime))))

(deftest js-only-pins-the-js-backend
  (js-delete js/globalThis "__CG_JS_ONLY__")
  (is (false? (rt/js-only?)))
  (set! (.-__CG_JS_ONLY__ js/globalThis) true)
  (try
    (is (true? (rt/js-only?)))
    (is (= "js" (rt/init!)))
    (is (= "js" (rt/re-resolve! true)) "a reachable backend does not move a JS-only page")
    (finally
      (js-delete js/globalThis "__CG_JS_ONLY__")
      (rt/init!)
      nil)))

(.then (js/Promise.resolve (t/run-tests))
       (fn [results]
         (let [fail (or (get results "fail") 0)
               err  (or (get results "error") 0)]
           (when (pos? (+ fail err))
             (.exit js/process 1)))))
