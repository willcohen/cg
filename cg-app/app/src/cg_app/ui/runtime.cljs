;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.runtime
  "Runtime selection for the app shell. An explicit choice wins, and auto
   follows backend reachability. !backend-url is the only source of the url."
  (:require ["../../../../shared/squint/src/cg_gui/run/lane.mjs" :as lane]))

(def runtime-storage-key "cg-runtime")
(def backend-url-storage-key "cg-backend-url")
(def default-backend-url "http://localhost:3000")

(def !choice (atom "auto"))
(def !resolved (atom "js"))
(def !backend-url (atom default-backend-url))

;; With site data blocked, the browser throws SecurityError when the page
;; reads localStorage. Then the app keeps its choices in memory only.
(defn storage
  "localStorage, or nil when the host has none or blocks it."
  []
  (try (.-localStorage js/globalThis) (catch :default _ nil)))

;; setItem throws an error when the storage is full.
(defn- store! [k v]
  (try (some-> (storage) (.setItem k v)) (catch :default _ nil)))

(defn direct-edition?
  "True in the macOS Direct build, where window.__CG_NATIVE__ has edition
   'direct'."
  []
  (let [n (aget js/globalThis "__CG_NATIVE__")]
    (boolean (and n (= "direct" (aget n "edition"))))))

(defn js-only?
  "True on a page that has no backend, such as the GitHub Pages demo. The
   page sets window.__CG_JS_ONLY__ before the app loads."
  []
  (true? (aget js/globalThis "__CG_JS_ONLY__")))

(defn default-choice
  "The choice when none is stored: jvm for the Direct edition, auto for all
   others."
  []
  (if (direct-edition?) "jvm" "auto"))

(defn current-choice [] @!choice)
(defn resolved-runtime [] @!resolved)
(defn backend-url [] @!backend-url)

(defn init!
  "Loads the stored choice and backend url. Returns the effective choice."
  []
  (reset! !choice (if (js-only?)
                    "js"
                    (or (some-> (storage) (.getItem runtime-storage-key)) (default-choice))))
  ;; The url that the native shell injects wins over a stored url, because
  ;; the stored port can be stale.
  (reset! !backend-url (or (.-__CG_BACKEND_URL__ js/globalThis)
                           (some-> (storage) (.getItem backend-url-storage-key))
                           default-backend-url))
  @!choice)

(defn set-choice!
  "Stores the choice, then resolves. Returns the resolved runtime."
  [choice jvm-reachable?]
  (reset! !choice choice)
  (store! runtime-storage-key choice)
  (reset! !resolved (lane/resolve-runtime choice jvm-reachable?)))

(defn re-resolve!
  "Resolves the runtime for one reachability result. Returns the resolved
   runtime."
  [jvm-reachable?]
  (reset! !resolved (lane/resolve-runtime @!choice jvm-reachable?)))

(def !ever-ready (atom false))

(defn observe-health!
  "Records the first ready result."
  [backend-available?]
  (when backend-available? (reset! !ever-ready true)))

(defn ready?
  "True once the backend has answered at least once this run."
  []
  @!ever-ready)

(defn set-backend-url!
  "Stores the backend url. Returns the effective url. session/set-backend-url!
   adds the probe."
  [url]
  (reset! !backend-url (or url default-backend-url))
  (store! backend-url-storage-key @!backend-url)
  @!backend-url)
