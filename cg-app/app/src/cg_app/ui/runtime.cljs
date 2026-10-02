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

(defn storage
  "localStorage, or nil under node tests. Reads it from globalThis, because a
   bare js/localStorage throws ReferenceError under node."
  []
  (.-localStorage js/globalThis))

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
  (let [ls (storage)]
    (reset! !choice (if (js-only?)
                      "js"
                      (or (when ls (.getItem ls runtime-storage-key)) (default-choice))))
    ;; The url that the native shell injects wins over a stored url, because
    ;; the stored port can be stale.
    (reset! !backend-url (or (.-__CG_BACKEND_URL__ js/globalThis)
                             (when ls (.getItem ls backend-url-storage-key))
                             default-backend-url))
    @!choice))

(defn set-choice!
  "Stores the choice, then resolves. Returns the resolved runtime."
  [choice jvm-reachable?]
  (reset! !choice choice)
  (when-let [ls (storage)]
    (.setItem ls runtime-storage-key choice))
  (reset! !resolved (lane/resolve-runtime choice jvm-reachable?))
  @!resolved)

(defn re-resolve!
  "Resolves the runtime for one reachability result. Returns the resolved
   runtime."
  [jvm-reachable?]
  (reset! !resolved (lane/resolve-runtime @!choice jvm-reachable?))
  @!resolved)

(def !ever-ready (atom false))

(defn ready-gate
  "The readiness gate: :connecting until the backend first answers, then
   :ready. After the first answer the gate stays open."
  [ever-ready? backend-available?]
  (if (or ever-ready? backend-available?) :ready :connecting))

(defn observe-health!
  "Records the first ready result and returns :ready or :connecting."
  [backend-available?]
  (when backend-available? (reset! !ever-ready true))
  (ready-gate @!ever-ready backend-available?))

(defn ready?
  "True once the backend has answered at least once this run."
  []
  @!ever-ready)

(defn set-backend-url!
  "Stores the backend url. Returns the effective url. session/set-backend-url!
   adds the probe."
  [url]
  (reset! !backend-url (or url default-backend-url))
  (when-let [ls (storage)]
    (.setItem ls backend-url-storage-key @!backend-url))
  @!backend-url)
