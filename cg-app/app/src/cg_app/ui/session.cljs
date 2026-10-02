;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.session
  "The health loop and the backend session routes. The loop probes while the
   backend is up too, because it must see the backend go down."
  (:require [cg-app.ui.state :as state]
            [cg-app.ui.runtime :as rt]))

(def default-interval-ms 30000)
(def probe-timeout-ms 2000)
;; A fast poll while the shell waits for the backend, because the JVM boots in
;; about 2 s and the default interval is too slow for the first connect.
(def connect-poll-interval-ms 500)
(def connect-poll-max-attempts 40)

(defonce !loop-id (atom nil))

(defn- sleep [ms] (js/Promise. (fn [res _] (js/setTimeout res ms))))

(defn- probe-signal []
  (try (js/AbortSignal.timeout probe-timeout-ms)
       (catch :default _ js/undefined)))

(defn sync-to-state! []
  (swap! state/app-state assoc
         :runtime-choice (rt/current-choice)
         :runtime-resolved (rt/resolved-runtime)
         :backend-url (rt/backend-url)))

(defn ^:async check-backend!
  "Probe <backend-url>/api/health. Resolves to a boolean; never rejects."
  []
  (try
    (let [resp (await (js/fetch (str (rt/backend-url) "/api/health")
                                #js {:method "GET" :signal (probe-signal)}))]
      (.-ok resp))
    (catch :default _ false)))

(defn ^:async probe-now!
  "One probe; feed the result to runtime re-resolution and the store.
   Returns the boolean."
  []
  (let [ok (await (check-backend!))]
    (rt/re-resolve! ok)
    (rt/observe-health! ok)
    (swap! state/app-state assoc
           :backend-available ok
           :backend-ready? (rt/ready?))
    (sync-to-state!)
    ok))

(defn stop-health-loop! []
  (when-let [id @!loop-id]
    (js/clearInterval id))
  (reset! !loop-id nil)
  nil)

(defn start-health-loop!
  "Probes every `interval-ms`, also while the backend is available."
  [interval-ms]
  (stop-health-loop!)
  (reset! !loop-id (js/setInterval (fn [] (probe-now!)) (or interval-ms default-interval-ms)))
  @!loop-id)

(defn ^:async set-runtime!
  "Sets the runtime choice. `auto` probes immediately. An explicit choice
   resolves against the last known availability."
  [choice]
  (if (= choice "auto")
    (let [ok (await (check-backend!))]
      (rt/set-choice! choice ok)
      (swap! state/app-state assoc :backend-available ok))
    (rt/set-choice! choice (boolean (:backend-available @state/app-state))))
  (sync-to-state!)
  (rt/resolved-runtime))

(defn ^:async set-backend-url!
  "Sets the backend url and probes immediately, because a new url makes the
   last reachability result invalid."
  [url]
  (rt/set-backend-url! url)
  (sync-to-state!)
  (await (probe-now!)))

(defn ^:async connect-then-steady!
  "Probes every connect-poll-interval-ms until the backend first answers, or
   for `attempts-left` probes, then starts the steady loop."
  [steady-interval-ms attempts-left]
  (let [ok (await (probe-now!))]
    (if (or ok (rt/ready?) (<= attempts-left 0))
      (start-health-loop! steady-interval-ms)
      (do (await (sleep connect-poll-interval-ms))
          (await (connect-then-steady! steady-interval-ms (dec attempts-left)))))))

(defn ^:async start!
  "Starts the health checks. The Direct edition polls fast until the JVM
   answers. A JS-only page starts none. A nil `interval-ms` means the default."
  [interval-ms]
  (cond
    (rt/js-only?) nil
    (rt/direct-edition?) (await (connect-then-steady! interval-ms connect-poll-max-attempts))
    :else (do (start-health-loop! interval-ms)
              (await (probe-now!)))))

(defonce !session-id (atom nil))

(defn ^:async api-fetch
  "A JSON fetch of <backend-url>/api<path>. Sends X-Session-Id when a session
   exists. Throws on a non-ok response."
  [method path body]
  (let [headers #js {"Content-Type" "application/json"}]
    (when @!session-id
      (aset headers "X-Session-Id" @!session-id))
    (let [opts #js {:method method :headers headers}]
      (when body
        (aset opts "body" (js/JSON.stringify body)))
      (let [resp (await (js/fetch (str (rt/backend-url) "/api" path) opts))]
        (if (.-ok resp)
          (await (.json resp))
          (throw (js/Error. (str "HTTP " (.-status resp)))))))))

(defn ^:async ensure-session!
  "Create a backend session on first call; later calls reuse it."
  []
  (when (nil? @!session-id)
    (let [resp (await (api-fetch "POST" "/session/create" nil))]
      (reset! !session-id (aget resp "session-id"))))
  @!session-id)

(defn ^:async load-session-code!
  "POSTs the loaded .cg text for dataset introspection and stores the vars for
   the sidebar. A failure warns and keeps the previous vars."
  [code filename]
  (state/set-session-loading! true)
  (try
    (let [resp (await (api-fetch "POST" "/session/load"
                                 #js {:code code :filename filename}))]
      (state/set-session-vars! (aget resp "vars"))
      (aget resp "vars"))
    (catch :default e
      (js/console.warn "session load failed:" e)
      nil)
    (finally
      (state/set-session-loading! false))))

(defn ^:async fetch-var-data!
  "The GeoJSON FeatureCollection of a session dataset var (EPSG:4326, at most
   5000 rows)."
  [var-name]
  (await (api-fetch "POST" (str "/data/" var-name) #js {:limit 5000})))

(defn ^:async sync-file!
  "File-load hook: ensure a session, then load the code for introspection."
  [code filename]
  (await (ensure-session!))
  (await (load-session-code! code filename)))
