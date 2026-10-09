;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.session
  "The health loop and the backend session routes. The loop probes while the
   backend is up too, because it must see the backend go down."
  (:require [cg-app.ui.state :as state]
            [cg-app.ui.runtime :as rt]
            ["../../../../shared/squint/src/cg_gui/run/jvm.mjs" :as jvm]))

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

(defn- send-with
  "The transport of the JVM client: fetch to the backend url, with a JSON
   body and the fetch options `extra`."
  [extra]
  (fn ^:async send [method path headers body]
    (let [opts (js/Object.assign #js {:method method
                                      :headers (js/Object.assign #js {"Content-Type" "application/json"}
                                                                 headers)}
                                 extra)]
      (when body (aset opts "body" (js/JSON.stringify body)))
      (let [resp (await (js/fetch (str (rt/backend-url) path) opts))]
        #js {:status (.-status resp)
             :body (try (await (.json resp)) (catch :default _ nil))}))))

(def send (send-with nil))

(defn sync-to-state! []
  (swap! state/app-state assoc
         :runtime-choice (rt/current-choice)
         :runtime-resolved (rt/resolved-runtime)
         :backend-url (rt/backend-url)))

(defn ^:async check-backend!
  "Probe <backend-url>/api/health. Resolves to a boolean; never rejects."
  []
  (await (jvm/health? (send-with #js {:signal (probe-signal)}))))

(defn ^:async probe-now!
  "Resolves to the result of check-backend!."
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
  (reset! !loop-id (js/setInterval (fn [] (probe-now!)) (or interval-ms default-interval-ms))))

(defn ^:async set-runtime!
  "`auto` probes immediately. An explicit choice resolves against the last
   known availability."
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

(defn ^:async in-session
  "(f sid) with the session of the app, in a new session when it expired."
  [f]
  (await (jvm/with-session send !session-id f)))

(defn ^:async load-session-code!
  "Loads the .cg text in the session for dataset introspection and stores the
   vars for the sidebar. A failure warns and keeps the previous vars."
  [code filename]
  (state/set-session-loading! true)
  (try
    (let [r (await (in-session (fn [sid] (jvm/load-session send sid code filename))))]
      (if-let [e (:error r)]
        (js/console.warn "session load failed:" e)
        (do (state/set-session-vars! (:ok r))
            (:ok r))))
    (catch :default e
      (js/console.warn "session load failed:" e)
      nil)
    (finally
      (state/set-session-loading! false))))

(defn ^:async fetch-var-data!
  "The GeoJSON FeatureCollection of a session dataset var (EPSG:4326, at most
   5000 rows). Throws on an error."
  [var-name]
  (let [r (await (in-session (fn [sid] (jvm/fetch-dataset send sid var-name {:limit 5000}))))]
    (or (:fc r) (throw (js/Error. (:error r))))))
