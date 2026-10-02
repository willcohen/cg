;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.state
  "The app-shell store: one atom and a useSyncExternalStore adapter. Storage
   and matchMedia access are guarded, because the node tests have no DOM."
  (:require ["react" :refer [useSyncExternalStore]]))

(defonce app-state (atom {:theme "white" :loaded-file nil :load-error nil
                          :flow nil :positions #js {} :module-status "not_loaded"
                          :panels #js {:flow true :code true :status false
                                       :map false :results false}
                          ;; The model that Run runs; nil runs the last one.
                          :run-model nil
                          :report-result nil
                          ;; A single-value run result for the badge, as
                          ;; #js {:text :name}.
                          :scalar-result nil
                          :sidebar-open? false
                          :examples nil
                          :map-layers #js []
                          :session-vars #js []
                          :session-loading? false}))

(defonce ^:private sub-n (atom 0))

(defn subscribe
  "Register a change callback; returns an unsubscribe thunk."
  [cb]
  (let [k (str "sub-" (swap! sub-n inc))]
    (add-watch app-state k (fn [_ _ _ _] (cb)))
    (fn [] (remove-watch app-state k))))

(defn use-app-state []
  (useSyncExternalStore subscribe (fn [] @app-state)))

(def ^:private theme-storage-key "cg-theme")

(defn- storage [] (aget js/globalThis "localStorage"))

(defn- prefers-dark? []
  (try (.-matches (js/window.matchMedia "(prefers-color-scheme: dark)"))
       (catch :default _ false)))

(defn- read-stored-theme []
  (try (some-> (storage) (.getItem theme-storage-key)) (catch :default _ nil)))

(defn- persist-theme! [t]
  (try (some-> (storage) (.setItem theme-storage-key t)) (catch :default _ nil)))

(defn theme-mode
  "Carbon theme -> cg-brand mode."
  [t]
  (if (or (= t "g100") (= t "g90")) "dark" "light"))

(defn init-theme!
  "Sets the theme: the stored choice, else the OS preference. Call it before
   the mount, because the first paint needs the theme."
  []
  (let [resolved (or (read-stored-theme) (if (prefers-dark?) "g100" "white"))]
    (swap! app-state assoc :theme resolved)
    resolved))

(defn set-theme! [t] (swap! app-state assoc :theme t) t)

(defn- clear-stored-theme! []
  (try (some-> (storage) (.removeItem theme-storage-key)) (catch :default _ nil)))

(defn toggle-theme!
  "Flips the theme, and stores it only when it differs from the OS
   preference."
  []
  (let [nt (if (= (:theme @app-state) "g100") "white" "g100")]
    (if (= (theme-mode nt) (if (prefers-dark?) "dark" "light"))
      (clear-stored-theme!)
      (persist-theme! nt))
    (swap! app-state assoc :theme nt)
    nt))

(defn- without-run-output
  "`st` with no map layers or run results, and the next :load-n."
  [st]
  (assoc st :map-layers #js [] :report-result nil :scalar-result nil
         :load-n (inc (or (:load-n st) 0))))

(defn set-loaded-flow!
  "Stores the parsed flow and the file (name and text). Clears the run
   status, the model pick and the output of the last run."
  [filename text flow]
  (swap! app-state
         (fn [st]
           (assoc (without-run-output st)
                  :flow flow :loaded-file {:name filename :text text}
                  :load-error nil :exec nil :run-model nil))))

(defn set-edited-text!
  "Stores an edited text and `flow`, its parse, or a nil `flow` and the reason
   in `unparsed`. Keeps :load-n, and the canvas keeps its view."
  [text flow unparsed]
  (swap! app-state
         (fn [st]
           (cond-> (assoc st :loaded-file (assoc (:loaded-file st) :text text :unparsed unparsed))
             flow (assoc :flow flow)))))

(defn diagnostics
  "The diagnostics of the loaded flow, as a JS array. Text that does not
   parse adds one error, first."
  [st]
  (let [diags (or (when-let [flow (:flow st)] (aget flow "diagnostics")) #js [])]
    (if-let [unparsed (:unparsed (:loaded-file st))]
      (.concat #js [#js {:code "unparsed-text" :severity "error"
                         :message (str "the code has " unparsed
                                       "; the canvas shows the last text that parsed")}]
               diags)
      diags)))

(defn set-examples!
  "Stores the names in examples/index.json."
  [names]
  (swap! app-state assoc :examples names))

(defn starter-example
  "simple.cg when `names` has it, else the first name, else nil."
  [names]
  (when (and names (pos? (.-length names)))
    (if (.includes names "simple.cg") "simple.cg" (aget names 0))))

(defn show-url-bar!
  "Shows the URL text box with `url` in it and `error`, or nil, under it."
  [url error]
  (swap! app-state assoc :url-bar {:url url :error error}))

(defn hide-url-bar! [] (swap! app-state assoc :url-bar nil))

(defn toggle-url-bar! []
  (swap! app-state assoc :url-bar (when-not (:url-bar @app-state) {:url "" :error nil})))

(defn set-opening!
  "Stores the name of the file that the app gets from a URL, or nil."
  [name]
  (swap! app-state assoc :opening name))

(defn set-run-model!
  "Sets the binding that Run runs."
  [model-name]
  (swap! app-state assoc :run-model model-name))

(defn clear-loaded!
  "Returns to the no-file state. Clears the positions, because old drags must
   not win over the overlay of the next file."
  []
  (swap! app-state
         (fn [st]
           (assoc (without-run-output st)
                  :flow nil :loaded-file nil :positions #js {}
                  :load-error nil :exec nil :run-model nil))))

(defn clear-exec!
  "Drops the status of the last run."
  []
  (swap! app-state assoc :exec nil))

(defn toggle-settings! []
  (swap! app-state assoc :settings-open (not (:settings-open @app-state)))
  (:settings-open @app-state))

(defn set-load-error! [msg]
  (swap! app-state assoc :load-error msg))

(defn set-module-status! [s] (swap! app-state assoc :module-status s))

(defn set-backend-flapping!
  "Stores the flap signal of the native supervisor: the backend restarted too
   many times, and the shell shows a banner."
  [b]
  (swap! app-state assoc :backend-flapping? (boolean b)))

(defn positions [] (:positions @app-state))

(defn set-position!
  "Records the [x y] of one node. Writes a new object, because subscribers
   fire only on a new value."
  [node-id pos]
  (let [nxt (js/Object.assign #js {} (:positions @app-state))]
    (aset nxt node-id pos)
    (swap! app-state assoc :positions nxt)))

(defn merge-positions!
  "Adds many positions (from a file overlay). Existing keys win, because a
   drag in this session outranks the overlay."
  [table]
  (let [nxt (js/Object.assign #js {} table (:positions @app-state))]
    (swap! app-state assoc :positions nxt)))

(defn panels [] (:panels @app-state))

(defn toggle-panel!
  "Toggles the visibility of one panel. Returns the new boolean."
  [k]
  (let [nxt (js/Object.assign #js {} (:panels @app-state))]
    (aset nxt k (not (aget nxt k)))
    (swap! app-state assoc :panels nxt)
    (aget nxt k)))

(defn select-panel!
  "Show exactly one panel (phone mode)."
  [k]
  (let [nxt #js {:flow false :code false :status false :map false :results false}]
    (aset nxt k true)
    (swap! app-state assoc :panels nxt))
  k)

(defn set-report-result!
  "Stores the report table of the last run ({:title :columns :rows} JS object)
   for the results panel. nil clears it."
  [t]
  (swap! app-state assoc :report-result t))

(defn set-scalar-result!
  "Stores the scalar result of the last run (#js {:text :name}) for the badge
   of the results panel. nil clears it."
  [v]
  (swap! app-state assoc :scalar-result v))

(defn clear-run-results!
  "Clears the report and the scalar result at the start of a run, because a
   result from an earlier run must not show as the output of this one."
  []
  (swap! app-state assoc :report-result nil :scalar-result nil))

(defn toggle-sidebar! []
  (swap! app-state assoc :sidebar-open? (not (:sidebar-open? @app-state)))
  (:sidebar-open? @app-state))

(defn set-session-vars! [vars]
  (swap! app-state assoc :session-vars (or vars #js [])))

(defn set-session-loading! [b]
  (swap! app-state assoc :session-loading? (boolean b)))

(defn get-layer [var-name]
  (.find (:map-layers @app-state)
         (fn [l] (= var-name (aget l "var")))))

(defn add-map-layer!
  "Adds or replaces a layer entry and makes it visible. `spec` is the spec of
   the map-layer sink when the flow ends in one, else nil."
  ([var-name color crs geojson] (add-map-layer! var-name color crs geojson nil))
  ([var-name color crs geojson spec]
  (let [entry #js {:var var-name :color color :visible? true
                   :crs crs :geojson geojson :spec spec}
        cur (:map-layers @app-state)
        found (.some cur (fn [l] (= var-name (aget l "var"))))
        nxt (if found
              (.map cur (fn [l] (if (= var-name (aget l "var")) entry l)))
              (.concat cur #js [entry]))]
    (swap! app-state assoc :map-layers nxt))))

(defn remove-map-layer!
  "Hides a layer and drops its geojson. The entry stays, because its color
   must survive a later show."
  [var-name]
  (let [nxt (.map (:map-layers @app-state)
                  (fn [l]
                    (if (= var-name (aget l "var"))
                      (js/Object.assign #js {} l #js {:visible? false :geojson nil})
                      l)))]
    (swap! app-state assoc :map-layers nxt)))

(defn set-layer-visible!
  "Shows or hides a layer and keeps its geojson, because a run layer has no
   source to fetch it from again."
  [var-name visible?]
  (let [nxt (.map (:map-layers @app-state)
                  (fn [l]
                    (if (= var-name (aget l "var"))
                      (js/Object.assign #js {} l #js {:visible? (boolean visible?)})
                      l)))]
    (swap! app-state assoc :map-layers nxt)))

(defn visible-layers []
  (.filter (:map-layers @app-state) (fn [l] (true? (aget l "visible?")))))

(defn- retire-cleanup
  "The retired cleanups of `st` plus its current projection cleanup. The map
   requests tiles through them until the next style is in."
  [st]
  (let [retired (or (:map-retired-cleanups st) [])]
    (if-let [old (:map-cleanup st)]
      (conj retired old)
      retired)))

(defn set-map-projection!
  "Stores a reprojected style and retires the previous cleanup with no run. A
   reprojection to the same CRS reuses its tile protocols and drops it."
  [proj]
  (let [st @app-state
        same-crs? (= (aget proj "crs") (:map-crs st))]
    (swap! app-state assoc
           :map-crs (aget proj "crs")
           :map-style (aget proj "style")
           :map-bounds (aget proj "bounds")
           :map-max-bounds (aget proj "maxBounds")
           :map-transformer (aget proj "transformer")
           :map-cleanup (aget proj "cleanup")
           :map-retired-cleanups (if same-crs?
                                   (:map-retired-cleanups st)
                                   (retire-cleanup st)))))

(defn start-crs-switch!
  "Marks the switch to `crs` as pending. The CRS button keeps the old CRS."
  [crs]
  (swap! app-state assoc :map-crs-pending crs :map-crs-error nil))

(defn crs-switch-pending? [crs] (= crs (:map-crs-pending @app-state)))

(defn end-crs-switch!
  "Ends the switch to `crs`, with `message` as its failure or nil. Does
   nothing for a switch that a later pick replaced."
  [crs message]
  (when (crs-switch-pending? crs)
    (swap! app-state assoc :map-crs-pending nil
           :map-crs-error (when message #js {:crs crs :message message}))))

(defn dismiss-crs-error! [] (swap! app-state assoc :map-crs-error nil))

(defn clear-map-projection!
  "Returns to the default Mercator. Retires the current cleanup."
  []
  (let [st @app-state]
    (swap! app-state assoc
           :map-crs nil :map-style nil :map-bounds nil
           :map-max-bounds nil :map-transformer nil :map-cleanup nil
           :map-retired-cleanups (retire-cleanup st))))

(defn take-retired-cleanups!
  "The retired projection cleanups, removed from the state."
  []
  (let [fs (:map-retired-cleanups @app-state)]
    (swap! app-state assoc :map-retired-cleanups nil)
    (or fs [])))
