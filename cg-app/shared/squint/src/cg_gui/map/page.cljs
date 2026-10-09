;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.map.page
  "The page of the cg-vscode CG Map tab. It runs a .cg on the JS backend,
   draws the layers of a run or of cg/layers, and evaluates the forms of the
   JS REPL. `post` sends a message to the host."
  (:require [cg-gui.map.core :as mc]
            [cg-gui.map.popup :as popup]
            [cg-gui.map.queue :as queue]
            [cg-gui.run.js :as run-js]))

(defn- status! [text]
  (when-let [e (js/document.getElementById "cg-exec-status")]
    (set! (.-textContent e) text)))

(defn- error-text [e]
  (str (or (when e (.-message e)) e)))

(defn- console-text
  "The text of one console argument: the head of the stack of an error,
   because its message alone does not locate it."
  [x]
  (cond
    (instance? js/Error x) (.join (.slice (.split (or (.-stack x) (.-message x)) "\n") 0 5) "\n")
    (string? x) x
    :else (try (js/JSON.stringify x) (catch :default _ (str x)))))

;; True while a run is on: its console.log text is the output of the run. The
;; map libraries also log, and their text is not.
(def ^:private !in-run (atom false))

(defn- forward-errors!
  "Only the webview developer tools show the console of the page."
  [post]
  (let [send (fn [level xs]
               (post #js {:type "cg/map-error" :level level
                          :error (.join (.map xs console-text) " ")}))]
    (.addEventListener js/window "error"
                       (fn [^js e] (send "error" #js [(or (.-error e) (.-message e))])))
    (.addEventListener js/window "unhandledrejection"
                       (fn [^js e] (send "error" #js [(.-reason e)])))
    (doseq [level ["error" "warn" "log"]]
      (let [original (aget js/console level)]
        (aset js/console level
              (fn [& xs]
                (.apply original js/console (to-array xs))
                (when (or (not= "log" level) @!in-run)
                  (send level (to-array xs)))))))))

;; load-module reads a module through the host, because the page cannot read
;; the files of the workspace.
(def ^:private module-waits (js/Map.))
(def ^:private !module-seq (atom 0))

(defn- module-reader [post]
  (fn read-module [path]
    (js/Promise.
     (fn [res rej]
       (let [id (swap! !module-seq inc)]
         (.set module-waits id #js {:res res :rej rej})
         (post #js {:type "cg/read-module" :id id :path path}))))))

(defn- answer-module! [^js m]
  (when-let [^js w (.get module-waits (.-id m))]
    (.delete module-waits (.-id m))
    (if (string? (.-text m))
      ((.-res w) (.-text m))
      ((.-rej w) (js/Error. (or (.-error m) (str "load-module " (.-path m) ": no file")))))))

(defn- timeout [ms msg]
  (js/Promise. (fn [_ rej] (js/setTimeout (fn [] (rej (js/Error. msg))) ms))))

(defn- ^:async start-map!
  "The map, or a failure after 25 s, because a basemap can fail to answer."
  [post]
  (let [m (await (js/Promise.race
                  #js [(js/Promise. (fn [res _] (mc/ensure-map! nil res)))
                       (timeout 25000 "map init timed out (basemap unreachable?)")]))]
    (mc/install-viewport-refetch! m (fn [bbox] (post #js {:type "cg/viewport-settled" :bbox bbox})) 300)
    m))

(defn- when-styled [^js m]
  (js/Promise. (fn [res _]
                 (if (.isStyleLoaded m)
                   (res m)
                   (.once m "idle" (fn [] (res m)))))))

(defn- set-visible! [^js m var-name shown?]
  (doseq [id (.getLayersOrder m)]
    (when (= var-name (.replace id #"^cg[a-z]+-" ""))
      (.setLayoutProperty m id "visibility" (if shown? "visible" "none")))))

;; The checkbox reads the visibility on the map, because a draw must keep the
;; choice of the user.
(defn- layer-toggle [^js m var-name]
  (let [cb (popup/el "input" nil nil)]
    (set! (.-type cb) "checkbox")
    (set! (.-checked cb) (not= "none" (.getLayoutProperty m (str "cgdata-" var-name) "visibility")))
    (.addEventListener cb "change" (fn [_] (set-visible! m var-name (.-checked cb))))
    (popup/append! (popup/el "label" "cg-map-legend-name" nil)
                   #js [cb (js/document.createTextNode var-name)])))

(defn- legend-row [kind ^js row]
  (let [sw (popup/el "span" "cg-map-swatch" nil)
        n (aget row "count")]
    (.setAttribute sw "data-kind" kind)
    (.setProperty (.-style sw) "--cg-swatch" (aget row "swatch-color"))
    (popup/append! (popup/el "li" "cg-map-legend-row" nil)
                   #js [sw
                        (popup/el "span" "cg-map-legend-label" (aget row "label"))
                        (popup/el "span" "cg-map-legend-count"
                                  (when (number? n) (.toLocaleString n "en-US")))])))

;; The JVM fetch stops at the row limit, and map-core shows a truncated layer
;; only from street zoom.
(defn- truncation-note [^js l]
  (let [total (when l (aget l "total-rows"))
        shown (when l (aget l "returned-rows"))]
    (when (and (number? total) (number? shown) (< shown total))
      (popup/el "div" "cg-map-legend-title"
                (str "showing " shown " of " total " rows; zoom to street level for all")))))

(defn- legend-section [^js m layers ^js legend]
  (let [var-name (aget legend "var")
        sec (popup/el "section" "cg-map-legend-layer" nil)]
    (.setAttribute sec "data-layer" var-name)
    (popup/append! sec #js [(layer-toggle m var-name)
                            (when-let [t (aget legend "title")]
                              (popup/el "div" "cg-map-legend-title" t))
                            (truncation-note (.find layers (fn [l] (= var-name (aget l "var")))))
                            (popup/append! (popup/el "ul" "cg-map-legend-rows" nil)
                                           (.map (aget legend "rows")
                                                 (fn [row] (legend-row (aget legend "layer-type") row))))])))

(defn- render-legend! [^js m layers]
  (when-let [box (js/document.getElementById "cg-map-legend")]
    (let [legends (mc/legends layers)]
      (.replaceChildren box)
      (popup/append! box (.map legends (fn [lg] (legend-section m layers lg))))
      (set! (.-hidden box) (zero? (.-length legends))))))

(defn- ^:async draw! [map-ready layers]
  (let [m (await (when-styled (await map-ready)))]
    (mc/sync-layers! m layers)
    (render-legend! m layers)))

(defn- draw-failed! [ctx e]
  (status! (str "map draw failed: " (error-text e)))
  ((:post ctx) #js {:type "cg/map-error" :error (error-text e)}))

;; The run ends before the draw, and the next run does not wait for the draw,
;; because a basemap that does not answer holds the draw for 25 s.
(defn- ^:async run-cg! [ctx ^js m]
  (status! (str "running " (or (.-model m) ".cg") "..."))
  (reset! !in-run true)
  (try
    (let [mod (:mod ctx)
          r (await (run-js/run-js! mod (.-text m) (.-flow m)
                                   {:post (:post ctx) :urls (:urls ctx)
                                    :read-module (:read-module ctx)
                                    :model (.-model m) :file (.-file m)}))
          layers (run-js/run-result->layers mod r)
          layer (last layers)
          n (aget r "count")]
      ((:post ctx) #js {:type "cg/run-ended" :outcome "success" :count n
                        :layer (when layer (aget layer "var"))})
      (if layer
        (.then (draw! (:map-ready ctx) layers)
               (fn [_] (status! (str "run layer: cgdata-" (aget layer "var") " (" n " result(s))")))
               (fn [e] (draw-failed! ctx e)))
        (status! (str "run ok (" n " result(s)), no map layer"))))
    (catch :default e
      (status! (str "run failed: " (error-text e))))
    (finally
      (reset! !in-run false))))

(defn- ^:async repl-eval! [ctx ^js m]
  (let [r (await (run-js/repl-eval! (:mod ctx) (.-code m) (:read-module ctx)))]
    (aset r "type" "cg/repl-result")
    (aset r "id" (.-id m))
    ((:post ctx) r)))

(defn- draw-jvm-layers! [ctx layers]
  (.then (draw! (:map-ready ctx) layers)
         (fn [_] (status! (str (.-length layers) " layer(s) from the JVM session")))
         (fn [e] (draw-failed! ctx e))))

(defn- ^:async boot!
  "Gives the context of a run."
  [^js opts]
  (let [post (.-post opts)
        _ (await (run-js/ensure-wasmts! nil))
        mod (await (js* "import(~{})" (.-cgBundle opts)))
        map-ready (start-map! post)]
    (.catch map-ready (fn [e] (post #js {:type "cg/map-error" :error (error-text e)})))
    (status! "ready, waiting for a .cg run")
    (post #js {:type "cg/ready"})
    {:post post :mod mod :urls (.-urls opts) :map-ready map-ready
     :read-module (module-reader post)}))

(defn start!
  "Starts the page. opts: post, cgBundle (the URL of the cg browser bundle)
   and urls (the wasmts-js and proj-db URLs of run-js/ensure-workers!)."
  [^js opts]
  (forward-errors! (.-post opts))
  (let [post (.-post opts)
        boot (boot! opts)
        jobs (queue/make boot post)]
    (.catch boot (fn [e] (post #js {:type "cg/map-error" :error (error-text e)})))
    (.addEventListener js/window "message"
                       (fn [^js e]
                         (let [m (.-data e)]
                           (case (when m (.-type m))
                             "cg/run" (queue/enqueue! jobs run-cg! m true)
                             "cg/repl-eval" (queue/enqueue! jobs repl-eval! m false)
                             "cg/layers" (.then boot (fn [ctx] (draw-jvm-layers! ctx (.-layers m))))
                             "cg/status" (status! (.-text m))
                             "cg/module-text" (answer-module! m)
                             nil))))))
