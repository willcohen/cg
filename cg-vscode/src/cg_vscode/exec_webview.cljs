;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.exec-webview
  "The 'CG Map' webview tab. It runs a .cg on the JS backend in the page, or in
   the JVM session of the tab, and draws its layers on a MapLibre map."
  (:require ["vscode" :as vscode]
            ["path" :as path]
            [cg-vscode.asset-server :as asset-server]
            [cg-vscode.exec-html :as exec-html]
            [cg-vscode.extension.lifecycle :as lc]
            [cg-vscode.extract :as extract]
            [cg-vscode.modules :as modules]
            ["../../../cg-app/shared/squint/src/cg_gui/run/lane.mjs" :as lane]
            ["../../../cg-app/shared/squint/src/cg_gui/run/js.mjs" :as run-js]
            [cg-vscode.run-relay :as relay]
            [cg-vscode.run-status :as run-status]
            [cg-vscode.runtime :as runtime]
            [cg-vscode.runtime.jvm-client :as jvm-client]
            ["../../../cg-app/shared/squint/src/cg_gui/run/jvm.mjs" :as jvm]
            [cg-vscode.doc :as doc]
            [cg-vscode.repl.calva :as calva]
            [cg-vscode.repl.js-repl :as js-repl]))

(defonce ^:private !panel (atom nil))
(defonce ^:private !ready (atom false))
(defonce ^:private !last-run (atom nil))
;; {:uri :file} of each JS run in the tab, oldest first. The run messages go
;; to the canvas of the first one.
(defonce ^:private !run-docs (atom []))
(defonce ^:private !map-session (atom nil))
(defonce ^:private !jvm-port (atom nil))
(defonce ^:private !last-vars (atom nil))
;; A newer viewport refetch supersedes one in flight.
(defonce ^:private !refetch-gen (atom 0))
;; The Promise of the nREPL server of the JS REPL, and the evals that wait
;; for the page: id -> resolve.
(defonce ^:private !js-repl (atom nil))
(defonce ^:private repl-waits (js/Map.))
(defonce ^:private !repl-seq (atom 0))
(defonce ^:private !ready-sends (atom []))

(defn- resolve-dirs [^js extension-uri]
  (let [repo (path/resolve (.-fsPath extension-uri) "..")]
    {:public-dir (path/join repo "cg-app" "shared" "ui" "public")
     :wasmts-dir (path/join repo "cg" "node_modules" "@wcohen" "wasmts" "dist")
     :cg-dist-dir (path/join repo "cg" "dist")
     :grammar-dir (path/join repo "cg" "node_modules" "@yogthos" "tree-sitter-clojure")}))

(defn- post-pending! [^js panel]
  (when-let [^js m @!last-run]
    (.. panel -webview (postMessage m))))

(defn- post-status! [^js panel ^js text]
  (.. panel -webview (postMessage #js {:type "cg/status" :text text})))

(defn- stash-and-post! [^js panel ^js msg]
  (reset! !last-run msg)
  (when @!ready (post-pending! panel)))

(defn- post-refused!
  "There is no fallback to JS."
  [^js panel]
  (reset! !last-run nil)
  (vscode/window.showErrorMessage
   "CG map: runtime is set to jvm but the CG backend is unreachable. Start it with `bb backend:dev`.")
  (post-status! panel "jvm runtime selected, backend unreachable"))

(defn ^:async with-tab-session!
  "(f send sid) with the JVM session of the tab on `port`, made when there is
   none, and again with a new session when `f` gives {:expired true}."
  [port f]
  (let [send (jvm-client/send-to port nil)]
    (await (jvm/with-session send !map-session (fn [sid] (f send sid))))))

(defn- render-jvm-result! [^js panel result]
  (let [layers (:layers result)]
    (cond
      (:error result)
      (vscode/window.showErrorMessage (str "CG map (JVM) run failed: " (:error result)))

      (and layers (pos? (.-length layers)))
      (stash-and-post! panel #js {:type "cg/layers" :layers layers})

      :else
      (post-status! panel "run ok, no geometry dataset vars to map. Open a file that defs dataset layers."))))

(defn- remember-fetched-vars!
  "A viewport refetch fetches these vars again."
  [result]
  (let [layers (:layers result)]
    (when (and layers (pos? (.-length layers)))
      (reset! !last-vars (.map layers (fn [^js l] (aget l "var")))))))

(defn ^:async run-jvm-feed! [^js panel port ^js document]
  (reset! !jvm-port port)
  ;; session/load has no response timeout, and a first load can take minutes.
  (post-status! panel "loading session (a first-run full analysis takes minutes)")
  (try
    (let [r (await (with-tab-session! port
                     (fn [send sid]
                       (jvm-client/load-layers send sid (.getText document)
                                               (path/basename (.. document -uri -fsPath))
                                               {:limit jvm-client/default-row-limit
                                                :root (modules/bb-root-for-document document)}))))]
      (remember-fetched-vars! r)
      (render-jvm-result! panel r))
    (catch :default e
      (vscode/window.showErrorMessage (str "CG map (JVM) failed: " (.-message e))))))

(defn ^:async handle-viewport-settled! [^js panel bbox]
  (let [sid  @!map-session
        port @!jvm-port
        vars @!last-vars]
    (when (and sid port vars)
      (swap! !refetch-gen inc)
      (let [gen @!refetch-gen]
        (try
          (let [r (await (jvm-client/fetch-layers (jvm-client/send-to port nil) sid vars
                                                  {:bbox bbox :limit jvm-client/default-row-limit}))]
            (cond
              (:expired r) (reset! !map-session nil)
              (= gen @!refetch-gen) (render-jvm-result! panel r)))
          (catch :default _ nil))))))

(defn- post-js-run! [!state ^js panel ^js cg-uri text flow]
  (let [uri-str (str cg-uri)
        file (.-fsPath cg-uri)]
    ;; The stash keeps only the last message for a page that is not ready.
    (if @!ready
      (swap! !run-docs conj {:uri uri-str :file file})
      (reset! !run-docs [{:uri uri-str :file file}]))
    (stash-and-post! panel #js {:type "cg/run" :text text :flow flow
                                :model (get-in @!state [:editor/run-models uri-str])
                                :file (path/basename file)})))

(defn ^:async run-js! [!state ^js panel ^js document]
  (let [text (.getText document)
        ^js uri (.-uri document)]
    (try
      (post-js-run! !state panel uri text (await (extract/extract-models text)))
      (catch :default e
        (run-status/post! !state (str uri)
                          #js {:type "cg/run-error"
                               :message (str "the parser failed: " (.-message e))})))))

(defn- jvm-run-request [!state ^js document]
  (let [^js cg-uri (.-uri document)
        model (get-in @!state [:editor/run-models (str cg-uri)])
        req #js {:code (.getText document) :filename (path/basename (.-fsPath cg-uri))
                 :root (modules/bb-root-for-document document)}]
    (when model (aset req "opts" #js {:name model :model model}))
    req))

(defn- end-jvm-run! [^js panel post file ^js res]
  (let [layers (run-js/run-result->layers nil res)
        n (aget res "count")]
    (doseq [m (relay/canvas-messages #js {:type "cg/run-ended" :outcome "success" :count n
                                          :layer (some-> (last layers) (aget "var"))
                                          :runtime "JVM"}
                                     file)]
      (post m))
    (if (pos? (.-length layers))
      (stash-and-post! panel #js {:type "cg/layers" :layers layers})
      (post-status! panel (str "run ok (" n " result(s)), no map layer")))))

(defn ^:async run-jvm!
  "Runs `document` in the JVM session of the tab, so that its datasets stay
   there."
  [!state ^js panel ^js document port]
  (let [^js uri (.-uri document)
        file (.-fsPath uri)
        post (fn [m] (run-status/post! !state (str uri) m))
        fail! (fn [message]
                (post #js {:type "cg/run-error" :message message})
                (vscode/window.showErrorMessage (str "CG map run failed: " message)))]
    (reset! !jvm-port port)
    ;; The viewport refetch belongs to Show Datasets.
    (reset! !last-vars nil)
    (post #js {:type "cg/run-started" :file file :runtime "JVM"})
    (post-status! panel "running in the JVM session")
    (try
      (let [req (jvm-run-request !state document)
            on-event (fn [evt] (post #js {:type "cg/trace-event" :event evt}))
            r (await (with-tab-session! port (fn [_ sid] (jvm-client/run-in-session port sid req on-event))))]
        (if-let [res (:result r)]
          (end-jvm-run! panel post file res)
          (fail! (:error r))))
      (catch :default e (fail! (.-message e))))))

(defn- backend-stage
  "The on-stage fn of runtime/ensure-backend! for a run of the document
   `uri-str`: the boot of the backend shows as a stage of the run."
  [!state uri-str]
  (fn [state]
    (run-status/post! !state uri-str
                      #js {:type "cg/trace-event"
                           :event #js {:kind "stage" :label "start the JVM backend"
                                       :state state :ts (js/Date.now)}})))

(defn ^:async run-route!
  "lane/execute-route of the runtime choice, with the :port of the backend.
   An explicit jvm choice starts the backend. auto uses it only when it is up,
   because a first boot takes minutes."
  [!state uri-str]
  (when (runtime/jvm-choice?)
    (await (runtime/ensure-backend! !state (backend-stage !state uri-str))))
  (let [{:keys [reachable port]} (await (runtime/resolve-map-target !state))]
    (assoc (lane/execute-route (:runtime/choice @!state) reachable) :port port)))

(defn- request! [!state ^js document runtime]
  (run-status/requested! !state (str (.-uri document)) (.. document -uri -fsPath) runtime))

(defn ^:async run-in-tab! [!state ^js panel ^js document]
  (request! !state document (runtime/config-runtime))
  (let [route (await (run-route! !state (str (.-uri document))))]
    (cond
      (:refused route) (do (post-refused! panel)
                           (run-status/post! !state (str (.-uri document))
                                             #js {:type "cg/run-error" :message (:reason route)}))
      (= "jvm" (:runtime route)) (await (run-jvm! !state panel document (:port route)))
      :else (await (run-js! !state panel document)))))

(defn ^:async answer-module-read! [^js panel ^js msg]
  (let [id (.-id msg)
        p (.-path msg)
        answer (fn [extra]
                 (.. panel -webview
                     (postMessage (js/Object.assign #js {:type "cg/module-text" :id id :path p}
                                                    extra))))]
    (try
      (let [uri-str (:uri (first @!run-docs))
            ^js document (when uri-str
                           (await (vscode/workspace.openTextDocument (vscode/Uri.parse uri-str))))
            abs (when document (modules/resolve-module-path document p))
            text (when abs (modules/read-text-safe! abs))]
        (if text
          (answer #js {:text text})
          (answer #js {:error (str "load-module " p ": no such file under the bb root of the .cg or in cg")})))
      (catch :default e
        (answer #js {:error (str "load-module " p ": " (.-message e))})))))

(defn- page-eval
  "Sends the REPL text `code` to the page of the tab after cg/ready, as a run
   waits for it. Gives a Promise of #js {:value :out} or #js {:err :out}."
  [code]
  (if-let [^js panel @!panel]
    (js/Promise.
     (fn [res _]
       (let [id (swap! !repl-seq inc)
             send #(.. panel -webview (postMessage #js {:type "cg/repl-eval" :id id :code code}))]
         (.set repl-waits id res)
         (if @!ready (send) (swap! !ready-sends conj send)))))
    (js/Promise.resolve #js {:err "the CG Map tab is not open. Open it with CG: Open Map." :out ""})))

(defn- answer-repl! [^js msg]
  (when-let [res (.get repl-waits (.-id msg))]
    (.delete repl-waits (.-id msg))
    (res msg)))

(defn- drop-repl-waits! []
  (.forEach repl-waits (fn [res _] (res #js {:err "the CG Map tab closed" :out ""})))
  (.clear repl-waits))

(defn- relay!
  "`end?` closes the run."
  [!state ^js msg end?]
  (when-let [{:keys [uri file]} (first @!run-docs)]
    (doseq [m (relay/canvas-messages msg file)]
      (run-status/post! !state uri m))
    (when end? (swap! !run-docs (fn [ds] (vec (rest ds)))))))

(defn- handle-message! [!state ^js panel ^js msg]
  (case (.-type msg)
    "cg/ready"            (do (reset! !ready true)
                              (post-pending! panel)
                              (doseq [send @!ready-sends] (send))
                              (reset! !ready-sends []))
    "cg/read-module"      (answer-module-read! panel msg)
    "cg/viewport-settled" (handle-viewport-settled! panel (.-bbox msg))
    "cg/repl-result"      (answer-repl! msg)
    "cg/run-started"      (relay! !state msg false)
    "cg/trace-event"      (relay! !state msg false)
    "cg/run-ended"        (relay! !state msg true)
    "cg/run-error"        (do (relay! !state msg true)
                              (vscode/window.showErrorMessage (str "CG map run failed: " (.-message msg))))
    ;; Before cg/ready the page could not load the JS backend, and a run that
    ;; waits for it never starts.
    ;; Only the boot failure has no :level. A console line of the page has one.
    "cg/map-error"        (if (or @!ready (.-level msg))
                            (run-status/post! !state (:uri (first @!run-docs))
                                              (relay/page-error-output msg))
                            (relay! !state #js {:type "cg/run-error"
                                                :message (str "the CG Map tab did not start: " (.-error msg))}
                                    true))
    nil))

(defn- serve-html! [^js webview roots port {:keys [public-dir wasmts-dir grammar-dir]}]
  (let [origin (str "http://localhost:" port)]
    (set! (.-options webview)
          #js {:enableScripts true
               :localResourceRoots #js []
               :portMapping #js [#js {:webviewPort port :extensionHostPort port}]})
    (set! (.-html webview)
          (exec-html/build-html
           {:asset-uri  (fn [abs] (str origin (asset-server/url-for roots abs)))
            :csp-source origin
            :nonce      (lc/make-nonce)
            :public-dir public-dir
            :wasmts-dir wasmts-dir
            :parser-wasm (path/join grammar-dir "tree-sitter-clojure.wasm")}))))

(defn- build-panel! [!state keep-focus?]
  (let [^js context (:extension/context @!state)
        {:keys [public-dir wasmts-dir grammar-dir cg-dist-dir] :as dirs} (resolve-dirs (.-extensionUri context))
        ^js panel (vscode/window.createWebviewPanel
                   "cg.mapView"
                   "CG Map"
                   #js {:viewColumn vscode/ViewColumn.Beside
                        :preserveFocus (boolean keep-focus?)}
                   #js {:retainContextWhenHidden true})
        ^js webview (.-webview panel)
        roots {"public" public-dir "wasmts" wasmts-dir "grammar" grammar-dir}
        ;; cg.run finds the worker module next to the bundle.
        server (asset-server/start! roots {"/public/cg-wasmts-handler.mjs"
                                          (path/join cg-dist-dir "cg-wasmts-handler.mjs")})]
    (-> server
        (.then (fn [^js srv] (serve-html! webview roots (.. srv address -port) dirs)))
        (.catch (fn [^js e] (vscode/window.showErrorMessage (str "CG map: the page did not load: " (.-message e))))))
    (.onDidReceiveMessage webview (fn [^js msg] (handle-message! !state panel msg)))
    (.onDidDispose panel (fn [] (.then server (fn [^js srv] (.close srv)))
                          (dotimes [_ (count @!run-docs)]
                            (relay! !state #js {:type "cg/run-error" :message "the CG Map tab closed"} true))
                          (reset! !panel nil) (reset! !ready false)
                          (reset! !last-run nil) (reset! !map-session nil)
                          (reset! !jvm-port nil) (reset! !last-vars nil)
                          (reset! !refetch-gen 0) (reset! !ready-sends []) (drop-repl-waits!)))
    panel))

(defn- ensure-panel!
  "`keep-focus?` leaves the focus in the active editor."
  [!state keep-focus?]
  (or @!panel
      (let [panel (build-panel! !state keep-focus?)]
        (reset! !ready false)
        (reset! !panel panel))))

(defn- with-active-cg! [^js panel f]
  (if-let [^js uri (doc/active-cg-uri)]
    (.then (vscode/workspace.openTextDocument uri) f)
    (do (reset! !last-run nil)
        (post-status! panel "no .cg in view. Open one and run the command again."))))

(defn- reveal-tab! [!state]
  (let [existing @!panel
        ^js panel (ensure-panel! !state false)]
    (when existing (.reveal panel vscode/ViewColumn.Beside))
    panel))

(defn- canvas-tab! [!state]
  (let [existing @!panel
        ^js panel (ensure-panel! !state true)]
    (when (and existing (not (.-visible panel)))
      (.reveal panel vscode/ViewColumn.Beside true))
    panel))

(defn open-map-webview! [!state & _]
  (let [panel (reveal-tab! !state)]
    (with-active-cg! panel (fn [document] (run-in-tab! !state panel document)))))

(defn ^:async run-document! [!state ^js document]
  (await (run-in-tab! !state (canvas-tab! !state) document)))

(defn- requested
  "`run`, a fn of the state, the tab and the document, after the CG Run line
   of its request on `runtime`."
  [run runtime]
  (fn [!state panel ^js document]
    (request! !state document runtime)
    (run !state panel document)))

(defn- run-active!
  "`label` names the command in the message when no .cg is in view."
  [!state run label]
  (if-let [^js uri (doc/active-cg-uri)]
    (.then (vscode/workspace.openTextDocument uri)
           (fn [^js document] (run !state (canvas-tab! !state) document)))
    (vscode/window.showInformationMessage (str label ": no .cg in view"))))

(defn ^:async jvm-port!
  "The port of the JVM backend, which it starts when it is down, or nil after
   an error message that names the command `label`. `on-stage` is the one of
   runtime/ensure-backend!, nil when no run follows."
  [!state ^js panel label on-stage]
  (when panel (post-status! panel "connecting to the JVM backend (a first boot takes 1 to 3 min)"))
  (let [{:keys [reachable port]} (await (runtime/ensure-backend! !state on-stage))]
    (if reachable
      port
      (do (vscode/window.showErrorMessage
           (str label " needs the JVM backend, and it did not start. See the CG output."))
          (when panel (post-status! panel "the JVM backend did not start"))
          nil))))

(defn- show-datasets! [!state & _]
  (let [panel (reveal-tab! !state)]
    (with-active-cg!
     panel
     (fn ^:async show [^js document]
       (when-let [port (await (jvm-port! !state panel "CG: Show Datasets" nil))]
         (await (run-jvm-feed! panel port document)))))))

(defn ^:async run-jvm-now! [!state ^js panel ^js document]
  (if-let [port (await (jvm-port! !state panel "CG: Eval (JVM)"
                                  (backend-stage !state (str (.-uri document)))))]
    (await (run-jvm! !state panel document port))
    (run-status/post! !state (str (.-uri document))
                      #js {:type "cg/run-error" :message "the JVM backend did not start"})))

(defn ^:async active-cg-document []
  (when-let [^js uri (doc/active-cg-uri)]
    (await (vscode/workspace.openTextDocument uri))))

(defn- calva-sequence [runtime ^js document]
  (calva/connect-sequence runtime
                          (some-> document modules/bb-root-for-document)
                          (some-> document .getText calva/file-ns)))

(defn ^:async connect-calva!
  "Opens the REPL Window too, the Calva view that takes input."
  [repl-port sequence]
  (await (vscode/commands.executeCommand "calva.connect"
                                         #js {:host "localhost" :port (str repl-port)
                                              :connectSequence sequence}))
  (await (vscode/commands.executeCommand "calva.showReplWindow")))

(defn ^:async connect-repl! [!state & _]
  (let [document (await (active-cg-document))]
    ;; No panel: a status of the connect would stay on the map after it.
    (when-let [port (await (jvm-port! !state nil "CG: Connect REPL (JVM)" nil))]
      (let [r (await (with-tab-session! port (fn [send sid] (jvm/start-repl send sid))))]
        (if-let [repl-port (:port r)]
          (await (connect-calva! repl-port (calva-sequence "jvm" document)))
          (vscode/window.showErrorMessage (str "CG: Connect REPL failed: " (:error r))))))))

(defn ^:async ensure-js-repl! []
  (when (nil? @!js-repl)
    (reset! !js-repl (js-repl/start-server! page-eval)))
  (await @!js-repl))

(defn ^:async connect-repl-js! [!state & _]
  (let [document (await (active-cg-document))]
    (canvas-tab! !state)
    (let [^js srv (await (ensure-js-repl!))]
      (await (connect-calva! (aget srv "port") (calva-sequence "js" document))))))

(defn- stop-js-repl! []
  (when-let [p @!js-repl]
    (reset! !js-repl nil)
    (.then p js-repl/stop-server!)))

(defn- js-state []
  (cond
    (and @!panel @!ready) "ready"
    @!panel "starting"
    :else "closed"))

(defn ^:async refresh-runtime! [!state & _]
  (let [{:keys [choice resolved reachable port]} (await (runtime/probe-runtime! !state))]
    (vscode/window.showInformationMessage
     (lane/runtime-report choice resolved (js-state) reachable port))))

(defn register-run-commands!
  "The JS backend is the CG Map tab, because only a webview loads wasmts the
   way the web app does."
  [!state]
  (lc/register-command! !state "cg.evalJs"
                        (fn [state] (run-active! state (requested run-js! "js") "CG: Eval (JS)")))
  (lc/register-command! !state "cg.evalJvm"
                        (fn [state] (run-active! state (requested run-jvm-now! "jvm") "CG: Eval (JVM)")))
  (lc/register-command! !state "cg.refreshRuntime" refresh-runtime!)
  (lc/register-command! !state "cg.runEval" (fn [state] (run-active! state run-in-tab! "CG: Run")))
  (lc/register-command! !state "cg.showDatasets" show-datasets!)
  (lc/register-command! !state "cg.connectRepl" connect-repl!)
  (lc/register-command! !state "cg.connectReplJs" connect-repl-js!)
  (lc/push-disposable! !state #js {:dispose stop-js-repl!}))
