;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.exec-webview
  "The 'CG Map' webview tab. It runs a .cg on the shared JS backend of the web app
   and shows its dataset output on a MapLibre map."
  (:require ["vscode" :as vscode]
            ["path" :as path]
            [cg-vscode.asset-server :as asset-server]
            [cg-vscode.exec-html :as exec-html]
            [cg-vscode.extension.lifecycle :as lc]
            [cg-vscode.extract :as extract]
            [cg-vscode.modules :as modules]
            ["../../../cg-app/shared/squint/src/cg_gui/run/lane.mjs" :as lane]
            [cg-vscode.run-relay :as relay]
            [cg-vscode.run-status :as run-status]
            [cg-vscode.runtime :as runtime]
            [cg-vscode.runtime.jvm-client :as jvm]
            [cg-vscode.doc :as doc]))

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

(defn- resolve-dirs
  "The dirs that the tab serves, from the fsPath of the extension."
  [^js extension-uri]
  (let [repo (path/resolve (.-fsPath extension-uri) "..")]
    {:public-dir (path/join repo "cg-app" "shared" "ui" "public")
     :wasmts-dir (path/join repo "cg" "node_modules" "@wcohen" "wasmts" "dist")
     :cg-dist-dir (path/join repo "cg" "dist")
     :grammar-dir (path/join repo "cg" "node_modules" "@yogthos" "tree-sitter-clojure")}))

(defn- post-pending!
  "Posts the stashed message (cg/run or cg/layers) to the webview, if there is
   one."
  [^js panel]
  (when-let [^js m @!last-run]
    (.. panel -webview (postMessage m))))

(defn- post-status! [^js panel ^js text]
  (.. panel -webview (postMessage #js {:type "cg/status" :text text})))

(defn- stash-and-post!
  "Stashes `msg` for replay and posts it when the webview is ready."
  [^js panel ^js msg]
  (reset! !last-run msg)
  (when @!ready (post-pending! panel)))

(defn- post-needs-backend!
  "The :unsupported route: the .cg needs the JVM backend (load-module or file I/O)
   and the backend is not reachable."
  [^js panel ^js cg-uri]
  (reset! !last-run nil)
  (vscode/window.showWarningMessage
   (str "CG map: " (.-fsPath cg-uri)
        " needs the JVM backend (it uses run/load-module or local-file I/O), but the"
        " CG backend is unreachable. Start it with `bb backend:dev`."))
  (post-status! panel "needs the JVM backend, which is unreachable (bb backend:dev)"))

(defn- post-refused!
  "The :refused route: the runtime choice is `jvm` and the backend is down.
   There is no fallback to JS."
  [^js panel]
  (reset! !last-run nil)
  (vscode/window.showErrorMessage
   "CG map: runtime is set to jvm but the CG backend is unreachable. Start it with `bb backend:dev`.")
  (post-status! panel "jvm runtime selected, backend unreachable"))

(defn- ensure-map-session!
  "The JVM session id of the tab, created when there is none. Promise<sid>."
  [port]
  (if-let [sid @!map-session]
    (js/Promise.resolve sid)
    (-> (jvm/create-session port)
        (.then (fn [sid] (reset! !map-session sid) sid)))))

(defn- render-jvm-result!
  "Posts cg/layers for the result of run->layers, or shows its :error."
  [^js panel result]
  (let [layers (:layers result)]
    (cond
      (:error result)
      (vscode/window.showErrorMessage (str "CG map (JVM) run failed: " (:error result)))

      (and layers (pos? (.-length layers)))
      (stash-and-post! panel #js {:type "cg/layers" :layers layers})

      :else
      (post-status! panel "run ok, no geometry dataset vars to map. Open a file that defs dataset layers."))))

(defn- remember-fetched-vars!
  "Records the geometry var names that a full run rendered. A viewport refetch
   fetches those vars again."
  [result]
  (let [layers (:layers result)]
    (when (and layers (pos? (.-length layers)))
      (reset! !last-vars (.map layers (fn [^js l] (aget l "var")))))))

(defn ^:async load-in-session!
  "run->layers in the session of the tab. An expired session is dropped, and
   the result is {:expired true}."
  [port text filename]
  (let [sid (await (ensure-map-session! port))
        r (await (jvm/run->layers port sid text filename {:limit jvm/default-row-limit}))]
    (when (:expired r) (reset! !map-session nil))
    r))

(defn ^:async run-jvm-feed!
  "Runs the active .cg in the JVM session of the tab, again in a new session
   when the session expired, then renders."
  [^js panel port text filename]
  (reset! !jvm-port port)
  ;; session/load has no response timeout, and a first load can take minutes.
  (post-status! panel "loading session (a first-run full analysis takes minutes)")
  (try
    (let [r (await (load-in-session! port text filename))
          r (if (:expired r) (await (load-in-session! port text filename)) r)]
      (remember-fetched-vars! r)
      (render-jvm-result! panel r))
    (catch :default e
      (vscode/window.showErrorMessage (str "CG map (JVM) failed: " (.-message e))))))

(defn ^:async handle-viewport-settled!
  "Fetches the last vars of the live JVM session for the new viewport `bbox`,
   with no new run. Only the result of the latest call renders."
  [^js panel bbox]
  (let [sid  @!map-session
        port @!jvm-port
        vars @!last-vars]
    (when (and sid port vars (pos? (.-length vars)))
      (swap! !refetch-gen inc)
      (let [gen @!refetch-gen]
        (try
          (let [r (await (jvm/refetch-layers port sid vars {:bbox bbox :limit jvm/default-row-limit}))]
            (cond
              (:expired r) (reset! !map-session nil)
              (= gen @!refetch-gen) (render-jvm-result! panel r)))
          (catch :default _ nil))))))

(defn- post-js-run!
  "Posts cg/run for the document `cg-uri` to the tab, with the model that its
   canvas picked, and records the document for the run messages."
  [!state ^js panel ^js cg-uri text flow]
  (let [uri-str (str cg-uri)
        file (.-fsPath cg-uri)]
    ;; The stash keeps only the last message for a page that is not ready.
    (if @!ready
      (swap! !run-docs conj {:uri uri-str :file file})
      (reset! !run-docs [{:uri uri-str :file file}]))
    (stash-and-post! panel #js {:type "cg/run" :text text :flow flow
                                :model (get-in @!state [:editor/run-models uri-str])
                                :file (path/basename file)})))

(defn ^:async route-and-run!
  "Starts the JVM backend when the run can need it, then runs the .cg on
   the route that lane/route-map-run picks."
  [^js panel !state ^js cg-uri text flow]
  (let [needs-jvm? (lane/flow-needs-jvm? flow)]
    (when (or needs-jvm? (runtime/wants-jvm?))
      (await (runtime/ensure-backend! !state (:runtime/output-channel @!state))))
    (let [{:keys [resolved reachable port]} (await (runtime/resolve-map-target !state))
          lane (lane/route-map-run
                {:needs-jvm? needs-jvm?
                 :backend-reachable? reachable
                 :resolved-runtime (if (= "jvm" resolved) :jvm :js)})]
      (cond
        (= lane :js)  (post-js-run! !state panel cg-uri text flow)
        (= lane :jvm) (do (reset! !run-docs [])
                          (await (run-jvm-feed! panel port text
                                                (path/basename (.-fsPath cg-uri)))))
        (= lane :unsupported) (post-needs-backend! panel cg-uri)
        (= lane :refused)     (post-refused! panel)))))

(defn- resolve-run!
  "Reads and parses the active .cg, then routes it to a backend. Posts a status
   when no .cg is in view."
  [^js panel !state]
  (let [^js cg-uri (doc/active-cg-uri)]
    (if-not cg-uri
      (do (reset! !last-run nil)
          (post-status! panel "no .cg in view. Open one and run CG: Open Map again."))
      (-> (vscode/workspace.openTextDocument cg-uri)
          (.then (fn [^js doc]
                   (let [text (.getText doc)]
                     (-> (extract/extract-models text)
                         (.then (fn [flow]
                                  (route-and-run! panel !state cg-uri text flow)))))))
          (.catch (fn [^js e]
                    (vscode/window.showErrorMessage
                     (str "CG map: parse failed for " (.-fsPath cg-uri) ": " (.-message e)))))))))

(defn ^:async answer-module-read!
  "Answers cg/read-module of the page with the text of the module, resolved
   against the bb root of the document of the run, as `bb run` resolves it."
  [^js panel ^js msg]
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
          (answer #js {:error (str "load-module " p ": no such file under the bb root of the .cg")})))
      (catch :default e
        (answer #js {:error (str "load-module " p ": " (.-message e))})))))

(defn- relay!
  "Posts the canvas messages of the map message `msg` to the canvas of the
   document of the run. `end?` closes the run."
  [!state ^js msg end?]
  (when-let [{:keys [uri file]} (first @!run-docs)]
    (doseq [m (relay/canvas-messages msg file)]
      (run-status/post! !state uri m))
    (when end? (swap! !run-docs (fn [ds] (vec (rest ds)))))))

(defn- handle-message!
  "Handles a message from the webview. The messages of a JS run also go to the
   canvas of its document."
  [!state ^js panel ^js msg]
  (case (.-type msg)
    "cg/ready"            (do (reset! !ready true)
                              (post-pending! panel))
    "cg/read-module"      (answer-module-read! panel msg)
    "cg/viewport-settled" (handle-viewport-settled! panel (.-bbox msg))
    "cg/run-started"      (relay! !state msg false)
    "cg/trace-event"      (relay! !state msg false)
    "cg/run-ended"        (relay! !state msg true)
    "cg/run-error"        (do (relay! !state msg true)
                              (vscode/window.showErrorMessage (str "CG map run failed: " (.-message msg))))
    ;; Before cg/ready the page could not load the JS backend, and a run that
    ;; waits for it never starts.
    "cg/map-error"        (when-not @!ready
                            (relay! !state #js {:type "cg/run-error"
                                                :message (str "the CG Map tab did not start: " (.-error msg))}
                                    true))
    nil))

(defn- serve-html!
  "Points `webview` at the asset server on `port` and sets its HTML."
  [^js webview roots port {:keys [public-dir wasmts-dir grammar-dir]}]
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

(defn- build-panel!
  "Creates the map webview panel beside the active editor, with its asset
   server, HTML and message handler. Clears the singleton state on dispose."
  [!state keep-focus?]
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
                          (reset! !panel nil) (reset! !ready false)
                          (reset! !last-run nil) (reset! !run-docs []) (reset! !map-session nil)
                          (reset! !jvm-port nil) (reset! !last-vars nil)
                          (reset! !refetch-gen 0)))
    panel))

(defn- ensure-panel!
  "The map tab, created when there is none. `keep-focus?` leaves the focus in
   the active editor."
  [!state keep-focus?]
  (or @!panel
      (let [panel (build-panel! !state keep-focus?)]
        (reset! !ready false)
        (reset! !panel panel))))

(defn open-map-webview!
  "The command handler. Reveals the map tab or creates it, then runs the active
   .cg."
  [!state & _]
  (let [existing @!panel
        ^js panel (ensure-panel! !state false)]
    (when existing (.reveal panel vscode/ViewColumn.Beside))
    (resolve-run! panel !state)))

(defn ^:async run-document!
  "Runs `document` on the JS backend in the CG Map tab. The tab opens beside the
   canvas and leaves it the focus."
  [!state ^js document]
  (let [text (.getText document)
        ^js uri (.-uri document)]
    (try
      (let [flow (await (extract/extract-models text))
            existing @!panel
            ^js panel (ensure-panel! !state true)]
        (when (and existing (not (.-visible panel)))
          (.reveal panel vscode/ViewColumn.Beside true))
        (post-js-run! !state panel uri text flow))
      (catch :default e
        (run-status/post! !state (str uri)
                          #js {:type "cg/run-error"
                               :message (str "the parser failed: " (.-message e))})))))

(defn- run-active-js!
  "Runs the .cg in view on the JS backend, in the CG Map tab."
  [!state]
  (if-let [^js uri (doc/active-cg-uri)]
    (-> (vscode/workspace.openTextDocument uri)
        (.then (fn [^js document] (run-document! !state document))))
    (vscode/window.showInformationMessage "CG: Run (JS): no .cg in view")))

(defn register-run-commands!
  "`CG: Eval (JS)` and `CG: Run`. The JS backend is the CG Map tab, because only
   a webview loads wasmts the way the web app does."
  [!state]
  (lc/register-command! !state "cg.evalJs" run-active-js!)
  (lc/register-command! !state "cg.runEval"
                        (fn [state]
                          (if (= "jvm" (or (:runtime/resolved @state) (:runtime/choice @state)))
                            (runtime/run-eval-jvm! state)
                            (run-active-js! state)))))
