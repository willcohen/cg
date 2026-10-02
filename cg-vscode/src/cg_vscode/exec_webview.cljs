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
;; The generation counter lets a newer refetch supersede one in flight.
(defonce ^:private !jvm-port (atom nil))
(defonce ^:private !last-vars (atom nil))
(defonce ^:private !refetch-gen (atom 0))

(defn- resolve-dirs
  "The absolute paths of the shared cg-app public dir, the wasmts dist dir and
   the dir of the clojure grammar, relative to the fsPath of the extension."
  [^js extension-uri]
  (let [repo (path/resolve (.-fsPath extension-uri) "..")]
    {:public-dir (path/join repo "cg-app" "shared" "ui" "public")
     :wasmts-dir (path/join repo "cg" "node_modules" "@wcohen" "wasmts" "dist")
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
  [^js panel ^js channel ^js cg-uri]
  (reset! !last-run nil)
  (let [msg (str (.-fsPath cg-uri)
                 " needs the JVM backend (it uses run/load-module or local-file "
                 "I/O), but the CG backend is unreachable — start it with "
                 "`bb backend:dev`.")]
    (when channel (.appendLine channel (str "CG map: " msg)))
    (vscode/window.showWarningMessage (str "CG map: " msg))
    (post-status! panel "needs the JVM backend, which is unreachable (bb backend:dev)")))

(defn- post-refused!
  "The :refused route: the runtime choice is `jvm` and the backend is down.
   There is no fallback to JS."
  [^js panel ^js channel]
  (reset! !last-run nil)
  (let [msg "runtime is set to jvm but the CG backend is unreachable — start it with `bb backend:dev`."]
    (when channel (.appendLine channel (str "CG map: " msg)))
    (vscode/window.showErrorMessage (str "CG map: " msg))
    (post-status! panel "jvm runtime selected — backend unreachable")))

(defn- ensure-map-session!
  "The JVM session id of the tab, created when there is none. Promise<sid>."
  [port]
  (if-let [sid @!map-session]
    (js/Promise.resolve sid)
    (-> (jvm/create-session port)
        (.then (fn [sid] (reset! !map-session sid) sid)))))

(defn- render-jvm-result!
  "Posts the result of run->layers: cg/layers on success, or shows :error. The
   caller handles :expired."
  [^js panel ^js channel result]
  (if (:error result)
    (do (when channel (.appendLine channel (str "CG map (JVM): run FAILED — " (:error result))))
        (vscode/window.showErrorMessage (str "CG map (JVM) run failed: " (:error result))))
    (let [layers (:layers result)
          warnings (:warnings result)]
      (when (and channel warnings (pos? (.-length warnings)))
        (.forEach warnings (fn [w] (.appendLine channel (str "CG map (JVM): " w)))))
      (if (and layers (pos? (.-length layers)))
        (do (stash-and-post! panel #js {:type "cg/layers" :layers layers})
            (when channel (.appendLine channel (str "CG map (JVM): " (.-length layers) " layer(s)"))))
        (do (post-status! panel "run ok — no geometry dataset vars to map. This file defines functions only; open a file that defs dataset layers.")
            (when channel (.appendLine channel "CG map (JVM): run ok — no geometry dataset vars. This file defines functions only; open a file that defs dataset layers.")))))))

(defn- remember-fetched-vars!
  "Records the geometry var names that a full run rendered. A viewport refetch
   fetches those vars again."
  [result]
  (let [layers (:layers result)]
    (when (and layers (pos? (.-length layers)))
      (reset! !last-vars (.map layers (fn [^js l] (aget l "var")))))))

(defn ^:async run-jvm-feed!
  "Runs the active .cg on the JVM backend: creates or reuses the tab session, calls
   run->layers, retries one time when the session expired (404), then renders."
  [^js panel ^js channel port text filename]
  (reset! !jvm-port port)
  ;; session/load has no response timeout, and a first load can take minutes.
  ;; Tell the user before the wait.
  (when channel
    (.appendLine channel
                 "CG map (JVM): loading session — cached layers arrive in seconds; a first-run full analysis takes minutes"))
  (post-status! panel "loading session (a first-run full analysis takes minutes)")
  (try
    (let [sid (await (ensure-map-session! port))
          r (await (jvm/run->layers port sid text filename {:limit jvm/default-row-limit}))]
      (if (:expired r)
        (do (reset! !map-session nil)
            (let [sid2 (await (ensure-map-session! port))
                  r2 (await (jvm/run->layers port sid2 text filename {:limit jvm/default-row-limit}))]
              (remember-fetched-vars! r2)
              (render-jvm-result! panel channel r2)))
        (do (remember-fetched-vars! r)
            (render-jvm-result! panel channel r))))
    (catch :default e
      (when channel (.appendLine channel (str "CG map (JVM): " (.-message e))))
      (vscode/window.showErrorMessage (str "CG map (JVM) failed: " (.-message e))))))

(defn ^:async handle-viewport-settled!
  "Fetches the last vars of the live JVM session for the new viewport `bbox`,
   with no new run. Only the result of the latest call renders."
  [^js panel ^js channel bbox]
  (let [sid  @!map-session
        port @!jvm-port
        vars @!last-vars]
    (when (and sid port vars (pos? (.-length vars)))
      (swap! !refetch-gen inc)
      (let [gen @!refetch-gen]
        (try
          (let [r (await (jvm/refetch-layers port sid vars {:bbox bbox :limit jvm/default-row-limit}))]
            (cond
              (:expired r)
              (do (reset! !map-session nil)
                  (when channel
                    (.appendLine channel "CG map (JVM): session expired on viewport refetch — re-run to reload")))
              (not= gen @!refetch-gen) nil
              :else (render-jvm-result! panel channel r)))
          (catch :default e
            (when channel
              (.appendLine channel (str "CG map (JVM) viewport refetch failed: " (.-message e))))))))))

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
  [^js panel ^js channel !state ^js cg-uri text flow]
  (let [needs-jvm? (lane/flow-needs-jvm? flow)]
    (when (or needs-jvm? (runtime/wants-jvm?))
      (await (runtime/ensure-backend! !state channel)))
    (let [{:keys [resolved reachable port]} (await (runtime/resolve-map-target !state))
          lane (lane/route-map-run
                {:needs-jvm? needs-jvm?
                 :backend-reachable? reachable
                 :resolved-runtime (if (= "jvm" resolved) :jvm :js)})]
      (when channel
        (.appendLine channel (str "CG map: " (.-fsPath cg-uri) " → route " lane
                                  " (resolved=" resolved ", reachable=" reachable ")")))
      (cond
        (= lane :js)  (post-js-run! !state panel cg-uri text flow)
        (= lane :jvm) (do (reset! !run-docs [])
                          (await (run-jvm-feed! panel channel port text
                                                (last (.split (.-fsPath cg-uri) "/")))))
        (= lane :unsupported) (post-needs-backend! panel channel cg-uri)
        (= lane :refused)     (post-refused! panel channel)))))

(defn- resolve-run!
  "Reads and parses the active .cg, then routes it to a backend. Posts a status
   when no .cg is in view."
  [^js panel ^js channel !state]
  (let [^js cg-uri (doc/active-cg-uri)]
    (if-not cg-uri
      (do (reset! !last-run nil)
          (post-status! panel "no .cg in view — open one and re-run CG: Open Map"))
      (-> (vscode/workspace.openTextDocument cg-uri)
          (.then (fn [^js doc]
                   (let [text (.getText doc)]
                     (-> (extract/extract-models text)
                         (.then (fn [flow]
                                  (route-and-run! panel channel !state cg-uri text flow)))))))
          (.catch (fn [^js e]
                    (when channel (.appendLine channel (str "CG map: parse failed — " (.-message e))))
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
  [!state ^js panel ^js channel ^js msg]
  (let [t (.-type msg)]
    (cond
      (= t "cg/ready")
      (do (reset! !ready true)
          (post-pending! panel))

      (= t "cg/run-started")
      (relay! !state msg false)

      (= t "cg/read-module")
      (answer-module-read! panel msg)

      (= t "cg/trace-event")
      (relay! !state msg false)

      (= t "cg/map-ready")
      (.appendLine channel (str "CG map: rendered layer cgdata-" (.-layer msg)
                                " (" (.-count msg) " result(s))"))

      (= t "cg/layers-rendered")
      (.appendLine channel (str "CG map (JVM): rendered " (.-count msg) " layer(s)"))

      (= t "cg/viewport-settled")
      (handle-viewport-settled! panel channel (.-bbox msg))

      (= t "cg/run-result")
      (do (.appendLine channel (str "CG map: run ok — " (.-count msg) " result(s)"
                                    (if (.-mapped msg)
                                      (str ", layer cgdata-" (.-layer msg))
                                      ", no map layer (report/scalar/no geometry)")))
          (relay! !state msg true))

      (= t "cg/run-error")
      (do (.appendLine channel (str "CG map: run FAILED — " (.-error msg)))
          (relay! !state msg true)
          (vscode/window.showErrorMessage (str "CG map run failed: " (.-error msg))))

      ;; Before cg/ready the page could not load the JS backend, and a run that
      ;; waits for it never starts.
      (= t "cg/map-error")
      (do (.appendLine channel (str "CG map: map init FAILED — " (.-error msg)))
          (when-not @!ready
            (relay! !state #js {:type "cg/run-error"
                                :error (str "the CG Map tab did not start: " (.-error msg))}
                    true)))

      (= t "cg/csp-violation")
      (.appendLine channel (str "CG map CSP violation: " (.-directive msg)
                                " blocked " (.-blocked msg))))))

(defn- build-panel!
  "Creates the map webview panel beside the active editor, with its asset
   roots, HTML and message handler. Clears the singleton state on dispose."
  [!state keep-focus?]
  (let [^js context (:extension/context @!state)
        ^js channel (:runtime/output-channel @!state)
        ^js ext-uri (.-extensionUri context)
        {:keys [public-dir wasmts-dir grammar-dir]} (resolve-dirs ext-uri)
        ^js panel (vscode/window.createWebviewPanel
                   "cg.mapView"
                   "CG Map"
                   #js {:viewColumn vscode/ViewColumn.Beside
                        :preserveFocus (boolean keep-focus?)}
                   #js {:enableScripts true
                        :retainContextWhenHidden true
                        :localResourceRoots
                        (to-array
                         [(vscode/Uri.joinPath ext-uri "dist" "webview")
                          (vscode/Uri.file public-dir)
                          (vscode/Uri.file wasmts-dir)
                          (vscode/Uri.file grammar-dir)])})
        ^js webview (.-webview panel)
        asset-uri (fn [abs] (str (.asWebviewUri webview (vscode/Uri.file abs))))]
    (set! (.-html webview)
          (exec-html/build-html
           {:asset-uri  asset-uri
            :csp-source (.-cspSource webview)
            :nonce      (lc/make-nonce)
            :public-dir public-dir
            :wasmts-dir wasmts-dir
            :parser-wasm (path/join grammar-dir "tree-sitter-clojure.wasm")}))
    (.onDidReceiveMessage webview (fn [^js msg] (handle-message! !state panel channel msg)))
    (.onDidDispose panel (fn [] (reset! !panel nil) (reset! !ready false)
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
        (when-let [^js channel (:runtime/output-channel @!state)]
          (.appendLine channel "CG map: opening..."))
        (reset! !ready false)
        (reset! !panel panel))))

(defn open-map-webview!
  "The command handler. Reveals the map tab or creates it, then runs the active
   .cg."
  [!state & _]
  (let [^js channel (:runtime/output-channel @!state)
        existing @!panel
        ^js panel (ensure-panel! !state false)]
    (when existing (.reveal panel vscode/ViewColumn.Beside))
    (resolve-run! panel channel !state)))

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
    (when-let [^js channel (:runtime/output-channel @!state)]
      (.appendLine channel "CG: Run (JS) — no .cg in view"))))

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
