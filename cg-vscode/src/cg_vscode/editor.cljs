;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.editor
  (:require ["vscode" :as vscode]
            ["fs" :as fs]
            ["child_process" :as cp]
            [clojure.string :as str]
            [cg-vscode.canvas-html :as canvas-html]
            [cg-vscode.dev-shell :as dev-shell]
            [cg-vscode.diagnostics :as diag]
            [cg-vscode.doc :as cg-doc]
            [cg-vscode.exec-webview :as exec-webview]
            [cg-vscode.extension.lifecycle :as lc]
            [cg-vscode.extract :as extract]
            [cg-vscode.kondo :as kondo]
            [cg-vscode.modules :as modules]
            [cg-vscode.run-status :as run-status]
            ["../../../cg-app/shared/squint/src/cg_gui/doc/overlay.mjs" :as overlay]
            ["../../../cg-app/shared/squint/src/cg_gui/run/js.mjs" :as run-js]))

(def ^:private debounce-ms 250)

(defn- read-brand-text
  "Reads a text asset (for example icons.svg) from dist/webview/cg-brand/ of
   the extension. Returns nil when the file is missing."
  [extension-uri filename]
  (let [^js uri (vscode/Uri.joinPath extension-uri "dist" "webview" "cg-brand" filename)
        path (.-fsPath uri)]
    (try
      (.readFileSync fs path "utf8")
      (catch :default _ nil))))

(defn- webview-html [^js webview extension-uri]
  (canvas-html/build-html
   {:asset-uri  (fn [rel]
                  (str (.asWebviewUri
                        webview
                        (apply vscode/Uri.joinPath extension-uri "dist" "webview"
                               (str/split rel #"/")))))
    :csp-source (.-cspSource webview)
    :nonce      (lc/make-nonce)
    :mode       (canvas-html/theme-kind->mode (.-kind vscode/window.activeColorTheme))
    :sprite     (read-brand-text extension-uri "icons.svg")}))

(defn- send-cg-flow-to-webview!
  "Posts the parsed CgFlow to the webview as {:type \"cg/flow\" :flow <js>}.
   The ^:cgproj overlay positions go in as :visual {:pos [x y]}."
  [^js panel cg-flow]
  (let [merged (extract/merge-overlay-into-cg-flow
                cg-flow (extract/decoded-overlay cg-flow))]
    (.. panel -webview
        (postMessage #js {:type "cg/flow"
                          :flow merged}))))

(defn- run-model-for
  "The model that Run runs for the document `uri-str`: the canvas pick while
   the flow defines it, else nil. nil runs the last threading-macro binding."
  [!state uri-str cg-flow]
  (let [pick (get-in @!state [:editor/run-models uri-str])]
    (when (some #(= pick %) (run-js/model-names cg-flow))
      pick)))

(defn- post-run-model!
  "Tells the canvas which model Run runs. Forgets a pick that the flow does not
   define."
  [!state ^js panel uri-str cg-flow]
  (let [pick (run-model-for !state uri-str cg-flow)]
    (when-not pick
      (swap! !state update :editor/run-models dissoc uri-str))
    (.. panel -webview
        (postMessage #js {:type "cg/run-model"
                          :model (or pick (run-js/model-binding-name cg-flow))}))))

(defn- post-flow-to-panel!
  "Posts the flow and the run model to the canvas, and records which node ids
   of this flow can keep a dragged position."
  [!state ^js panel uri-str cg-flow]
  (swap! !state assoc-in [:editor/overlay-ids uri-str] (extract/overlay-node-ids cg-flow))
  (send-cg-flow-to-webview! panel cg-flow)
  (post-run-model! !state panel uri-str cg-flow))

(defn- extract-and-fan-out!
  "Parses `document` and its modules, then sets the diagnostics and posts the
   flow to `panel`. A second check clears the unresolved marks on module ops."
  ([!state document] (extract-and-fan-out! !state nil document))
  ([!state ^js panel ^js document]
   (let [text (.getText document)
         ^js coll (:editor/diagnostic-collection @!state)
         uri (.-uri document)
         ^js channel (:runtime/output-channel @!state)]
     (when-let [^js kcoll (:editor/kondo-collection @!state)]
       (kondo/lint-document! kcoll document))
     (-> (extract/extract-models text)
         (.then (fn [cg-flow]
                  (-> (modules/follow-loaded-modules! document cg-flow)
                      (.then (fn [extra-ops]
                               (let [merged (if (seq extra-ops)
                                              (extract/recheck
                                               (update cg-flow :operations
                                                       (fn [ops]
                                                         (vec (concat ops extra-ops)))))
                                              cg-flow)]
                                 (when coll
                                   (diag/set-diagnostics! coll uri merged))
                                 (when panel
                                   (post-flow-to-panel! !state panel (str uri) merged)))))
                      (.catch (fn [err]
                                ;; A module that does not parse must not stop
                                ;; the main flow.
                                (when channel
                                  (.appendLine channel
                                               (str "CG: load-module follow failed "
                                                    (.-message err))))
                                (when coll
                                  (diag/set-diagnostics! coll uri cg-flow))
                                (when panel
                                  (post-flow-to-panel! !state panel (str uri) cg-flow)))))))
         (.catch (fn [err]
                   (when channel
                     (.appendLine channel
                                  (str "CG: parser error " (.-message err)))
                     (when-let [stack (.-stack err)]
                       (.appendLine channel
                                    (str "CG: parser stack\n" stack))))))))))

(defn- start-trace-tail!
  "Polls `trace-file-path` every 100 ms and posts each new event one time for
  each `:seq`. Returns a cleanup fn."
  [post ^js channel trace-file-path]
  (let [pos       (atom 0)
        seen-seqs (atom #{})
        timer     (atom nil)
        poll-fn   (fn poll []
                    (try
                      (when (.existsSync fs trace-file-path)
                        (let [^js stat (.statSync fs trace-file-path)
                              size (.-size stat)]
                          (when (> size @pos)
                            (let [^js fd (.openSync fs trace-file-path "r")
                                  delta (- size @pos)
                                  buf (js/Buffer.alloc delta)
                                  _ (.readSync fs fd buf 0 delta @pos)
                                  _ (.closeSync fs fd)
                                  text (.toString buf "utf8")]
                              (reset! pos size)
                              (doseq [line (str/split-lines text)]
                                (when-not (str/blank? line)
                                  (try
                                    (let [evt (js/JSON.parse line)
                                          seq-num (.-seq evt)]
                                      (when-not (contains? @seen-seqs seq-num)
                                        (swap! seen-seqs conj seq-num)
                                        (post #js {:type "cg/trace-event"
                                                   :event evt})))
                                    (catch :default e
                                      (.appendLine channel
                                                   (str "CG: trace-tail JSON parse failed: "
                                                        (.-message e)))))))))))
                      (catch :default e
                        (.appendLine channel
                                     (str "CG: trace-tail poll error: " (.-message e))))))
        interval  (js/setInterval poll-fn 100)]
    (reset! timer interval)
    (fn cleanup []
      (when @timer
        (js/clearInterval @timer)
        (reset! timer nil))
      ;; A last poll reads the end of the file after the subprocess exits.
      (poll-fn))))

(defn- post-run-output!
  "Posts a chunk of run output to the console pane of the canvas, as it is,
  because a chunk can end in the middle of a line."
  [post stream text]
  (post #js {:type "cg/run-output" :stream stream :text text}))

(defn- run-jvm-and-tail!
  "Spawns `bb run <doc>` in the dev shell, with the model that the canvas
  picked. Sends the output to the channel and the canvas, and tails the trace."
  [!state ^js panel ^js document model]
  (let [^js channel (:runtime/output-channel @!state)
        file-path (.. document -uri -fsPath)
        cwd       (modules/bb-root-for-document document)
        [prog args] (dev-shell/spawn-args js/process.env cwd
                                          (if model
                                            ["bb" "run" file-path "--model" model]
                                            ["bb" "run" file-path]))
        ;; cg.trace writes the trace file only into a folder that
        ;; CG_TRACE_DIR names.
        proc      (.spawn cp prog args
                          #js {:cwd cwd
                               :env (js/Object.assign #js {} js/process.env
                                                      #js {:CG_TRACE_DIR "tmp/cg-traces"})
                               :stdio #js ["pipe" "pipe" "pipe"]})
        cleanup   (atom nil)
        post      (fn [msg] (run-status/post-to! !state panel msg))]
    (.show channel true)
    (.appendLine channel (str "CG: spawned bb run " file-path
                              " (pid " (.-pid proc) ", cwd " cwd ")"))
    (post #js {:type "cg/run-started"
               :file file-path
               :pid  (.-pid proc)})
    (.on (.-stdout proc) "data"
         (fn [^js chunk]
           (let [text (.toString chunk "utf8")]
             (.append channel text)
             (post-run-output! post "stdout" text)
             (when-let [match (.match text #"\[cg\.trace\] writing events to (.+)")]
               (let [trace-file (.trim (aget match 1))]
                 (when-not @cleanup
                   (.appendLine channel (str "CG: tailing " trace-file))
                   (reset! cleanup (start-trace-tail! post channel trace-file))))))))
    (.on (.-stderr proc) "data"
         (fn [^js chunk]
           (let [text (.toString chunk "utf8")]
             (.append channel text)
             (post-run-output! post "stderr" text))))
    (.on proc "exit"
         (fn [code _signal]
           (.appendLine channel (str "CG: bb run exited code=" code))
           (when @cleanup (@cleanup) (reset! cleanup nil))
           (post #js {:type "cg/run-ended"
                      :outcome (if (= 0 code) "success" "error")
                      :exitCode code})))
    (.on proc "error"
         (fn [^js err]
           (.appendLine channel (str "CG: bb run spawn error: " (.-message err)))
           (post #js {:type "cg/run-error"
                      :message (.-message err)})))))

(defn- apply-position-edit! [^js document positions]
  (let [text (.getText document)]
    (-> (extract/extract-models text)
        (.then (fn [cg-flow]
                 (let [updated (overlay/update-visual-metadata text cg-flow positions)]
                   (when (not= text updated)
                     (let [edit (vscode/WorkspaceEdit.)
                           uri (.-uri document)
                           ^js doc document
                           full-range (vscode/Range. (vscode/Position. 0 0)
                                                     (.positionAt doc (.-length text)))]
                       (.replace edit uri full-range updated)
                       (vscode/workspace.applyEdit edit)))))))))

(defn- apply-literal-edit!
  "Replaces the span of one literal with its new source text. Returns false
   when the span no longer holds the old literal."
  [^js document ^js payload]
  (let [loc (.-location payload)
        r (vscode/Range.
           (vscode/Position. (dec (aget loc "start-line")) (dec (aget loc "start-column")))
           (vscode/Position. (dec (aget loc "end-line")) (dec (aget loc "end-column"))))]
    (if (= (.-was payload) (.getText document r))
      (let [edit (vscode/WorkspaceEdit.)]
        (.replace edit (.-uri document) r (.-text payload))
        (vscode/workspace.applyEdit edit)
        true)
      false)))

(defn- ensure-text-editor! [^js panel ^js document]
  (let [uri-str (str (.-uri document))
        has-text (some (fn [^js te]
                         (= (str (.. te -document -uri)) uri-str))
                       (.-visibleTextEditors vscode/window))]
    (when-not has-text
      (let [panel-col (.-viewColumn panel)
            text-col (if (= 1 panel-col) 2 1)]
        (-> (vscode/window.showTextDocument document
                                            #js {:viewColumn text-col
                                                 :preserveFocus true})
            (.then (fn [_] nil) (fn [_] nil)))))))

(defn- resolve-editor [!state extension-uri]
  (fn [^js document ^js panel _token]
    (let [uri-str (str (.-uri document))]
      (swap! !state update :editor/flow-editor-uris conj uri-str)
      (swap! !state assoc-in [:editor/panels uri-str] panel)
      (.onDidDispose panel
                     (fn []
                       (swap! !state update :editor/flow-editor-uris disj uri-str)
                       (swap! !state update :editor/panels dissoc uri-str)
                       (swap! !state update :editor/overlay-ids dissoc uri-str)
                       (swap! !state update :editor/run-models dissoc uri-str))))

    (set! (.. panel -webview -options)
          #js {:enableScripts true
               :localResourceRoots
               (to-array [(vscode/Uri.joinPath extension-uri "dist" "webview")])})

    (set! (.. panel -webview -html) (webview-html (. panel -webview) extension-uri))

    (let [!debounce (atom nil)]
      (.onDidReceiveMessage (. panel -webview)
                            (fn [^js msg]
                              (condp = (.-type msg)
                                "cg/ready"
                                ;; The mount can come after the first cg/flow:
                                ;; post the flow again.
                                (extract-and-fan-out! !state panel document)

                                "cg/edit"
                                (let [edit-type (.-editType msg)]
                                  (condp = edit-type
                                    "DragNode"
                                    (let [payload (.-payload msg)
                                          node-id (.-nodeId payload)
                                          pos [(.. payload -position -x) (.. payload -position -y)]
                                          ids (get-in @!state [:editor/overlay-ids (str (.-uri document))])]
                                      ;; The canvas lays out a thread frame, a
                                      ;; parameter chip and a block itself: a
                                      ;; position of one in the file has no
                                      ;; reader, and the write makes the canvas
                                      ;; move the card back.
                                      (when (and ids (contains? ids node-id))
                                        (apply-position-edit! document {node-id pos})))
                                    "SetLiteral"
                                    ;; The box shows the file value again only on
                                    ;; a new cg/flow, and a refused edit changes
                                    ;; no text that would send one.
                                    (when-not (apply-literal-edit! document (.-payload msg))
                                      (extract-and-fan-out! !state panel document))
                                    nil))
                                "cg/run"
                                (let [model (get-in @!state [:editor/run-models (str (.-uri document))])]
                                  (if (= "js" (or (:runtime/resolved @!state)
                                                  (:runtime/choice @!state)))
                                    (exec-webview/run-document! !state document)
                                    (run-jvm-and-tail! !state panel document model)))

                                "cg/select-model"
                                (let [model (.-model msg)]
                                  (swap! !state assoc-in [:editor/run-models (str (.-uri document))] model)
                                  (.. panel -webview
                                      (postMessage #js {:type "cg/run-model" :model model})))

                                "cg/open-defop"
                                ;; Reuse a visible editor of the target. Else
                                ;; each click opens a new pane, because Beside
                                ;; is relative to the active editor, the canvas.
                                (when-let [^js loc (.-location msg)]
                                  (let [sl (.-startLine loc)
                                        sc (.-startColumn loc)
                                        el (.-endLine loc)
                                        ec (.-endColumn loc)
                                        r (vscode/Range.
                                           (vscode/Position. (dec sl) (dec sc))
                                           (vscode/Position. (dec el) (dec ec)))
                                        path (.-path msg)
                                        target-uri (if (and (string? path) (not= "" path))
                                                     (vscode/Uri.file path)
                                                     (.-uri document))
                                        target-uri-str (str target-uri)
                                        existing-editor
                                        (some (fn [^js ed]
                                                (when (= target-uri-str
                                                         (str (.. ed -document -uri)))
                                                  ed))
                                              (or vscode/window.visibleTextEditors
                                                  #js []))
                                        reuse-column (some-> existing-editor .-viewColumn)
                                        opts #js {:selection r
                                                  :preserveFocus false
                                                  :viewColumn (or reuse-column
                                                                  vscode/ViewColumn.Beside)}
                                        open-doc-promise
                                        (if (and (string? path) (not= "" path))
                                          (vscode/workspace.openTextDocument target-uri)
                                          (js/Promise.resolve document))]
                                    (-> open-doc-promise
                                        (.then (fn [^js doc]
                                                 (vscode/window.showTextDocument doc opts))
                                               (fn [_] nil))
                                        (.then (fn [_] nil) (fn [_] nil)))))

                                nil)))
      (swap! !state update :extension/disposables conj
             (.onDidChangeTextDocument vscode/workspace
                                       (fn [^js event]
                                         (when (= (str (.. event -document -uri)) (str (.-uri document)))
                                           (when-let [existing @!debounce]
                                             (js/clearTimeout existing))
                                           (reset! !debounce
                                                   (js/setTimeout
                                                    (fn [] (extract-and-fan-out! !state panel document))
                                                    debounce-ms))))))

      (extract-and-fan-out! !state panel document)
      (ensure-text-editor! panel document))))

(defn- register-custom-editor! [!state extension-uri]
  (lc/push-disposable! !state
                       (vscode/window.registerCustomEditorProvider
                        "cg.flowEditor"
                        #js {:resolveCustomTextEditor (resolve-editor !state extension-uri)}
                        #js {:supportsMultipleEditorsPerDocument true})))

(defn- register-document-listeners!
  "Workspace open and close listeners. They set and clear the diagnostics of
   each .cg document, also when the custom flow editor is not open."
  [!state]
  (lc/push-disposable! !state
                       (.onDidOpenTextDocument vscode/workspace
                                               (fn [^js doc]
                                                 (when (cg-doc/cg-uri? (.-uri doc))
                                                   (extract-and-fan-out! !state doc)))))
  (lc/push-disposable! !state
                       (.onDidCloseTextDocument vscode/workspace
                                                (fn [^js doc]
                                                  (when (cg-doc/cg-uri? (.-uri doc))
                                                    (when-let [^js coll (:editor/diagnostic-collection @!state)]
                                                      (diag/clear-diagnostics! coll (.-uri doc)))
                                                    (when-let [^js kcoll (:editor/kondo-collection @!state)]
                                                      (diag/clear-diagnostics! kcoll (.-uri doc))))))))

(defn- auto-split! [!state ^js text-editor]
  (when text-editor
    (let [doc (.-document text-editor)
          uri (.-uri doc)
          uri-str (str uri)]
      (when (and (cg-doc/cg-uri? uri)
                 (not (contains? (:editor/flow-editor-uris @!state) uri-str)))
        (swap! !state update :editor/flow-editor-uris conj uri-str)
        (-> (vscode/commands.executeCommand
             "vscode.openWith"
             uri
             "cg.flowEditor"
             (.-Beside vscode/ViewColumn))
            (.then
             (fn [_] nil)
             (fn [_err]
               (swap! !state update :editor/flow-editor-uris disj uri-str))))))))

(defn setup-split-view! [!state]
  (when-let [active (.-activeTextEditor vscode/window)]
    (auto-split! !state active))
  (lc/push-disposable! !state
                       (.onDidChangeActiveTextEditor vscode/window
                                                     (fn [^js text-editor]
                                                       (auto-split! !state text-editor)))))

(defn- register-theme-listener!
  "Posts cg/theme-kind to each open flow-editor panel when the VS Code color
   theme changes."
  [!state]
  (lc/push-disposable! !state
                       (.onDidChangeActiveColorTheme vscode/window
                                                     (fn [^js color-theme]
                                                       (let [mode (canvas-html/theme-kind->mode (.-kind color-theme))]
                                                         (doseq [[_uri-str ^js panel] (:editor/panels @!state)]
                                                           (.. panel -webview
                                                               (postMessage #js {:type "cg/theme-kind"
                                                                                 :mode mode}))))))))

(defn- register-cursor-listener!
  "Posts the caret line, 1-based like parser spans, to the panel of the
   document. The canvas selects the node that covers it."
  [!state]
  (lc/push-disposable!
   !state
   (.onDidChangeTextEditorSelection
    vscode/window
    (fn [^js e]
      (let [^js editor (.-textEditor e)
            uri-str (.toString (.. editor -document -uri))]
        (when-let [^js panel (get (:editor/panels @!state) uri-str)]
          (.. panel -webview
              (postMessage #js {:type "cg/cursor"
                                :line (inc (.. editor -selection -active -line))}))))))))

(defn- reveal-in-canvas!
  "The `CG: Reveal in Canvas` command. A hidden panel is revealed first and the
   message waits 300 ms, because the webview builds again on reveal."
  [!state]
  (when-let [^js editor (.-activeTextEditor vscode/window)]
    (let [uri-str (.toString (.. editor -document -uri))
          line    (inc (.. editor -selection -active -line))]
      (when-let [^js panel (get (:editor/panels @!state) uri-str)]
        (let [post! (fn []
                      (.. panel -webview
                          (postMessage #js {:type "cg/reveal-line"
                                            :line line})))]
          (if (.-visible panel)
            (post!)
            (do (.reveal panel js/undefined true)
                (js/setTimeout post! 300))))))))

(defn init-editor! [!state]
  (let [extension-uri (.. ^js (:extension/context @!state) -extensionUri)
        ^js coll (diag/create-collection!)
        ^js kondo-coll (diag/create-collection! "clj-kondo")]
    (swap! !state assoc :editor/diagnostic-collection coll)
    (swap! !state assoc :editor/kondo-collection kondo-coll)
    (lc/push-disposable! !state coll)
    (lc/push-disposable! !state kondo-coll)
    (register-document-listeners! !state)
    (register-custom-editor! !state extension-uri)
    (register-theme-listener! !state)
    (register-cursor-listener! !state)
    (lc/register-command! !state "cg.revealInCanvas"
                          reveal-in-canvas!)))
