;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.editor
  (:require ["vscode" :as vscode]
            ["fs" :as fs]
            [clojure.string :as str]
            [cg-vscode.canvas-html :as canvas-html]
            [cg-vscode.diagnostics :as diag]
            [cg-vscode.doc :as cg-doc]
            [cg-vscode.exec-webview :as exec-webview]
            [cg-vscode.extension.lifecycle :as lc]
            [cg-vscode.extract :as extract]
            [cg-vscode.kondo :as kondo]
            [cg-vscode.modules :as modules]
            ["../../../cg-app/shared/squint/src/cg_gui/doc/overlay.mjs" :as overlay]
            ["../../../cg-app/shared/squint/src/cg_gui/run/js.mjs" :as run-js]))

(def ^:private debounce-ms 250)

(defn- read-brand-text
  "Returns nil when the file is missing."
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

(defn- run-model-for
  "The model that Run runs for the document `uri-str`: the canvas pick while
   the flow defines it, else nil. nil runs the last threading-macro binding."
  [!state uri-str cg-flow]
  (let [pick (get-in @!state [:editor/run-models uri-str])]
    (when (some #(= pick %) (run-js/model-names cg-flow))
      pick)))

(defn- post-run-model!
  "Forgets a pick that the flow does not define."
  [!state ^js panel uri-str cg-flow]
  (let [pick (run-model-for !state uri-str cg-flow)]
    (when-not pick
      (swap! !state update :editor/run-models dissoc uri-str))
    (.. panel -webview
        (postMessage #js {:type "cg/run-model"
                          :model (or pick (run-js/model-binding-name cg-flow))}))))

;; A parse can end after its canvas closed, and a post to a closed webview
;; throws.
(defonce ^:private closed-panels (js/WeakSet.))

(defn- post-flow-to-panel! [!state ^js panel uri-str cg-flow]
  (when-not (.has closed-panels panel)
    (swap! !state assoc-in [:editor/overlay-ids uri-str] (overlay/overlay-node-ids cg-flow))
    (.. panel -webview
        (postMessage #js {:type "cg/flow"
                          :flow (overlay/merge-positions cg-flow
                                                         (overlay/overlay-positions cg-flow))}))
    (post-run-model! !state panel uri-str cg-flow)))

(defn- extract-and-fan-out!
  "A second check clears the unresolved marks on module ops."
  ([!state document] (extract-and-fan-out! !state nil document))
  ([!state ^js panel ^js document]
   (let [text (.getText document)
         ^js coll (:editor/diagnostic-collection @!state)
         uri (.-uri document)
         ^js channel (:runtime/output-channel @!state)]
     (kondo/lint-document! (:editor/kondo-collection @!state) document)
     (-> (extract/extract-models text)
         (.then (fn [cg-flow]
                  (-> (modules/follow-loaded-modules! document cg-flow)
                      (.then (fn [extra-ops]
                               (if (seq extra-ops)
                                 (extract/recheck
                                  (update cg-flow :operations
                                          (fn [ops] (vec (concat ops extra-ops)))))
                                 cg-flow)))
                      ;; A module that does not parse must not stop the main flow.
                      (.catch (fn [err]
                                (.appendLine channel
                                             (str "CG: load-module follow failed "
                                                  (.-message err)))
                                cg-flow)))))
         (.then (fn [flow]
                  (diag/set-diagnostics! coll uri flow)
                  (when panel
                    (post-flow-to-panel! !state panel (str uri) flow))))
         (.catch (fn [err]
                   (.appendLine channel
                                (str "CG: parser error " (.-message err)))
                   (when-let [stack (.-stack err)]
                     (.appendLine channel
                                  (str "CG: parser stack\n" stack)))))))))

(defn- apply-position-edit! [^js document positions]
  (let [text (.getText document)]
    (-> (extract/extract-models text)
        (.then (fn [cg-flow]
                 (let [updated (overlay/update-visual-metadata text cg-flow positions)]
                   (when (not= text updated)
                     (let [edit (vscode/WorkspaceEdit.)]
                       (.replace edit (.-uri document)
                                 (vscode/Range. (vscode/Position. 0 0)
                                                (.positionAt document (.-length text)))
                                 updated)
                       (vscode/workspace.applyEdit edit)))))))))

(defn- apply-literal-edit!
  "Returns false when the span no longer holds the old literal."
  [^js document ^js payload]
  (let [loc (.-location payload)
        r (vscode/Range. (dec (aget loc "start-line")) (dec (aget loc "start-column"))
                         (dec (aget loc "end-line")) (dec (aget loc "end-column")))]
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
                       (.add closed-panels panel)
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
                                (exec-webview/run-document! !state document)

                                "cg/select-model"
                                (let [model (.-model msg)]
                                  (swap! !state assoc-in [:editor/run-models (str (.-uri document))] model)
                                  (.. panel -webview
                                      (postMessage #js {:type "cg/run-model" :model model})))

                                "cg/open-defop"
                                ;; Reuse a visible editor of the target. Else
                                ;; each click opens a new pane, because Beside
                                ;; is relative to the active editor, the canvas.
                                (let [^js loc (.-location msg)
                                      r (vscode/Range. (dec (.-startLine loc)) (dec (.-startColumn loc))
                                                       (dec (.-endLine loc)) (dec (.-endColumn loc)))
                                      path (.-path msg)
                                      other-file? (and (string? path) (not= "" path))
                                      target-uri (if other-file?
                                                   (vscode/Uri.file path)
                                                   (.-uri document))
                                      target-uri-str (str target-uri)
                                      existing-editor
                                      (some (fn [^js ed]
                                              (when (= target-uri-str
                                                       (str (.. ed -document -uri)))
                                                ed))
                                            vscode/window.visibleTextEditors)
                                      reuse-column (some-> existing-editor .-viewColumn)
                                      opts #js {:selection r
                                                :viewColumn (or reuse-column
                                                                vscode/ViewColumn.Beside)}
                                      open-doc-promise
                                      (if other-file?
                                        (vscode/workspace.openTextDocument target-uri)
                                        (js/Promise.resolve document))]
                                  (-> open-doc-promise
                                      (.then (fn [^js doc]
                                               (vscode/window.showTextDocument doc opts)))
                                      (.then (fn [_] nil) (fn [_] nil))))

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
  "Sets and clears the diagnostics of each .cg document, also when the custom
   flow editor is not open."
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
                                                    (diag/clear-diagnostics! (:editor/diagnostic-collection @!state) (.-uri doc))
                                                    (diag/clear-diagnostics! (:editor/kondo-collection @!state) (.-uri doc)))))))

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
  (auto-split! !state (.-activeTextEditor vscode/window))
  (lc/push-disposable! !state
                       (.onDidChangeActiveTextEditor vscode/window (partial auto-split! !state))))

(defn- register-theme-listener! [!state]
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
  "A hidden panel is revealed first and the message waits 300 ms, because the
   webview builds again on reveal."
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
