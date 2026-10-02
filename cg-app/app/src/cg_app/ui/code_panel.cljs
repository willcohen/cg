;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.code-panel
  "The Monaco code panel. Monaco loads from the local monaco-editor package on
   first show and stays mounted."
  (:require ["react" :refer [useEffect useRef useState]]
            ["@monaco-editor/react" :refer [Editor loader]]
            [cg-app.ui.state :as state]
            [cg-app.ui.loader :refer [asset-url]]
            [cg-app.ui.codeview :as codeview]
            [cg-app.ui.carbon :as c]))

(.config loader #js {:paths #js {:vs (asset-url "monaco-editor/min/vs")}})

(defn- scrub-closure-uids!
  "Makes closure_uid_* prototype keys non-enumerable, because the Monaco ESM
   shim fails on them. Run it before each Monaco chunk load."
  []
  (doseq [proto [(.-prototype js/Function) (.-prototype js/Object)]]
    (.forEach (js/Object.keys proto)
              (fn [k]
                (when (.startsWith k "closure_uid_")
                  (js/Object.defineProperty proto k
                                            #js {:value (aget proto k)
                                                 :enumerable false
                                                 :writable true
                                                 :configurable true}))))))

(defn- FormatTab [^js props]
  (let [v (.-value props)]
    #jsx [c/Button {:kind (if (= v (.-fmt props)) "primary" "tertiary")
                    :size "sm"
                    :data-testid (str "code-format-" v)
                    ;; A language switch loads another Monaco chunk.
                    :onClick (fn [_]
                               (scrub-closure-uids!)
                               ((.-onSelect props) v))}
          (.-label props)]))

(def ^:private edit-delay-ms 250)

;; The send of the text that waits for edit-delay-ms, or nil.
(defonce ^:private !waiting (atom nil))

(defn send-waiting-edit!
  "Sends the text of the editor now when it waits for edit-delay-ms, and
   gives what :onEdit gives, or nil."
  []
  (when-let [send! @!waiting] (send!)))

(defn- put-text!
  "Replaces the editor text and keeps the cursor, the scroll and the undo
   history. A read-only editor gets the text through its model."
  [^js ed text editable]
  (let [view (.saveViewState ed)
        model (.getModel ed)]
    (if editable
      (do (.executeEdits ed "cg" #js [#js {:range (.getFullModelRange model)
                                           :text text
                                           :forceMoveMarkers true}])
          (.pushUndoStop ed))
      (.setValue model text))
    (.restoreViewState ed view)))

(defn CodePanel
  "props: :visible, and :onEdit, which takes the text of the CG DSL tab
   edit-delay-ms after the last key."
  [^js props]
  (let [st (state/use-app-state)
        [fmt set-fmt] (useState "dsl")
        [shown set-shown] (useState false)
        visible (true? (.-visible props))
        file (:loaded-file st)
        ;; The Flow JSON tab is a view of the flow, and there is no text to
        ;; edit before a load.
        editable (and (= fmt "dsl") (some? file))
        ;; Text that does not parse came from the editor with its overlay,
        ;; and the overlay lines of the flow in the store are those of an
        ;; older text.
        content (if (and editable (:unparsed file))
                  (:text file)
                  (codeview/content-for fmt (:text file) (:flow st) (state/positions)))
        editor (useRef nil)
        ;; @monaco-editor/react calls the onMount of its first render. That
        ;; sync! must put the content of the last render, and not an older
        ;; one over the text of a file that loaded while Monaco loaded.
        latest (useRef nil)
        ;; False while put-text! runs, and in a tab that is not editable: a
        ;; change of the model is then not a key of the user.
        typing (useRef false)
        timer (useRef nil)
        sent (useRef nil)
        ;; The store text goes into the editor, except when it is the text
        ;; that the editor sent: the user can be some keys ahead of it.
        sync! (fn []
                (when-let [ed (.-current editor)]
                  (let [now (.-current latest)
                        text (aget now "content")
                        can-edit (aget now "editable")]
                    (when-not (or (= text (.-current sent)) (= text (.getValue ed)))
                      (js/clearTimeout (.-current timer))
                      (reset! !waiting nil)
                      (set! (.-current sent) nil)
                      (set! (.-current typing) false)
                      (put-text! ed text can-edit)
                      (set! (.-current typing) can-edit)))))
        send! (fn [text]
                (js/clearTimeout (.-current timer))
                (reset! !waiting nil)
                (set! (.-current sent) text)
                ((.-onEdit props) text))
        ;; The other tab has another model, and its content would put the
        ;; store text over the keys that wait.
        select-fmt (fn [v]
                     (send-waiting-edit!)
                     (set-fmt v))
        ;; The text of the last key, not the model at the send: a switch to
        ;; the Flow JSON tab during the delay changes the model.
        on-change (fn [text _]
                    (when (.-current typing)
                      (js/clearTimeout (.-current timer))
                      (let [send-text! (fn [] (send! text))]
                        (reset! !waiting send-text!)
                        (set! (.-current timer) (js/setTimeout send-text! edit-delay-ms)))))]
    (set! (.-current typing) editable)
    (set! (.-current latest) #js {:content content :editable editable})
    (useEffect
     (fn []
       (sync!)
       js/undefined)
     #js [content])
    (useEffect
     (fn []
       (when visible
         (scrub-closure-uids!)
         (set-shown true))
       js/undefined)
     #js [visible])
    #jsx [:div {:className "cg-code-panel" :data-testid "code-panel"}
          [:div {:className "cg-code-tabs"}
           [FormatTab {:value "dsl" :label "CG DSL" :fmt fmt :onSelect select-fmt}]
           [FormatTab {:value "flow" :label "Flow JSON" :fmt fmt :onSelect select-fmt}]]
          ;; React Flow takes the Space and Backspace keys of the page, except
          ;; under .nokey. Monaco edits in a div, which React Flow does not
          ;; see as an input.
          [:div {:className "cg-code-editor nokey"}
           (when shown
             #jsx [Editor
                   {:height "100%"
                    :language (if (= fmt "dsl") "clojure" "json")
                    :theme (if (= "dark" (state/theme-mode (:theme st))) "vs-dark" "vs")
                    ;; One model for each tab: undo in the CG DSL tab never
                    ;; brings back the JSON.
                    :path fmt
                    :defaultValue content
                    :onChange on-change
                    :onMount (fn [ed]
                               (set! (.-current editor) ed)
                               (sync!))
                    :options #js {:readOnly (not editable)
                                  :minimap #js {:enabled false}
                                  :fontSize 13
                                  :lineNumbers "on"
                                  :scrollBeyondLastLine false
                                  :automaticLayout true
                                  :tabSize 2}}])]]))
