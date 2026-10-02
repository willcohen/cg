;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.toolbar
  "The op toolbar. A click on an op chip drops a ghost, and a drag drops it at the
   cursor. The chips are plain buttons, because they must be draggable."
  (:require ["react" :refer [useState]]
            ["@carbon/react" :refer [Button SelectableTag]]
            ["@carbon/react/icons" :refer [ChevronDown ChevronUp]]
            [cg-gui.canvas.op-catalog :as catalog]))

(def ^:private tb-bg "var(--cds-layer-01, var(--cg-surface, var(--vscode-editor-background, #1e1e1e)))")
(def ^:private tb-fg "var(--cds-text-primary, var(--cg-text, var(--vscode-foreground, #d4d4d4)))")
(def ^:private tb-line "var(--cds-border-subtle-01, var(--cg-line-soft, var(--vscode-panel-border, #444)))")
(def ^:private mono "var(--cg-font-mono, monospace)")

(defn- defop->ui-op
  "An OperationDecl (JS) -> the #js {name, desc} that the op button takes."
  [op]
  #js {:name (aget op "name")
       :desc (str (.-length (or (aget op "inputs") #js [])) " in / "
                  (.-length (or (aget op "outputs") #js [])) " out")})

(defn- loaded-defop-tabs
  "Groups the :operations of the CgFlow by :source-module label into one tab
   each, sorted. Operations with no source-module go in an \"Ops\" tab."
  [operations]
  (let [groups #js {}]
    (.forEach (or operations #js [])
              (fn [op]
                (let [sm    (aget op "source-module")
                      label (or (and sm (aget sm "label")) "Ops")]
                  (when-not (aget groups label) (aset groups label #js []))
                  (.push (aget groups label) (defop->ui-op op)))))
    (.map (.sort (js/Object.keys groups))
          (fn [label]
            #js {:id label :label label :ops (aget groups label)}))))

(defn- op-button [op on-add-ghost]
  (let [nm   (aget op "name")
        desc (aget op "desc")
        title (str nm
                   (when desc (str " — " desc))
                   "\nclick or drag onto the canvas")]
    #jsx [:button {:key nm
                   :className "cg-toolbar-op"
                   :title title
                   :draggable true
                   :onClick (fn [_] (on-add-ghost nm))
                   :onDragStart (fn [^js e]
                                  (.. e -dataTransfer (setData "application/cg-op" nm))
                                  (set! (.. e -dataTransfer -effectAllowed) "copy"))
                   :style #js {:padding "3px 9px" :margin "0 3px 0 0"
                               :border (str "1px solid " tb-line)
                               :background tb-bg :color tb-fg
                               :borderRadius "2px" :fontFamily mono
                               :fontSize "11px" :whiteSpace "nowrap" :cursor "grab"}}
          nm]))

(defn- tab-text [^js tab]
  (str (.-label tab) " (" (.-length (or (.-ops tab) #js [])) ")"))

(defn- TabTags
  "One Carbon selectable tag for each tab. A click on the open tab closes it."
  [^js props]
  (let [active (.-active props)
        set-active (.-setActive props)]
    #jsx [:div {:className "cg-toolbar-tags"
                :style #js {:display "flex" :flexWrap "wrap" :gap "4px"
                            :padding "6px 0 2px"}}
          (.map (.-tabs props)
                (fn [^js tab]
                  (let [id (.-id tab)]
                    #jsx [SelectableTag {:key id
                                         :className "cg-toolbar-tab"
                                         :size "sm"
                                         :text (tab-text tab)
                                         :selected (= id active)
                                         :onChange (fn [on?] (set-active (when on? id)))}])))]))

(defn- OpStrip [^js props]
  (let [on-add-ghost (.-onAddGhost props)]
    #jsx [:div {:className "cg-toolbar-ops"
                :style #js {:display "flex" :flexDirection "row" :flexWrap "wrap"
                            :gap "3px 0" :padding "6px 0 2px"
                            :maxHeight "40vh" :overflowY "auto"}}
          (.map (.-ops props) (fn [op] (op-button op on-add-ghost)))]))

(defn Toolbar
  "props: {operations <js array of OperationDecl>, onAddGhost (fn [name])}."
  [^js props]
  (let [operations   (aget props "operations")
        on-add-ghost (aget props "onAddGhost")
        tabs         (.concat catalog/builtins (loaded-defop-tabs operations))
        [open? set-open?]   (useState false)
        [active set-active] (useState nil)
        active-tab   (when active (.find tabs (fn [t] (= active (aget t "id")))))
        toggle!      (fn [_]
                       (when open? (set-active nil))
                       (set-open? (not open?)))]
    #jsx [:div {:className "cg-toolbar" :data-testid "op-toolbar"
                :data-open (str open?)
                :style #js {:background (when open? tb-bg)
                            :border (when open? (str "1px solid " tb-line))
                            :padding (when open? "4px 8px 6px")}}
          [Button {:className "cg-toolbar-toggle"
                   :kind "tertiary" :size "sm"
                   :renderIcon (if open? ChevronUp ChevronDown)
                   :aria-expanded (str open?)
                   :onClick toggle!}
           "Browse ops"]
          (when open?
            #jsx [TabTags {:tabs tabs :active active :setActive set-active}])
          (when (and open? active-tab)
            #jsx [OpStrip {:ops (aget active-tab "ops") :onAddGhost on-add-ghost}])]))
