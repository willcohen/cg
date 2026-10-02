;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.sidebar
  "The session-vars sidebar. A dataset toggle fetches GeoJSON for the map,
   and the toggles are disabled while the backend is down."
  (:require ["react" :refer [useState]]
            [cg-app.ui.state :as state]
            [cg-app.ui.session :as session]
            [cg-app.ui.layers :as layers]
            [cg-app.ui.runtime :as rt]
            [cg-app.ui.carbon :as c]))

(defn- ColorSwatch [^js props]
  #jsx [:span {:style #js {:width "12px" :height "12px" :borderRadius "50%"
                           :backgroundColor (.-color props)
                           :border "1px solid var(--cg-line-soft)"
                           :display "inline-block" :marginRight "8px"
                           :flex "none"}}])

(defn- toggle-dataset! [var-name color crs set-loading toggled?]
  (if toggled?
    (do
      (set-loading true)
      (-> (session/fetch-var-data! var-name)
          (.then (fn [geojson]
                   (state/add-map-layer! var-name color crs geojson)
                   (set-loading false)))
          (.catch (fn [e]
                    (js/console.error "failed to fetch" var-name e)
                    (set-loading false)))))
    (state/remove-map-layer! var-name)))

(defn- DatasetTile [^js props]
  (let [v (.-varInfo props)
        backend-up (true? (.-backendUp props))
        var-name (aget v "name")
        layer (state/get-layer var-name)
        visible? (and layer (true? (aget layer "visible?")))
        color (or (and layer (aget layer "color"))
                  (layers/next-color (.-idx props)))
        crs (layers/crs-str (aget v "crs"))
        [loading? set-loading] (useState false)
        subtitle (layers/var-subtitle v)]
    #jsx [:div {:className "cg-dataset-tile" :data-testid (str "dataset-" var-name)
                :style #js {:padding "8px 10px" :marginBottom "4px"
                            :background "var(--cg-surface)" :borderRadius "4px"
                            :borderLeft (str "3px solid "
                                             (if visible? color "var(--cg-line-soft)"))}}
          [:div {:style #js {:display "flex" :alignItems "center"
                             :justifyContent "space-between"}}
           [:span {:style #js {:display "flex" :alignItems "center"
                               :fontSize "13px" :fontWeight "500"}}
            [ColorSwatch {:color color}]
            var-name]
           (if loading?
             #jsx [c/InlineLoading {:description "" :status "active"}]
             #jsx [:span {:title (when-not backend-up
                                   "Connect JVM backend for map visualization")}
                   [c/Toggle {:id (str "toggle-" var-name)
                              :size "sm"
                              :labelText ""
                              :hideLabel true
                              :toggled (boolean visible?)
                              :disabled (not backend-up)
                              :onToggle (fn [t]
                                          (toggle-dataset! var-name color crs
                                                           set-loading t))}]])]
          (when (pos? (count subtitle))
            #jsx [:div {:style #js {:fontSize "11px" :opacity "0.7"
                                    :marginTop "2px"}}
                  subtitle])]))

(defn- VarListItem [^js props]
  #jsx [:div {:style #js {:padding "3px 10px" :fontSize "12px" :opacity "0.75"}}
        (aget (.-varInfo props) "name")])

(defn Sidebar [^js props]
  (let [st (state/use-app-state)
        vars (:session-vars st)
        backend-up (true? (:backend-available st))
        groups (layers/group-vars vars)
        datasets (aget groups "datasets")
        functions (aget groups "functions")
        values (aget groups "values")]
    #jsx [:div {:className "cg-sidebar" :data-testid "sidebar"
                :data-overlay (str (true? (.-overlay props)))}
          [:h3 {:style #js {:fontSize "11px" :textTransform "uppercase"
                            :letterSpacing "0.5px" :opacity "0.7"
                            :margin "6px 0 8px" :display "flex"
                            :alignItems "center" :gap "8px"}}
           "Session"
           (when (:session-loading? st)
             #jsx [c/InlineLoading {:description "" :status "active"}])]
          (when-not backend-up
            #jsx [:div {:data-testid "sidebar-backend-note"
                        :style #js {:fontSize "11px" :opacity "0.6"
                                    :padding "2px 0 8px"}}
                  (if (rt/js-only?)
                    "This page runs in your browser. The session list needs the JVM backend."
                    "Backend not connected")])
          (when (pos? (.-length datasets))
            #jsx [c/Accordion
                  [c/AccordionItem {:title (str "Datasets (" (.-length datasets) ")")
                                    :open true}
                   (.map datasets
                         (fn [v i]
                           #jsx [DatasetTile {:key (or (aget v "qualified") (aget v "name"))
                                              :varInfo v :idx i
                                              :backendUp backend-up}]))]])
          (when (pos? (.-length functions))
            #jsx [c/Accordion
                  [c/AccordionItem {:title (str "Functions (" (.-length functions) ")")}
                   (.map functions
                         (fn [v]
                           #jsx [VarListItem {:key (or (aget v "qualified") (aget v "name"))
                                              :varInfo v}]))]])
          (when (pos? (.-length values))
            #jsx [c/Accordion
                  [c/AccordionItem {:title (str "Values (" (.-length values) ")")}
                   (.map values
                         (fn [v]
                           #jsx [VarListItem {:key (or (aget v "qualified") (aget v "name"))
                                              :varInfo v}]))]])]))
