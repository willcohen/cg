;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.crs-picker
  "The CRS picker modal. Uses Carbon ComposedModal and Search directly. The
   list filter is in cg-gui.map.sync."
  (:require ["react" :refer [useEffect useState]]
            ["@carbon/react" :refer [ComposedModal ModalHeader ModalBody Search]]
            ["../../../../shared/squint/src/cg_gui/map/sync.mjs" :as ms]
            [cg-app.ui.projection :as projection]))

(defn- CrsRow [^js props]
  (let [entry (.-entry props)
        code (str (aget entry "authName") ":" (aget entry "code"))]
    #jsx [:div {:className "cg-crs-row"
                :data-selected (str (= code (.-currentCrs props)))
                :onClick (fn [_]
                           ((.-onSelect props) code)
                           ((.-onClose props)))}
          [:div {:style #js {:fontWeight "600"}} (aget entry "name")]
          [:div {:style #js {:fontSize "12px" :opacity "0.6"}}
           (str code
                (when (aget entry "areaName")
                  (str " -- " (aget entry "areaName"))))]]))

(defn CrsPickerModal [^js props]
  (let [open (true? (.-open props))
        [search set-search] (useState "")
        [crs-list set-crs-list] (useState nil)
        [loading? set-loading] (useState false)
        [error set-error] (useState nil)]
    (useEffect
     (fn []
       (when (and open (nil? crs-list) (not loading?))
         (set-loading true)
         (-> (projection/get-crs-list)
             (.then (fn [list]
                      (set-crs-list list)
                      (set-loading false)))
             (.catch (fn [e]
                       (set-error (.-message e))
                       (set-loading false)))))
       js/undefined)
     #js [open])
    (let [filtered (when crs-list (ms/filter-crs-list crs-list search))]
      #jsx [ComposedModal {:open open
                           :onClose (.-onClose props)
                           :size "lg"}
            [ModalHeader {:title "Select Coordinate Reference System"}]
            [ModalBody
             [Search {:id "crs-search"
                      :labelText "Search CRS"
                      :placeholder "Search by name, code, or area..."
                      :value search
                      :onChange (fn [^js e] (set-search (.. e -target -value)))}]
             [:div {:style #js {:marginTop "16px" :maxHeight "400px"
                                :overflowY "auto"}}
              [:div {:className "cg-crs-row"
                     :data-testid "crs-mercator"
                     :data-selected (str (nil? (.-currentCrs props)))
                     :onClick (fn [_]
                                ((.-onSelect props) nil)
                                ((.-onClose props)))}
               [:div {:style #js {:fontWeight "600"}} "Web Mercator (default)"]
               [:div {:style #js {:fontSize "12px" :opacity "0.6"}}
                "EPSG:3857 -- No reprojection"]]
              (cond
                loading?
                #jsx [:div {:style #js {:padding "16px" :textAlign "center"}}
                      "Loading CRS database..."]
                error
                #jsx [:div {:style #js {:padding "16px" :color "var(--cg-error)"}}
                      (str "Error: " error)]
                filtered
                #jsx [:div
                      (.map filtered
                            (fn [entry]
                              #jsx [CrsRow {:key (str (aget entry "authName") ":"
                                                      (aget entry "code"))
                                            :entry entry
                                            :currentCrs (.-currentCrs props)
                                            :onSelect (.-onSelect props)
                                            :onClose (.-onClose props)}]))]
                :else nil)]]])))
