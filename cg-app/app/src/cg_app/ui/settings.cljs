;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.settings
  "SettingsPanel: the runtime choice and the backend url and status."
  (:require ["react" :refer [useState]]
            [cg-app.ui.state :as state]
            [cg-app.ui.session :as session]))

(defn- RuntimeRadio [^js props]
  (let [value (.-value props)]
    #jsx [:label {:className "cg-settings-radio"}
          [:input {:type "radio" :name "cg-runtime" :value value
                   :checked (= (.-choice props) value)
                   :data-testid (str "runtime-" value)
                   :onChange (fn [_] (session/set-runtime! value))}]
          value]))

(defn SettingsPanel []
  (let [st (state/use-app-state)
        choice (or (:runtime-choice st) "auto")
        [url-draft set-url-draft] (useState (or (:backend-url st) ""))]
    #jsx [:div {:className "cg-settings" :data-testid "settings-panel"}
          [:div {:className "cg-settings-row"}
           [:span "Runtime:"]
           [RuntimeRadio {:choice choice :value "auto"}]
           [RuntimeRadio {:choice choice :value "jvm"}]
           [RuntimeRadio {:choice choice :value "js"}]]
          [:div {:className "cg-settings-row"}
           [:span "Backend:"]
           [:input {:type "text" :className "cg-settings-url" :value url-draft
                    :data-testid "backend-url-input"
                    :onChange (fn [^js e] (set-url-draft (.. e -target -value)))}]
           [:button {:data-testid "backend-url-apply"
                     :onClick (fn [_] (session/set-backend-url! url-draft))}
            "Apply"]]
          [:div {:className "cg-settings-row"}
           [:button {:data-testid "reprobe"
                     :onClick (fn [_] (session/probe-now!))}
            "Re-probe"]
           [:span {:data-testid "resolved-line"}
            (str "resolved: " (or (:runtime-resolved st) "js")
                 " | backend: " (cond (true? (:backend-available st)) "up"
                                      (false? (:backend-available st)) "down"
                                      :else "unknown"))]]]))
