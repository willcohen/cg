;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.app
  "The app shell: header chrome and the shared canvas in one window. The
   shell and the canvas talk over window postMessage."
  (:require ["react" :refer [Fragment useEffect useRef useState]]
            ["react-dom/client" :refer [createRoot]]
            [cg-app.ui.state :as state]
            [cg-app.ui.save :as save]
            [cg-app.ui.loader :as loader]
            [cg-app.ui.open-url :as open-url]
            [cg-app.ui.extract :as extract]
            [cg-app.ui.runtime :as rt]
            ["../../../../shared/squint/src/cg_gui/run/js.mjs" :as run-js]
            ["../../../../shared/squint/src/cg_gui/run/lane.mjs" :as lane]
            ["../../../../shared/squint/src/cg_gui/doc/literal.mjs" :as literal]
            [cg-app.ui.session :as session]
            [cg-app.ui.execution :as execution]
            [cg-app.ui.settings :as settings]
            [cg-app.ui.code-panel :as code-panel]
            [cg-app.ui.sidebar :as sidebar]
            [cg-app.ui.map-panel :as map-panel]
            [cg-app.ui.status :as status]
            [cg-app.ui.status-view :as status-view]
            [cg-app.ui.splitter :as splitter]
            [cg-app.ui.viewport :as viewport]
            [cg-app.ui.carbon :as c]))

(defn- ResultsPanel [^js _props]
  (let [st (state/use-app-state)
        t  (:report-result st)
        sc (:scalar-result st)]
    (cond
      sc
      #jsx [:div {:className "cg-results" :data-testid "results-scalar"}
            [:div {:className "cg-results-title"} (str (aget sc "name"))]
            [:div {:className "cg-results-badge"} (str (aget sc "text"))]]

      (not t)
      #jsx [:div {:className "cg-results-empty" :data-testid "results-empty"}
            "No report result yet. Run a flow whose result is a report summary (summarize / block-presence)."]

      :else
      #jsx [:div {:className "cg-results" :data-testid "results-table"}
            [:div {:className "cg-results-title"} (str (aget t "title"))]
            [:table {:className "cg-results-tbl"}
             [:thead
              [:tr
               (.map (or (aget t "columns") #js [])
                     (fn [c i]
                       #jsx [:th {:key (str "h" i)} (str c)]))]]
             [:tbody
              (.map (or (aget t "rows") #js [])
                    (fn [row ri]
                      #jsx [:tr {:key (str "r" ri)}
                            (.map row
                                  (fn [cell ci]
                                    #jsx [:td {:key (str "c" ri "-" ci)} (str cell)]))]))]]])))

(defn- grammar-wasm-url []
  (loader/asset-url "@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm"))

(defonce ^:private !canvas-mounted (atom false))

(defn ^:async mount-canvas-once!
  "Imports the shared canvas and mounts it into #root, one time. Call it after
   the Shell commits, because the Shell renders #root."
  []
  (when-not @!canvas-mounted
    (reset! !canvas-mounted true)
    (let [canvas (await (js* "import(~{})" "cg-app/canvas"))]
      ((aget canvas "mount") "root"))))

(defn- post!
  "Posts `msg` on the window bus. The canvas listens for window 'message'."
  [msg]
  (.postMessage js/window msg "*"))

(defn post-flow!
  "Merges the positions into the loaded flow and posts cg/flow. Then tells the
   canvas which model Run runs (cg/run-model)."
  []
  (let [{:keys [flow run-model load-n]} @state/app-state]
    (when flow
      (post! #js {:type "cg/flow"
                  :flow (js/Object.assign
                         (extract/merge-positions-into-flow flow (state/positions))
                         #js {:loadId load-n})})
      (post! #js {:type "cg/run-model"
                  :model (or run-model (run-js/model-binding-name flow))}))))

(defn- select-model!
  "Makes `model-name` the model that Run runs."
  [model-name]
  (state/set-run-model! model-name)
  (post! #js {:type "cg/run-model" :model model-name}))

(defn- ModelMenu [^js props]
  (let [names (run-js/model-names (.-flow props))]
    (when (> (count names) 1)
      #jsx [:select {:className "cg-shell-models"
                     :data-testid "model-menu"
                     :title "The model that Run runs"
                     :value (or (.-selected props) (last names))
                     :onChange (fn [^js e] (select-model! (.. e -target -value)))}
            (.map names (fn [n] #jsx [:option {:key n :value n} n]))])))

(defn- set-page-url!
  "Puts `url` in the url query parameter of the address bar, or removes the
   parameter when `url` is nil."
  [url]
  ;; A native shell can refuse a history change, and the load must go on.
  (try (.replaceState js/history nil "" (open-url/page-href (.-href js/location) url))
       (catch :default _ nil)))

(defn ^:async process-text!
  "Loads a .cg file: decodes the overlay, parses, follows the loaded modules,
   stores the flow and posts it."
  [filename text]
  (set-page-url! nil)
  (try
    (let [file-pos (try
                     (let [mod (await (loader/load-cg-module!))]
                       (state/set-module-status! "loaded")
                       (loader/overlay-positions (loader/metadata-mod mod) text))
                     (catch :default e
                       (state/set-module-status! "error")
                       (js/console.warn "cg module codec unavailable; rendering without overlay positions:" e)
                       #js {}))
          ;; :opaque-blocks makes a form that the parser cannot model show as
          ;; a read-only card.
          flow0 (await (extract/extract-flow text {:grammar-wasm (grammar-wasm-url)
                                                   :opaque-blocks true}))
          flow (await (extract/follow-loaded-modules! flow0 {:grammar-wasm (grammar-wasm-url)}))
          diags (or (aget flow "diagnostics") #js [])]
      (state/merge-positions! file-pos)
      (state/set-loaded-flow! filename text flow)
      (status/reset-run!)
      (when (pos? (.-length diags))
        (js/console.warn "cg parser diagnostics:" diags))
      (post-flow!)
      ;; A session sync failure must not stop the load.
      (when (true? (:backend-available @state/app-state))
        (-> (session/sync-file! text filename)
            (.catch (fn [e] (js/console.warn "session sync failed:" e))))))
    (catch :default e
      (state/set-load-error! (or (.-message e) (str e))))))

(defn- ^:async parse-edit
  "{:flow} for `text`, or {:unparsed <reason>} when the text is in the middle
   of an edit or the parser throws."
  [text]
  (let [opts {:grammar-wasm (grammar-wasm-url)}]
    (if-let [open (literal/unclosed text)]
      {:unparsed open}
      (try
        {:flow (await (extract/follow-loaded-modules!
                       (await (extract/extract-flow text (assoc opts :opaque-blocks true)))
                       opts))}
        (catch :default e
          {:unparsed (str "a form that the parser cannot read ("
                          (or (.-message e) (str e)) ")")})))))

(defonce ^:private !edit-n (atom 0))

(defn ^:async edit-text!
  "Stores, parses and posts the new text of the loaded file. Text that does
   not parse keeps the last flow, and the canvas stays as it was."
  [text]
  (swap! !edit-n inc)
  (let [n @!edit-n
        {:keys [load-n loaded-file]} @state/app-state
        {:keys [flow unparsed]} (await (parse-edit text))]
    ;; A file load or a later edit during the parse wins over this edit.
    (when (and (= load-n (:load-n @state/app-state)) (= n @!edit-n))
      (state/set-edited-text! text flow unparsed)
      (when flow
        (post-flow!)
        (when (true? (:backend-available @state/app-state))
          (-> (session/sync-file! text (:name loaded-file))
              (.catch (fn [e] (js/console.warn "session sync failed:" e)))))))))

(defonce ^:private !last-edit (atom nil))

(defn- start-edit!
  "Runs edit-text! and keeps its promise for settle-edits!."
  [text]
  (let [p (edit-text! text)]
    (reset! !last-edit p)
    p))

(defn- ^:async settle-edits!
  "Waits for the text that waits in the code panel and for the parse of the
   last edit, because Run and Save read the stored text."
  []
  (code-panel/send-waiting-edit!)
  (when-let [p @!last-edit]
    (await p)))

(defn- ^:async run-flow! []
  (await (settle-edits!))
  (await (execution/execute!)))

(defn- ^:async save-file! []
  (await (settle-edits!))
  (save/save!))

(defn- set-literal!
  "Puts the new source text of one literal in place of its span. A span that
   no longer holds the old literal refuses the edit, and the flow posts again."
  [^js payload]
  (if-let [edited (literal/splice (:text (:loaded-file @state/app-state))
                                  (aget payload "location")
                                  (aget payload "was")
                                  (aget payload "text"))]
    (start-edit! edited)
    (post-flow!)))

(defn- ^:async load-list!
  "The names in examples/index.json next to the page, or nil when the page
   has no examples."
  []
  (try
    (let [resp (await (js/fetch "examples/index.json"))]
      (when (.-ok resp) (await (.json resp))))
    (catch :default _ nil)))

(defn- ^:async load-example! [name]
  (let [resp (await (js/fetch (str "examples/" name)))]
    (if (.-ok resp)
      (await (process-text! name (await (.text resp))))
      (state/set-load-error! (str name ": HTTP " (.-status resp))))))

(defn ^:async open-url!
  "Gets the .cg file at the URL in `text` and loads it. Does not run it."
  [text]
  (let [src (open-url/source-url text (.-href js/location))
        url (aget src "url")]
    (if-not url
      (state/show-url-bar! text (aget src "error"))
      (do
        (state/set-opening! (open-url/file-name url))
        (try
          (let [body (await (open-url/fetch-text url))]
            (state/hide-url-bar!)
            (await (process-text! (open-url/file-name url) body))
            (set-page-url! url))
          (catch :default e
            (state/set-load-error! (.-message e))
            (state/show-url-bar! url (.-message e)))
          (finally
            (state/set-opening! nil)))))))

(defn- UrlBar
  "The text box for the URL of a .cg file."
  [^js props]
  (let [bar (.-bar props)
        opening (.-opening props)
        [typed set-typed] (useState (or (:url bar) ""))]
    #jsx [:form {:className "cg-url-bar" :data-testid "url-bar" :noValidate true
                 :onSubmit (fn [^js e] (.preventDefault e) (open-url! typed))}
          [:label {:className "cg-url-label" :htmlFor "cg-url-input"} "URL of a .cg file"]
          [:input {:id "cg-url-input" :className "cg-url-input" :type "url"
                   :data-testid "url-input" :autoFocus true :spellCheck false
                   :placeholder "https://…/file.cg" :value typed
                   :onChange (fn [^js e] (set-typed (.. e -target -value)))}]
          [:button {:type "submit" :className "cg-url-open" :data-testid "url-open"
                    :disabled (boolean opening)}
           (if opening "Opening…" "Open")]
          [:button {:type "button" :className "cg-url-close" :data-testid "url-close"
                    :title "Close" :aria-label "Close"
                    :onClick (fn [_] (state/hide-url-bar!))}
           "×"]
          (if-let [err (:error bar)]
            #jsx [:p {:className "cg-url-error" :data-testid "url-error" :role "alert"} err]
            #jsx [:p {:className "cg-url-note"}
                  "Open loads the file and shows it. It does not run the file."])]))

(defn- ExamplesMenu [^js props]
  (let [names (.-names props)]
    (when (and names (pos? (.-length names)))
      #jsx [:select {:className "cg-shell-examples"
                     :data-testid "examples-menu"
                     :value ""
                     :onChange (fn [^js e] (load-example! (.. e -target -value)))}
            [:option {:value "" :disabled true} "Examples"]
            (.map names (fn [n] #jsx [:option {:key n :value n} n]))])))

(defn- EmptyCanvas
  "The first step on an empty canvas: an example, else a .cg file."
  [^js props]
  (let [example (state/starter-example (.-examples props))]
    (if-let [opening (.-opening props)]
      #jsx [:div {:className "cg-empty" :data-testid "empty-canvas"}
            [:p {:className "cg-empty-title"} (str "Opening " opening)]
            [:p {:className "cg-empty-text"} "The app gets the file from its URL."]]
      #jsx [:div {:className "cg-empty" :data-testid "empty-canvas"}
            [:p {:className "cg-empty-title"} "No file loaded"]
            [:p {:className "cg-empty-text"}
             (if example
               "Open an example to see its flow, its code and its map."
               "Load a .cg file to see its flow and its code.")]
            (if example
              #jsx [c/Button {:size "md" :data-testid "empty-open-example"
                              :onClick (fn [_] (load-example! example))}
                    (str "Open " example)]
              #jsx [c/Button {:size "md" :data-testid "empty-load-file"
                              :onClick (.-onLoadFile props)}
                    "Load .cg file"])])))

(defn- handle-file-select [^js event]
  (when-let [file (aget (.. event -target -files) 0)]
    (let [reader (js/FileReader.)]
      (set! (.-onload reader)
            (fn [^js e]
              (process-text! (.-name file) (.. e -target -result))))
      (.readAsText reader file))))

(defn- apply-theme-attrs!
  "Sets data-carbon-theme and data-theme together."
  [theme]
  (let [root (.-documentElement js/document)]
    (.setAttribute root "data-carbon-theme" theme)
    (.setAttribute root "data-theme" (state/theme-mode theme))))

(defn- toggle-or-select!
  "Toggles panel `k`. On a phone, shows only panel `k`."
  [bp k]
  (if (viewport/phone? bp)
    (state/select-panel! k)
    (state/toggle-panel! k)))

(defn- show-panel!
  "Shows panel `k`. On a phone, shows only panel `k`."
  [bp k]
  (cond
    (viewport/phone? bp) (state/select-panel! k)
    (not (true? (aget (state/panels) k))) (state/toggle-panel! k)))

(defn- hidden-outputs
  "#js {:map :results}: true for a hidden panel that holds output of the last
   run."
  [st]
  (let [panels (:panels st)
        hidden? (fn [k] (not (true? (aget panels k))))]
    #js {:map (and (hidden? "map")
                   (.some (:map-layers st) (fn [l] (true? (aget l "visible?")))))
         :results (and (hidden? "results")
                       (boolean (or (:report-result st) (:scalar-result st))))}))

(def ^:private status-panel-delay-ms 800)

(defn- show-status-of-long-run!
  "Opens the Status panel when the run still runs after status-panel-delay-ms.
   A short run keeps the room for its result."
  [bp]
  (js/setTimeout
   (fn []
     (when (= "running" (:status (:exec @state/app-state)))
       (show-panel! bp "status")))
   status-panel-delay-ms))

(defn- reveal-outputs!
  "Opens each hidden panel that holds output of the run that ended. A phone
   shows one panel, and there the reader picks it in the status bar."
  [bp]
  (when-not (viewport/phone? bp)
    (let [outs (hidden-outputs @state/app-state)]
      (when (aget outs "map") (show-panel! bp "map"))
      (when (aget outs "results") (show-panel! bp "results")))))

(defn- dismiss-run-status! []
  (state/clear-exec!)
  (status/reset-run!))

(defn- PanelToggle [^js props]
  (let [k (.-k props)]
    #jsx [:button {:className "cg-shell-panel-toggle"
                   :data-testid (str "panel-toggle-" k)
                   :aria-pressed (str (true? (aget (.-panels props) k)))
                   :onClick (fn [_] ((.-onToggle props) k))}
          (.-label props)]))

(def ^:private panel-defs
  #js [#js ["flow" "Flow"] #js ["code" "Code"] #js ["status" "Status"]
       #js ["map" "Map"] #js ["results" "Results"]])

(defn- PanelToggles
  "The panel toggles: a group in the header, or the tab bar at the bottom of a
   phone."
  [^js props]
  #jsx [:nav {:className (.-className props) :aria-label "Panels"
              :data-testid (.-testid props)}
        (.map panel-defs
              (fn [d]
                #jsx [PanelToggle {:key (aget d 0) :k (aget d 0) :label (aget d 1)
                                   :panels (.-panels props)
                                   :onToggle (.-onToggle props)}]))])

(defn- shown-keys
  "The keys of the visible panels, in panel order."
  [panels]
  (.filter (.map panel-defs (fn [d] (aget d 0)))
           (fn [k] (true? (aget panels k)))))

(defn- panel-label [k]
  (aget (.find panel-defs (fn [d] (= k (aget d 0)))) 1))

(defn- Border
  "The border after panel `k` when a shown panel follows it."
  [^js props]
  (let [shown (.-shown props)
        k (.-k props)
        right (when (.includes shown k) (splitter/next-shown shown k))]
    (when right
      #jsx [splitter/Splitter {:shown shown :left k :right right
                               :leftLabel (panel-label k)
                               :rightLabel (panel-label right)
                               :onChange (.-onChange props)}])))

(defn- one-panel!
  "Leaves one panel visible: Flow when it is visible, else the first visible
   one."
  []
  (let [shown (shown-keys (state/panels))]
    (when (not= 1 (.-length shown))
      (state/select-panel! (if (or (zero? (.-length shown)) (.includes shown "flow"))
                             "flow"
                             (aget shown 0))))))

(defn- StatusLine [^js props]
  (let [st (.-st props)
        line (status/status-line (:load-error st)
                                 (:name (:loaded-file st))
                                 (state/diagnostics st))]
    #jsx [:span {:className "cg-shell-status" :data-testid "status"
                 :data-status (.-kind line)
                 :title (.-title line)}
          (.-text line)]))

(defn Shell []
  (let [st (state/use-app-state)
        theme (:theme st)
        panels (:panels st)
        bp (viewport/use-breakpoint)
        phone? (viewport/phone? bp)
        shown (shown-keys panels)
        ;; A phone shows one panel, and it has no borders to drag.
        sizes (when-not phone? (splitter/sizes-for (splitter/set-key shown)))
        [_ set-sizes-n] (useState 0)
        on-sizes (fn [] (set-sizes-n inc))
        border (fn [k]
                 (when-not phone?
                   #jsx [Border {:shown shown :k k :onChange on-sizes}]))
        panel-attrs (fn [k class-name]
                      #js {:id (str "cg-panel-" k)
                           :className class-name
                           :data-testid (str "panel-" k)
                           :data-panel-hidden (str (not (true? (aget panels k))))
                           :style (splitter/panel-style sizes k)})
        on-panel-toggle (fn [k] (toggle-or-select! bp k))
        file-input (useRef nil)
        open-file-picker (fn [_] (when-let [el (.-current file-input)] (.click el)))
        ;; The Direct edition starts the embedded JVM about 2 s after launch
        ;; and defaults the runtime to jvm. Hold Run until the first
        ;; /api/health ok (:backend-ready?). Other editions do not wait.
        connecting? (and (rt/direct-edition?) (not (:backend-ready? st)))]
    (useEffect
     (fn []
       (apply-theme-attrs! theme)
       (post! #js {:type "cg/theme-kind" :mode (state/theme-mode theme)})
       js/undefined)
     #js [theme])
    ;; After the commit, one time: #root now exists in the DOM.
    (useEffect
     (fn []
       (mount-canvas-once!)
       js/undefined)
     #js [])
    (useEffect
     (fn []
       (when phone? (one-panel!))
       js/undefined)
     #js [phone?])
    (useEffect
     (fn []
       (case (:status (:exec st))
         "running" (show-status-of-long-run! bp)
         "ok"      (reveal-outputs! bp)
         nil)
       js/undefined)
     #js [(:exec st)])
    #jsx [Fragment
          [:header {:className "cg-shell-header"}
           [:div {:className "cg-hdr-group cg-hdr-brand"}
            (when-not (rt/js-only?)
              #jsx [:button {:className "cg-shell-sidebar-toggle"
                             :data-testid "sidebar-toggle"
                             :title "Session variables"
                             :aria-label "Session variables"
                             :onClick (fn [_] (state/toggle-sidebar!))}
                    "≡"])
            [:span {:className "cg-shell-title"} "CG"]]
           [:div {:className "cg-hdr-group cg-hdr-file"}
            [:input {:ref file-input :type "file" :accept ".cg"
                     :style #js {:display "none"}
                     :onChange handle-file-select
                     :data-testid "file-input"}]
            [:button {:className "cg-shell-load"
                      :title "Open a .cg file from this device"
                      :onClick open-file-picker}
             "Load .cg"]
            [:button {:className "cg-shell-open-url"
                      :data-testid "open-url-button"
                      :title "Open a .cg file from a URL"
                      :aria-expanded (str (some? (:url-bar st)))
                      :onClick (fn [_] (state/toggle-url-bar!))}
             "Open URL"]
            [ExamplesMenu {:names (:examples st)}]
            [:button {:className "cg-shell-save"
                      :data-testid "save-button"
                      :title "Save the .cg file with the positions of its cards"
                      :disabled (not (:flow st))
                      :onClick (fn [_] (save-file!))}
             "Save"]]
           [:div {:className "cg-hdr-group cg-hdr-run"}
            [ModelMenu {:flow (:flow st) :selected (:run-model st)}]
            [:button {:className "cg-shell-run"
                      :data-testid "run-button"
                      :disabled (or (not (and (:flow st) (= "loaded" (:module-status st))))
                                    connecting?)
                      :onClick (fn [_] (run-flow!))}
             "Run"]
            (when connecting?
              #jsx [:span {:className "cg-shell-connecting"
                           :data-testid "connecting-notice"
                           :title "Starting the embedded JVM engine. Run is enabled once it answers."}
                    [:span {:className "cg-shell-connecting-dot"}]
                    "Connecting to engine…"])]
           [StatusLine {:st st}]
           (when-not phone?
             #jsx [PanelToggles {:className "cg-hdr-group cg-hdr-panels" :testid "panel-toggles"
                                 :panels panels :onToggle on-panel-toggle}])
           [:div {:className "cg-hdr-group cg-hdr-tools"}
            [:button {:className "cg-shell-theme"
                      :data-testid "theme-toggle"
                      :title "Switch between the light and the dark theme"
                      :onClick (fn [_] (state/toggle-theme!))}
             (if (= "g100" theme) "Light" "Dark")]
            (when-not (rt/js-only?)
              #jsx [:button {:className "cg-shell-chip"
                             :data-testid "runtime-chip"
                             :onClick (fn [_] (state/toggle-settings!))}
                    (lane/chip-text (:runtime-choice st) (:runtime-resolved st))])]]
          (when (:settings-open st)
            #jsx [settings/SettingsPanel])
          (when-let [bar (:url-bar st)]
            #jsx [UrlBar {:bar bar :opening (:opening st)}])
          (when (:backend-flapping? st)
            #jsx [:div {:className "cg-exec-banner"
                        :data-testid "backend-flap-banner"
                        :data-exec-status "error"}
                  "The engine keeps restarting. Try quitting and reopening CG."])
          [status-view/RunStatus {:exec (:exec st)
                                  :outputs (hidden-outputs st)
                                  :onShow (fn [k] (show-panel! bp k))
                                  :onDismiss dismiss-run-status!}]
          [:div {:className "cg-main"}
           (when (and (:sidebar-open? st) (viewport/phone? bp))
             #jsx [:div {:className "cg-sidebar-backdrop"
                         :data-testid "sidebar-backdrop"
                         :onClick (fn [_] (state/toggle-sidebar!))}])
           (when (:sidebar-open? st)
             #jsx [sidebar/Sidebar {:overlay (viewport/phone? bp)}])
           [:div {:& (panel-attrs "flow" "cg-panel")}
            [:div {:id "root"}]
            (when-not (or (:flow st) (:loaded-file st))
              #jsx [EmptyCanvas {:examples (:examples st)
                                 :opening (:opening st)
                                 :onLoadFile open-file-picker}])]
           (border "flow")
           [:div {:& (panel-attrs "code" "cg-panel")}
            [code-panel/CodePanel {:visible (true? (aget panels "code"))
                                   :onEdit start-edit!}]]
           (border "code")
           [:div {:& (panel-attrs "status" "cg-panel cg-panel-status")}
            [status-view/RunPane {:exec (:exec st)}]]
           (border "status")
           [:div {:& (panel-attrs "map" "cg-panel")}
            [map-panel/MapPanel {:visible (true? (aget panels "map"))}]]
           (border "map")
           [:div {:& (panel-attrs "results" "cg-panel")}
            [ResultsPanel]]]
          (when phone?
            #jsx [PanelToggles {:className "cg-tabbar" :testid "panel-tabbar"
                                :panels panels :onToggle on-panel-toggle}])]))

(defn- on-bus-message [^js e]
  (let [m (.-data e)
        t (when m (aget m "type"))]
    (cond
      (= t "cg/ready") (post-flow!)
      (and (= t "cg/edit") (= "SetLiteral" (aget m "editType")))
      (set-literal! (aget m "payload"))
      ;; The layout pins a node that has a position.
      (= t "cg/edit")
      (when (= "DragNode" (aget m "editType"))
        (let [payload (aget m "payload")
              nid (aget payload "nodeId")
              p   (aget payload "position")]
          (state/set-position! nid #js [(aget p "x") (aget p "y")])
          (post-flow!)))
      (= t "cg/run") (run-flow!)
      (= t "cg/select-model") (select-model! (aget m "model"))
      ;; The native shell found the backend port. Set the URL here, because
      ;; init! read the default URL before the port existed.
      (= t "cg/backend-url") (session/set-backend-url! (aget m "url"))
      (= t "cg/backend-flapping") (state/set-backend-flapping! true)
      (= t "cg/load-file")
      (process-text! (or (aget m "name") "untitled.cg") (aget m "content"))
      ;; An empty flow renders an empty canvas.
      (= t "cg/new")
      (do (state/clear-loaded!)
          (status/reset-run!)
          (post! #js {:type "cg/flow" :flow #js {:loadId (:load-n @state/app-state)}}))
      ;; From the native shell (File > Save). save! posts cg/file-save-request
      ;; back, because the web side owns the overlay codec.
      (= t "cg/save") (save-file!)
      (= t "cg/open-defop") (js/console.log "cg/open-defop: the app shell has no editor to open an op in")
      :else nil)))

(defn- inject-icon-sprite!
  "Fetches the icon sprite into the page. Ignores a failure."
  []
  (when-not (.getElementById js/document "cg-icon-sprite")
    (-> (js/fetch "css/cg-brand/icons.svg")
        (.then (fn [^js r] (when (.-ok r) (.text r))))
        (.then (fn [svg]
                 (when svg
                   (let [div (.createElement js/document "div")]
                     (set! (.-id div) "cg-icon-sprite")
                     (.setAttribute div "style"
                                    "position:absolute;width:0;height:0;overflow:hidden;pointer-events:none;visibility:hidden")
                     (.setAttribute div "aria-hidden" "true")
                     (set! (.-innerHTML div) svg)
                     (.appendChild (.-body js/document) div)))))
        (.catch (fn [_] nil)))))

(defn ^:async init []
  ;; Set the theme before the mount, because the first render must not flash.
  (apply-theme-attrs! (state/init-theme!))
  (inject-icon-sprite!)
  (rt/init!)
  ;; A phone shows one panel. Pick it before the first render, because a
  ;; visible Code panel loads Monaco.
  (when (viewport/phone? (viewport/breakpoint (.-innerWidth js/window)))
    (state/select-panel! "flow"))
  (session/start! nil)
  ;; Add the bus listener before the canvas import, because cg/ready must not
  ;; be missed.
  (.addEventListener js/window "message" on-bus-message)
  (status/listen!)
  (.render (createRoot (js/document.getElementById "app")) #jsx [Shell])
  (.then (load-list!) state/set-examples!)
  (when-let [url (open-url/url-param (.-search js/location))]
    (open-url! url))
  (state/set-module-status! "loading")
  (-> (loader/load-cg-module!)
      (.then (fn [_] (state/set-module-status! "loaded")))
      (.catch (fn [e]
                (state/set-module-status! "error")
                (js/console.warn "cg module load failed (codec/execution degraded):" e)))))
