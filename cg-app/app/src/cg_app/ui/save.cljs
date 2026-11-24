;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.save
  "Saves the .cg text with an overlay from the live positions. The native
   shell shows the save dialog; a plain browser gets a download."
  (:require [cg-app.ui.state :as state]
            ["../../../../shared/squint/src/cg_gui/doc/overlay.mjs" :as overlay]))

(defn request-for
  "The cg/file-save-request message for the loaded file, with `positions` in
   the ^:cgproj overlay. Text that does not parse goes as typed, because the
   overlay lines of `flow` are those of an older text."
  [name loaded-file flow positions]
  #js {:type "cg/file-save-request"
       :name name
       :content (if (:unparsed loaded-file)
                  (:text loaded-file)
                  (overlay/update-visual-metadata (:text loaded-file) flow positions))})

(defn- native-post!
  "Posts `msg` to native as a JSON string (RN WebView onMessage). Returns true
   when the channel exists."
  [msg]
  (when-let [rn (aget js/globalThis "ReactNativeWebView")]
    (.postMessage rn (js/JSON.stringify msg))
    true))

(defn- browser-download! [name content]
  (let [blob (js/Blob. #js [content] #js {:type "text/plain"})
        url  (js/URL.createObjectURL blob)
        a    (.createElement js/document "a")]
    (set! (.-href a) url)
    (set! (.-download a) name)
    (.click a)
    (js/URL.revokeObjectURL url)))

(defn save!
  "Save the loaded file. No-op when nothing is loaded."
  []
  (let [{:keys [loaded-file flow]} @state/app-state]
    (when (and loaded-file flow)
      (let [nm  (or (:name loaded-file) "untitled.cg")
            req (request-for nm loaded-file flow (state/positions))]
        (or (native-post! req)
            (browser-download! nm (aget req "content")))))))
