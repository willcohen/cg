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

(defn cg-text
  "The .cg text for `text` and `flow` with `positions` in the ^:cgproj overlay.
   Appends the overlay when the source has none."
  [text flow positions]
  (overlay/update-visual-metadata text flow positions))

(defn save-request
  "The cg/file-save-request message the native side consumes."
  [name text flow positions]
  #js {:type "cg/file-save-request"
       :name name
       :content (cg-text text flow positions)})

(defn request-for
  "The save request for the loaded file. Text that does not parse goes as
   typed, because the overlay lines of `flow` are those of an older text."
  [name loaded-file flow positions]
  (if (:unparsed loaded-file)
    #js {:type "cg/file-save-request" :name name :content (:text loaded-file)}
    (save-request name (:text loaded-file) flow positions)))

(defn- native-post!
  "Posts `msg` to native as a JSON string (RN WebView onMessage). Returns true
   when the channel exists."
  [msg]
  (when-let [rn (aget js/globalThis "ReactNativeWebView")]
    (.postMessage rn (js/JSON.stringify msg))
    true))

(defn- browser-download!
  [name content]
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
