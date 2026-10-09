;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.loader
  "Loads the cg module, and gives the URLs and text of the files that the page
   loads. Do not console.log a module object, because JavaScriptCore in the
   macOS app throws on it.")

(defonce cg-module (atom nil))

(defn ^:async load-cg-module!
  "Resolves to the cg module. Imports it on the first call."
  []
  (or @cg-module (reset! cg-module (await (js* "import(~{})" "@wcohen/cg")))))

(defn asset-url
  "The URL of a file in an npm package. The page's importmap decides where
   the package is: the dev server, a static copy or a CDN."
  [specifier]
  (.resolve js/import.meta specifier))

(defn ^:async fetch-module-text
  "The text of the .cg module at `path`. The path resolves against the cg
   root, as the JVM does. The URL is relative to the page, because the demo
   page is under a sub-path."
  [path]
  (let [resp (await (js/fetch (str "modules/" path)))]
    (when-not (.-ok resp)
      (throw (js/Error. (str "load-module " path ": HTTP " (.-status resp)))))
    (await (.text resp))))
