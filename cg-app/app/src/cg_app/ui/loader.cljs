;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.loader
  "Loads the cg module. Do not console.log a module object, because
   JavaScriptCore in the macOS app throws on it.")

(defonce cg-module (atom nil))

(defn ^:async load-cg-module!
  "Resolves to the cg module. Imports it on the first call. Derefs after
   reset!, because squint reset! returns undefined."
  []
  (when (nil? @cg-module)
    (let [m (await (js* "import(~{})" "@wcohen/cg"))]
      (reset! cg-module m)))
  @cg-module)

(defn asset-url
  "The URL of a file in an npm package. The page's importmap decides where
   the package is: the dev server, a static copy or a CDN."
  [specifier]
  (.resolve js/import.meta specifier))

(defn module-url
  "The page-relative URL of the module at load path `path`. Relative, because
   the demo is served under a sub-path."
  [path]
  (str "modules/" path))

(defn ^:async fetch-module-text
  "The text of the .cg module at `path`, from module-url. The path resolves
   against the cg root, as the JVM does."
  [path]
  (let [resp (await (js/fetch (module-url path)))]
    (when-not (.-ok resp)
      (throw (js/Error. (str "load-module " path ": HTTP " (.-status resp)))))
    (await (.text resp))))
