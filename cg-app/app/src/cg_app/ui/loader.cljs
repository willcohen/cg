;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-app.ui.loader
  "Loads the cg module and decodes the overlay. Do not console.log a module
   object, because JavaScriptCore in the macOS app throws on it.")

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

(defn metadata-mod
  "The metadata namespace off a loaded cg module (nil-safe)."
  [mod]
  (when mod (aget mod "metadata")))

(defn overlay-positions
  "^:cgproj overlay -> #js {bare-id -> [x y]}. Returns an empty object when
   there is no overlay or no :nodes."
  [md-mod text]
  (let [split ((aget md-mod "split_code_and_metadata") text)
        md    (aget split "metadata")
        nodes (and md (aget md "nodes"))
        out   #js {}]
    (when nodes
      (.forEach (js/Object.entries nodes)
                (fn [pair]
                  (let [props (aget pair 1)
                        p     (and props (aget props "pos"))]
                    (when p (aset out (aget pair 0) p))))))
    out))
