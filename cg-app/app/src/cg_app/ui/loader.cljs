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
