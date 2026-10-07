;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.exec-html
  "The HTML of the CG Map webview. cg-gui.map.page is the code of the page.
   The CSP allows 'unsafe-eval' and 'wasm-unsafe-eval' for wasmts and for the
   eval of the JS backend."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]
            [cg-vscode.canvas-html :as canvas-html]))

;; `bb importmap:write` in cg-app writes importmap.json from cg-app/importmap.mjs.
(defn- read-importmap [public-dir]
  (js/JSON.parse (fs/readFileSync (path/join public-dir "importmap.json") "utf8")))

(defn- importmap-json [shared asset-uri parser-wasm]
  (let [imports (js/Object.assign #js {} (aget shared "imports"))]
    (aset imports "@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm"
          (asset-uri parser-wasm))
    (js/JSON.stringify #js {:imports imports})))

;; The basemap hosts of cg-gui.map.core. MapLibre fetches tiles and glyphs through
;; connect-src, and the sprite through img-src.
(def ^:private basemap-hosts
  "https://openmaptiles.github.io https://tiles.openstreetmap.us")

(defn build-csp
  "`csp-source` is the origin of the asset server, and `cdn-hosts` are the
   hosts of the importmap."
  [csp-source nonce cdn-hosts]
  (str "default-src 'none'; "
       "script-src 'nonce-" nonce "' 'unsafe-eval' 'wasm-unsafe-eval' " csp-source " " cdn-hosts "; "
       ;; https: lets a run fetch the data sources that a .cg names.
       "connect-src " csp-source " " cdn-hosts " " basemap-hosts " https:; "
       "img-src " csp-source " data: blob: " basemap-hosts "; "
       "style-src 'unsafe-inline' " csp-source "; "
       ;; A worker-router worker is a blob: module with a static import of its
       ;; bootstrap, and a browser checks that import against worker-src.
       "worker-src blob: " csp-source " " cdn-hosts "; "
       "font-src " csp-source " data:;"))

(defn build-html
  "`asset-uri` maps an absolute path to a URL."
  [{:keys [asset-uri csp-source nonce public-dir wasmts-dir parser-wasm]}]
  (let [public    (fn [f] (asset-uri (path/join public-dir f)))
        wasmts-js (asset-uri (path/join wasmts-dir "wasmts.js"))
        shared    (read-importmap public-dir)]
    (str "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta http-equiv=\"Content-Security-Policy\" content=\"" (build-csp csp-source nonce (str/join " " (aget shared "hosts"))) "\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1.0\">
  <script type=\"importmap\" nonce=\"" nonce "\">" (importmap-json shared asset-uri parser-wasm) "</script>
  <link rel=\"stylesheet\" href=\"" (public "css/cg-shell.css") "\">
  <style nonce=\"" nonce "\">
:root {
" (canvas-html/emit-token-css "cg-" canvas-html/theme-tokens) "
}
    html, body { margin: 0; height: 100%; font-family: var(--vscode-font-family);
      color: var(--vscode-foreground); background: var(--vscode-editor-background); }
    #cg-exec-status { position: absolute; top: 0; left: 0; right: 0; height: 22px;
      line-height: 22px; padding: 0 8px; font-size: 12px; z-index: 1;
      background: var(--vscode-editor-background); opacity: 0.85; }
    #cg-map { position: absolute; top: 22px; left: 0; right: 0; bottom: 0; }
  </style>
</head>
<body>
  <div id=\"cg-exec-status\">Starting wasmts and the cg bundle...</div>
  <div id=\"cg-map\"></div>
  <div id=\"cg-map-legend\" class=\"cg-map-legend\" hidden></div>
  <script nonce=\"" nonce "\" src=\"" wasmts-js "\"></script>
  <script type=\"module\" nonce=\"" nonce "\">
    import { start_BANG_ } from \"" (public "cg-map-page.mjs") "\";
    const vscode = acquireVsCodeApi();
    start_BANG_({
      post: (m) => vscode.postMessage(m),
      cgBundle: \"" (public "cg-bundled-browser.mjs") "\",
      urls: { \"wasmts-js\": \"" wasmts-js "\", \"proj-db\": import.meta.resolve(\"gdal-wasm/proj.db\") },
    });
  </script>
</body>
</html>")))
