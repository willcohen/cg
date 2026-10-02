;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.canvas-html
  "The HTML of the canvas webview of the flow editor. No vscode dependency: a
   test and a headless browser can load the page."
  (:require [clojure.string :as str]))

(defn theme-kind->mode
  "Maps a VS Code ColorThemeKind to a cg-brand mode key. 1 = Light, 2 = Dark,
   3 = HighContrast (dark), 4 = HighContrastLight."
  [kind]
  (if (or (= kind 1) (= kind 4)) "light" "dark"))

(defn- mix
  "A CSS color between the editor background and the foreground: `pct` percent
   background."
  [pct]
  (str "color-mix(in srgb, var(--vscode-editor-background) " pct
       "%, var(--vscode-foreground))"))

;; editorWidget.background and panel.border are equal to the editor background
;; in the default dark theme. A surface or a line is a mix, which gives a step
;; in each theme.
(def theme-tokens
  "The --cg-* tokens of the canvas as VS Code theme colors. They hold for each
   theme, because VS Code changes its variables with the theme."
  #js {"surface"        "var(--vscode-editor-background)"
       "surface-raised" (mix 93)
       "surface-sunk"   (mix 86)
       "ink"            "var(--vscode-foreground)"
       "text"           "var(--vscode-foreground)"
       "ink-muted"      "var(--vscode-descriptionForeground)"
       "line"           (mix 76)
       "line-soft"      (mix 86)
       "accent"         "var(--vscode-textLink-foreground)"
       "run"            "var(--vscode-editorWarning-foreground)"
       "complete"       "var(--vscode-testing-iconPassed)"
       "error"          "var(--vscode-errorForeground)"
       "font-mono"      "var(--vscode-editor-font-family)"})

(defn- emit-token-css
  "Render a JS map of {key value} into `    --<prefix><key>: <value>;` lines."
  [prefix ^js js-obj]
  (->> (js/Object.entries js-obj)
       (map (fn [pair]
              (str "    --" prefix (aget pair 0) ": " (aget pair 1) ";")))
       (str/join "\n")))

(defn sprite-block
  "An inline sprite of icons.svg, for `<use href=\"#i-X\">` with no fetch. It
   has no size or pointer events: it must not move #root or take clicks."
  [svg]
  (if svg
    (str "<div id=\"cg-icon-sprite\""
         " style=\"position:absolute;width:0;height:0;overflow:hidden;"
         "pointer-events:none;visibility:hidden\""
         " aria-hidden=\"true\">"
         svg
         "</div>")
    ""))

(def carbon-components
  "The Carbon components that webview-css styles. A component that is not in
   this list shows with the browser defaults."
  #{"Search" "Button" "Tag" "SelectableTag"})

;; cg.css falls back to VS Code theme colors. Some of them give a card no edge,
;; and diffEditor.insertedLineBackground and charts.orange are translucent.
;; The .cds-- rules replace the Carbon style sheet.
(def ^:private webview-css
  (str "    .react-flow {
      --canvas-surface: var(--cg-surface-raised);
      --canvas-surface-2: " (mix 97) ";
      --canvas-surface-sunk: var(--cg-surface-sunk);
      --canvas-hover: " (mix 89) ";
      --canvas-line: var(--cg-line);
      --canvas-line-strong: " (mix 55) ";
      --canvas-complete-bg: color-mix(in srgb, var(--vscode-editor-background) 82%, var(--vscode-testing-iconPassed, #73c991));
      --canvas-mode-table: var(--vscode-terminal-ansiCyan, #11a8cd);
      --canvas-mode-map: var(--vscode-terminal-ansiMagenta, #bc3fbc);
      --xy-controls-button-background-color: var(--canvas-surface);
      --xy-controls-button-background-color-hover: var(--canvas-hover);
      --xy-controls-button-color: var(--canvas-ink);
      --xy-controls-button-color-hover: var(--canvas-ink);
      --xy-controls-button-border-color: var(--canvas-line);
      --xy-minimap-background-color: var(--canvas-surface-2);
    }
    .cg-search .cds--search { position: relative; flex: 1; min-width: 0; display: flex; align-items: center; }
    .cg-search .cds--label {
      position: absolute; width: 1px; height: 1px; overflow: hidden;
      clip: rect(0 0 0 0); white-space: nowrap;
    }
    .cg-search .cds--search-magnifier {
      position: absolute; left: 6px; top: 0; bottom: 0;
      display: flex; align-items: center; pointer-events: none;
      color: var(--canvas-ink-muted);
    }
    .cg-search .cds--search-input {
      width: 100%; height: 26px; box-sizing: border-box; padding: 0 26px;
      font: 12px/16px var(--vscode-font-family);
      color: var(--canvas-ink); background: var(--canvas-surface);
      border: 1px solid var(--canvas-line);
      border-radius: 2px; outline: none; appearance: none;
    }
    .cg-search .cds--search-input:focus { border-color: var(--vscode-focusBorder); }
    .cg-search .cds--search-input::placeholder { color: var(--canvas-ink-muted); }
    .cg-search .cds--search-input::-webkit-search-cancel-button { display: none; }
    .cg-search .cds--search-close {
      position: absolute; right: 0; top: 0; width: 26px; height: 26px; padding: 0;
      display: flex; align-items: center; justify-content: center;
      color: var(--canvas-ink); background: transparent; border: 0; cursor: pointer;
    }
    .cg-search .cds--search-close--hidden { visibility: hidden; }
    .cg-toolbar .cds--btn {
      display: inline-flex; align-items: center; gap: 6px; height: 26px; padding: 0 10px;
      font: 12px/16px var(--vscode-font-family);
      color: var(--canvas-ink); background: var(--canvas-surface);
      border: 1px solid var(--canvas-line); border-radius: 2px; cursor: pointer;
    }
    .cg-toolbar .cds--btn:hover { background: var(--canvas-hover); }
    .cg-toolbar .cds--btn:focus-visible, .cg-toolbar .cds--tag:focus-visible {
      outline: 1px solid var(--vscode-focusBorder); outline-offset: 1px;
    }
    .cg-toolbar .cds--tag {
      display: inline-flex; align-items: center; height: 18px; padding: 0 8px;
      box-sizing: border-box; border: 0; border-radius: 9px;
      font: 12px/16px var(--vscode-font-family); white-space: nowrap;
      color: var(--canvas-ink); background: var(--canvas-surface-sunk);
    }
    .cg-toolbar .cds--tag--selectable {
      cursor: pointer; background: transparent; border: 1px solid var(--canvas-line);
    }
    .cg-toolbar .cds--tag--selectable-selected {
      color: var(--canvas-on-color); background: var(--canvas-button); border-color: var(--canvas-button);
    }
"))

(defn build-html
  "The page of the canvas webview. `asset-uri` maps a path under dist/webview
   to a URL, and `sprite` is the text of icons.svg or nil."
  [{:keys [asset-uri csp-source nonce mode sprite]}]
  (let [;; ?v=nonce changes the URL on each load, because the webview service
        ;; worker caches resources by URL and serves a stale cg-canvas.mjs.
        script-uri   (str (asset-uri "cg-canvas.mjs") "?v=" nonce)
        link         (fn [href]
                       (str "<link rel=\"stylesheet\" nonce=\"" nonce "\" href=\"" href "\">"))
        ;; style-src has no nonce. React Flow sets inline styles on each node,
        ;; and Chromium ignores 'unsafe-inline' when a nonce is present.
        style-src    (str "'unsafe-inline' " csp-source)
        ;; The canvas writes the block of the mode again on cg/theme-kind.
        tokens       (js/JSON.stringify #js {:light theme-tokens :dark theme-tokens})]
    (str "<!DOCTYPE html>
<html lang=\"en\" data-theme=\"" mode "\">
<head>
  <meta charset=\"UTF-8\">
  <meta http-equiv=\"Content-Security-Policy\"
    content=\"default-src 'none'; script-src 'nonce-" nonce "' " csp-source "; style-src " style-src "; font-src " csp-source " data:; worker-src blob:;\">
  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1.0\">
  " (link (asset-uri "react-flow.css")) "
  " (link (str (asset-uri "cg.css") "?v=" nonce)) "
  <style nonce=\"" nonce "\" id=\"cg-tokens\">
:root {
" (emit-token-css "cg-" theme-tokens) "
}
  </style>
  <style nonce=\"" nonce "\">
    html, body, #root {
      width: 100%; height: 100%; margin: 0; padding: 0; overflow: hidden;
      font-family: var(--vscode-font-family);
      color: var(--vscode-foreground);
      background: var(--vscode-editor-background);
    }
    .react-flow__node { font-size: 12px; }
" webview-css "    /* The pulse module of the canvas writes --cg-pulse-opacity on <html>
       each frame, so that the dots of all running cards blink together. It
       holds the value at 1 when more than 12 cards run. */
    :root.cg-pulse-locked .cg-node.running .cg-stat span {
      animation: none;
      opacity: var(--cg-pulse-opacity, 1);
    }
  </style>
  <script nonce=\"" nonce "\">
    window.__CG_TOKENS__ = " tokens ";
  </script>
</head>
<body>
  " (sprite-block sprite) "
  <div id=\"root\"></div>
  <script type=\"module\" nonce=\"" nonce "\">
    import { mount } from \"" script-uri "\";
    mount(\"root\");
  </script>
</body>
</html>")))
