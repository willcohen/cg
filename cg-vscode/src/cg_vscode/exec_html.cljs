;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.exec-html
  "The HTML of the CG execution webview. Its CSP allows 'unsafe-eval' and
   'wasm-unsafe-eval' for wasmts and for the eval of the JS backend."
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as str]))

;; The importmap of the npm packages, the same as in the web app and the
;; demo. `bb importmap:write` in cg-app writes it from cg-app/importmap.mjs.
(defn- read-importmap
  "The parsed shared/ui/public/importmap.json of cg-app: imports and hosts."
  [public-dir]
  (js/JSON.parse (fs/readFileSync (path/join public-dir "importmap.json") "utf8")))

(defn- importmap-json
  "The `{imports {...}}` JSON of the page."
  [shared asset-uri parser-wasm]
  (let [imports (js/Object.assign #js {} (aget shared "imports"))]
    (when parser-wasm
      (aset imports "@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm"
            (asset-uri parser-wasm)))
    (js/JSON.stringify #js {:imports imports})))

;; The basemap hosts of cg-gui.map.core. MapLibre fetches tiles and glyphs through
;; connect-src, and the sprite through img-src.
(def ^:private basemap-hosts
  "https://openmaptiles.github.io https://tiles.openstreetmap.us")

(defn build-csp
  "The relaxed CSP of the execution webview. `csp-source` is webview.cspSource,
   and `cdn-hosts` are the hosts of the importmap."
  [csp-source nonce cdn-hosts]
  (str "default-src 'none'; "
       "script-src 'nonce-" nonce "' 'unsafe-eval' 'wasm-unsafe-eval' " csp-source " " cdn-hosts "; "
       "connect-src " csp-source " " cdn-hosts " " basemap-hosts "; "
       "img-src " csp-source " data: blob: " basemap-hosts "; "
       "style-src 'unsafe-inline' " csp-source "; "
       ;; A worker-router worker is a blob: module with a static import of its
       ;; bootstrap, and a browser checks that import against worker-src.
       "worker-src blob: " csp-source " " cdn-hosts "; "
       "font-src " csp-source " data:;"))

(defn- harness-script
  "The inline module script: it starts a map, runs cg/run and draws cg/layers.
   A run does not wait for the map, because a basemap can fail to answer."
  [nonce cg-bundle-uri cg-map-uri cg-run-uri]
  (str "<script type=\"module\" nonce=\"" nonce "\">\n"
       "const vscode = acquireVsCodeApi();\n"
       "const post = (m) => vscode.postMessage(m);\n"
       "const status = (t) => { const el = document.getElementById('cg-exec-status'); if (el) el.textContent = t; };\n"
       "document.addEventListener('securitypolicyviolation', (e) => {\n"
       "  post({ type: 'cg/csp-violation', directive: e.violatedDirective,\n"
       "    blocked: e.blockedURI || e.sourceFile || 'inline' });\n"
       "});\n"
       "let mod, rc, mc, map, mapReady, ready = false, pending = null;\n"
       ;; load-module reads a module through the host, because the page cannot
       ;; read the files of the workspace.
       "const moduleWaits = new Map();\n"
       "let moduleSeq = 0;\n"
       "function readModule(path) {\n"
       "  return new Promise((res, rej) => {\n"
       "    const id = ++moduleSeq;\n"
       "    moduleWaits.set(id, { res, rej });\n"
       "    post({ type: 'cg/read-module', id, path });\n"
       "  });\n"
       "}\n"
       "function moduleText(m) {\n"
       "  const w = moduleWaits.get(m.id);\n"
       "  if (!w) return;\n"
       "  moduleWaits.delete(m.id);\n"
       "  if (typeof m.text === 'string') w.res(m.text); else w.rej(new Error(m.error || ('load-module ' + m.path + ': no file')));\n"
       "}\n"
       "async function startMap() {\n"
       "  const opts = { container: 'cg-map', center: [-98.5, 39.8], zoom: 4 };\n"
       "  map = await Promise.race([\n"
       "    new Promise((res) => mc.ensure_map_BANG_(opts, res)),\n"
       "    new Promise((_, rej) => setTimeout(\n"
       "      () => rej(new Error('map init timed out (basemap unreachable?)')), 25000)),\n"
       "  ]);\n"
       ;; The extension answers with cg/layers from the live JVM session.
       "  mc.install_viewport_refetch_BANG_(map, (bbox) => post({ type: 'cg/viewport-settled', bbox }), 300);\n"
       "  return map;\n"
       "}\n"
       "async function boot() {\n"
       "  rc = await import('" cg-run-uri "');\n"
       "  await rc.ensure_wasmts_BANG_(null);\n"
       "  mod = await import('" cg-bundle-uri "');\n"
       "  await rc.ensure_arquero_BANG_(mod);\n"
       "  mc = await import('" cg-map-uri "');\n"
       "  mapReady = startMap();\n"
       "  mapReady.catch((e) => post({ type: 'cg/map-error', error: String((e && e.message) || e) }));\n"
       "  ready = true;\n"
       "  status('ready — waiting for a .cg run');\n"
       "  post({ type: 'cg/ready' });\n"
       "  if (pending) { const p = pending; pending = null; dispatch(p); }\n"
       "}\n"
       "async function runCg(text, flow, model, file) {\n"
       "  try {\n"
       "    status('running ' + (model || '.cg') + '...');\n"
       "    post({ type: 'cg/run-started', model: model || null });\n"
       "    const onTrace = (evt) => post({ type: 'cg/trace-event', event: evt });\n"
       "    const r = await rc.execute_js_BANG_(mod, text, flow, onTrace, readModule, model || null, file || null);\n"
       "    const layers = rc.run_result__GT_layers(mod, r);\n"
       "    const layer = layers.length ? layers[layers.length - 1] : null;\n"
       ;; The run ends before the draw, because the draw waits for the map,
       ;; and a basemap that does not answer holds it for 25 s.
       "    post({ type: 'cg/run-result', ok: true, count: r.count, mapped: !!layer,\n"
       "           layer: layer ? layer['var'] : null });\n"
       ;; The next run does not wait for this draw.
       "    if (layer) {\n"
       "      renderLayers(layers).then(() => {\n"
       "        status('run layer: cgdata-' + layer['var'] + ' (' + r.count + ' result(s))');\n"
       "        post({ type: 'cg/map-ready', online: true, layer: layer['var'], count: r.count });\n"
       "      }, (e) => {\n"
       "        status('map draw failed: ' + String((e && e.message) || e));\n"
       "        post({ type: 'cg/map-error', error: String((e && e.message) || e) });\n"
       "      });\n"
       "    } else {\n"
       "      status('run ok (' + r.count + ' result(s)) — no map layer (report/scalar/no geometry)');\n"
       "    }\n"
       "  } catch (e) {\n"
       "    status('run failed: ' + String((e && e.message) || e));\n"
       "    post({ type: 'cg/run-error', error: String((e && e.message) || e) });\n"
       "  }\n"
       "}\n"
       "async function renderLayers(layers) {\n"
       "  await mapReady;\n"
       "  return new Promise((res) => {\n"
       "    const apply = () => { mc.sync_layers_BANG_(map, layers); renderPanel(layers); window.__mapReady = true; res(); };\n"
       "    if (map.isStyleLoaded()) apply(); else map.once('idle', apply);\n"
       "  });\n"
       "}\n"
       ;; A checkbox shows the current MapLibre visibility of its layer, because
       ;; a render must keep the choice of the user. The legend reads the style
       ;; that map-core drew (layer_info): a map-layer spec and the panel cannot
       ;; disagree.
       "const varLayerIds = (v) => map.getLayersOrder().filter(\n"
       "  (id) => /^cg[a-z]+-/.test(id) && id.replace(/^cg[a-z]+-/, '') === v);\n"
       "function swatch(color) {\n"
       "  const sw = document.createElement('span');\n"
       "  sw.className = 'cg-swatch';\n"
       "  sw.style.background = color;\n"
       "  return sw;\n"
       "}\n"
       "function textEl(tag, cls, text) {\n"
       "  const el = document.createElement(tag);\n"
       "  el.className = cls;\n"
       "  el.textContent = text;\n"
       "  return el;\n"
       "}\n"
       "function renderPanel(layers) {\n"
       "  const panel = document.getElementById('cg-layers-panel');\n"
       "  if (!panel) return;\n"
       "  panel.textContent = '';\n"
       "  layers.forEach((l) => {\n"
       "    const v = l['var'];\n"
       "    const info = mc.layer_info(v);\n"
       "    const style = (info && info.style) || l.style || mc.spec__GT_style(v, l.geojson, l.spec);\n"
       "    const legend = mc.layer_legend(v, style);\n"
       "    const vis = map.getLayoutProperty('cgdata-' + v, 'visibility');\n"
       "    const row = document.createElement('div');\n"
       "    row.className = 'cg-layer-row';\n"
       "    row.setAttribute('data-var', v);\n"
       "    const label = document.createElement('label');\n"
       "    const cb = document.createElement('input');\n"
       "    cb.type = 'checkbox';\n"
       "    cb.checked = vis !== 'none';\n"
       "    cb.addEventListener('change', () => {\n"
       "      const to = cb.checked ? 'visible' : 'none';\n"
       "      varLayerIds(v).forEach((id) => map.setLayoutProperty(id, 'visibility', to));\n"
       "    });\n"
       "    label.appendChild(cb);\n"
       "    if (style.mode === 'single') label.appendChild(swatch(legend.rows[0]['swatch-color']));\n"
       "    label.appendChild(textEl('span', 'cg-layer-name', v));\n"
       "    row.appendChild(label);\n"
       ;; The row limit truncated the fetch, and map-core shows a truncated
       ;; layer only from street zoom.
       "    const tot = l['total-rows'], ret = l['returned-rows'];\n"
       "    if (typeof tot === 'number' && typeof ret === 'number' && ret < tot) {\n"
       "      row.appendChild(textEl('div', 'cg-layer-count',\n"
       "        'showing ' + ret + ' of ' + tot + ' rows — zoom to street level for all'));\n"
       "    }\n"
       "    if (style.mode === 'categorical') {\n"
       "      const box = document.createElement('div');\n"
       "      box.className = 'cg-legend';\n"
       "      if (legend.title) box.appendChild(textEl('div', 'cg-legend-title', legend.title));\n"
       "      legend.rows.forEach((r) => {\n"
       "        const item = document.createElement('div');\n"
       "        item.className = 'cg-legend-item';\n"
       "        item.appendChild(swatch(r['swatch-color']));\n"
       "        item.appendChild(textEl('span', 'cg-legend-label', r['label']));\n"
       "        if (typeof r['count'] === 'number') {\n"
       "          item.appendChild(textEl('span', 'cg-legend-count', r['count'].toLocaleString('en-US')));\n"
       "        }\n"
       "        box.appendChild(item);\n"
       "      });\n"
       "      row.appendChild(box);\n"
       "    }\n"
       "    panel.appendChild(row);\n"
       "  });\n"
       "}\n"
       "async function renderLayerMsg(layers) {\n"
       "  try {\n"
       "    status('rendering ' + layers.length + ' layer(s)...');\n"
       "    await renderLayers(layers);\n"
       "    const vars = layers.map((l) => l['var']);\n"
       "    status('layers: ' + vars.map((v) => 'cgdata-' + v).join(', '));\n"
       "    post({ type: 'cg/layers-rendered', count: layers.length, vars });\n"
       "  } catch (e) {\n"
       "    status('layer render failed: ' + String((e && e.message) || e));\n"
       "    post({ type: 'cg/map-error', error: String((e && e.message) || e) });\n"
       "  }\n"
       "}\n"
       ;; One run at a time, because the trace listeners of cg see each run
       ;; that is on, and the canvas must get the messages of one run.
       "let runs = Promise.resolve();\n"
       "function dispatch(m) {\n"
       "  if (m.type === 'cg/run') runs = runs.then(() => runCg(m.text, m.flow, m.model, m.file));\n"
       "  else if (m.type === 'cg/layers') renderLayerMsg(m.layers);\n"
       "}\n"
       "window.addEventListener('message', (e) => {\n"
       "  const m = e.data;\n"
       "  if (!m) return;\n"
       "  if (m.type === 'cg/run' || m.type === 'cg/layers') { if (ready) dispatch(m); else pending = m; }\n"
       "  else if (m.type === 'cg/status') status(m.text);\n"
       "  else if (m.type === 'cg/module-text') moduleText(m);\n"
       "});\n"
       "boot().catch((e) => post({ type: 'cg/map-error', error: String((e && e.message) || e) }));\n"
       "</script>"))

(defn build-html
  "The page of the execution webview. `asset-uri` maps an absolute path to a
   URL, and `parser-wasm` is optional."
  [{:keys [asset-uri csp-source nonce public-dir wasmts-dir parser-wasm]}]
  (let [cg-bundle-uri (asset-uri (path/join public-dir "cg-bundled-browser.mjs"))
        cg-map-uri    (asset-uri (path/join public-dir "cg-map.mjs"))
        cg-run-uri    (asset-uri (path/join public-dir "cg-run.mjs"))
        wasmts-js-uri (asset-uri (path/join wasmts-dir "wasmts.js"))
        shared        (read-importmap public-dir)
        importmap     (importmap-json shared asset-uri parser-wasm)
        csp           (build-csp csp-source nonce (str/join " " (aget shared "hosts")))]
    (str "<!DOCTYPE html>\n"
         "<html lang=\"en\">\n"
         "<head>\n"
         "  <meta charset=\"UTF-8\">\n"
         "  <meta http-equiv=\"Content-Security-Policy\" content=\"" csp "\">\n"
         "  <meta name=\"viewport\" content=\"width=device-width,initial-scale=1.0\">\n"
         "  <script type=\"importmap\" nonce=\"" nonce "\">" importmap "</script>\n"
         "  <style nonce=\"" nonce "\">\n"
         "    html, body { margin: 0; height: 100%; font-family: var(--vscode-font-family);\n"
         "      color: var(--vscode-foreground); background: var(--vscode-editor-background); }\n"
         "    #cg-exec-status { position: absolute; top: 0; left: 0; right: 0; height: 22px;\n"
         "      line-height: 22px; padding: 0 8px; font-size: 12px; z-index: 1;\n"
         "      background: var(--vscode-editor-background); opacity: 0.85; }\n"
         "    #cg-map { position: absolute; top: 22px; left: 0; right: 0; bottom: 0; }\n"
         ;; On the left: map-core puts the identify panel at the top right.
         "    #cg-layers-panel { position: absolute; top: 30px; left: 8px; z-index: 2;\n"
         "      max-height: calc(100% - 40px); overflow-y: auto; min-width: 130px;\n"
         "      padding: 6px 8px; font-size: 12px; border-radius: 4px;\n"
         "      background: var(--vscode-editor-background); opacity: 0.92;\n"
         "      border: 1px solid var(--vscode-panel-border); }\n"
         "    #cg-layers-panel:empty { display: none; }\n"
         "    .cg-layer-row { padding: 2px 0; }\n"
         "    .cg-layer-row label { display: flex; align-items: center; gap: 6px; cursor: pointer; }\n"
         "    .cg-legend { margin: 2px 0 4px 20px; }\n"
         "    .cg-legend-item { display: flex; align-items: center; gap: 6px; padding: 1px 0; }\n"
         "    .cg-legend-title, .cg-layer-count { opacity: 0.75; font-size: 11px; }\n"
         "    .cg-layer-count { margin-left: 20px; }\n"
         "    .cg-legend-count { margin-left: auto; padding-left: 12px; opacity: 0.75;\n"
         "      font-variant-numeric: tabular-nums; }\n"
         "    .cg-swatch { display: inline-block; width: 12px; height: 12px; flex: none;\n"
         "      border-radius: 2px; border: 1px solid rgba(128,128,128,0.4); }\n"
         "  </style>\n"
         "</head>\n"
         "<body>\n"
         "  <div id=\"cg-exec-status\">Starting wasmts and the cg bundle...</div>\n"
         "  <div id=\"cg-map\"></div>\n"
         "  <div id=\"cg-layers-panel\"></div>\n"
         "  <script nonce=\"" nonce "\" src=\"" wasmts-js-uri "\"></script>\n"
         "  " (harness-script nonce cg-bundle-uri cg-map-uri cg-run-uri) "\n"
         "</body>\n"
         "</html>")))
