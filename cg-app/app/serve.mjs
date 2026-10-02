// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Dev server for the app shell (bb dev:app). resolve() maps each page URL to
// its file in the monorepo.
import { createServer } from 'node:http';
import { readFile, stat } from 'node:fs/promises';
import { join, dirname, extname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { imports as cdnImports, pageUrls } from '../importmap.mjs';

const APP = dirname(fileURLToPath(import.meta.url));
const ROOT = join(APP, '..', '..');            // monorepo root
const PUB = join(APP, 'public');
const SHARED_PUB = join(APP, '..', 'shared', 'ui', 'public');

const MIME = { '.html': 'text/html', '.mjs': 'text/javascript', '.js': 'text/javascript',
  '.css': 'text/css', '.wasm': 'application/wasm', '.svg': 'image/svg+xml',
  '.json': 'application/json', '.map': 'application/json', '.cg': 'text/plain',
  '.ttf': 'font/ttf', '.woff': 'font/woff', '.woff2': 'font/woff2' };

function resolve(url) {
  const p = url.split('?')[0];
  if (p === '/' || p === '/index.html') return join(PUB, 'index.html');
  if (p.startsWith('/dist/')) return join(APP, p);
  if (p.startsWith('/canvas/')) return join(APP, '..', 'shared', 'squint', 'dist', p.slice('/canvas/'.length));
  // The app shell fetches the files of run/load-module here. Their paths are
  // relative to the cg core root.
  if (p.startsWith('/modules/') && !p.includes('..'))
    return join(ROOT, 'cg', p.slice('/modules/'.length));
  if (p.startsWith('/examples/') && !p.includes('..'))
    return join(ROOT, 'cg', p.slice(1));
  if (p.startsWith('/css/')) return join(SHARED_PUB, p);
  if (p === '/cg-bundled-browser.mjs') return join(SHARED_PUB, p);
  // cg.run finds the WasmTS worker handler next to the bundle that holds it.
  if (p === '/cg-wasmts-handler.mjs') return join(ROOT, 'cg', 'dist', p);
  return join(PUB, p);
}

// Warn when dist/cg-app.mjs is older than one of its inputs. `node serve.mjs`
// does not rebuild it, and a stale bundle reports false unresolved-op
// diagnostics for built-in ops. `bb bundle:check` does the same check.
(async () => {
  try {
    const meta = JSON.parse(await readFile(join(APP, 'dist', 'cg-app.meta.json'), 'utf8'));
    const built = (await stat(join(APP, 'dist', 'cg-app.mjs'))).mtimeMs;
    const stale = [];
    for (const p of Object.keys(meta.inputs || {})) {
      try {
        if ((await stat(join(APP, p))).mtimeMs > built) stale.push(p);
      } catch { stale.push(p + ' (missing)'); }
    }
    if (stale.length) {
      console.warn(`WARNING: dist/cg-app.mjs is older than ${stale.length} of its inputs, e.g. ${stale[0]}`);
      console.warn('         the served shell embeds a stale copy — rebuild: bb bundle:app');
    }
  } catch { /* No bundle yet. The 404 on /dist/ reports it. */ }
})();

// The backend writes its port to ../.cg-backend-port. A browser page cannot
// read that file. The dev server reads it, checks that the backend answers,
// and injects window.__CG_BACKEND_URL__ into index.html. The macOS app does
// the same. init! uses this URL before a saved one. Discovery occurs at page
// load: start the backend, then reload the page.
async function discoverBackendUrl() {
  // The probes need the js runtime. A local backend must not move their runs
  // to the JVM.
  if (process.env.CG_BACKEND_AUTODETECT === '0') return null;
  try {
    const port = (await readFile(join(APP, '..', '.cg-backend-port'), 'utf8')).trim();
    if (!/^\d+$/.test(port)) return null;
    const url = `http://127.0.0.1:${port}`;
    const ctl = new AbortController();
    const t = setTimeout(() => ctl.abort(), 400);
    const resp = await fetch(url + '/api/health', { signal: ctl.signal });
    clearTimeout(t);
    return resp.ok ? url : null;
  } catch { return null; }
}

const port = Number(process.env.PORT || 3100);
// The npm packages load from the CDN, as in the demo: the page gets the
// entries of cg-app/importmap.mjs and the CDN URLs of its style sheets and
// of wasmts.
function withCdn(html) {
  for (const [from, to] of Object.entries(pageUrls)) html = html.replace(`"${from}"`, `"${to}"`);
  const at = html.indexOf('{ "imports": {') + '{ "imports": {'.length;
  const extra = Object.entries(cdnImports).map(([k, v]) => `\n    ${JSON.stringify(k)}: ${JSON.stringify(v)},`).join('');
  return html.slice(0, at) + extra + html.slice(at);
}

createServer(async (req, res) => {
  try {
    const f = resolve(req.url);
    let body = await readFile(f);
    if (f.endsWith('index.html')) {
      body = withCdn(body.toString());
      const backend = await discoverBackendUrl();
      // init! must see it. An inline script before the importmap is valid,
      // because only module loads must come after the importmap.
      if (backend) body = body.toString().replace('<script',
        `<script>window.__CG_BACKEND_URL__ = ${JSON.stringify(backend)};</script>\n<script`);
    }
    res.writeHead(200, { 'content-type': MIME[extname(f)] || 'application/octet-stream',
                         'cache-control': 'no-store' });
    res.end(body);
  } catch {
    res.writeHead(404); res.end('not found: ' + req.url);
  }
}).listen(port, () => console.log(`cg-app dev server: http://localhost:${port}/`));
