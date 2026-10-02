// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Write a self-contained static directory (app/dist-static/). The same
// index.html serves it over https (web PWA) and file:// (native WKWebView).
// Each file that serve.mjs routes is copied to a relative path, and each
// root-absolute URL in index.html becomes ./... . Keep the copies in sync
// with resolve() in serve.mjs.
import * as esbuild from 'esbuild';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import {
  mkdirSync, rmSync, cpSync, copyFileSync, readFileSync, writeFileSync, readdirSync,
} from 'node:fs';

const here = dirname(fileURLToPath(import.meta.url)); // cg-app/app
const cgApp = join(here, '..');
const repo = join(cgApp, '..');                       // monorepo root
const out = join(here, 'dist-static');

rmSync(out, { recursive: true, force: true });
mkdirSync(join(out, 'dist'), { recursive: true });

// Keep this config the same as in esbuild.app.mjs.
await esbuild.build({
  entryPoints: [join(here, 'js', 'cg_app', 'ui', 'app.jsx')],
  bundle: true,
  format: 'esm',
  platform: 'browser',
  external: ['@wcohen/cg', 'cg-app/canvas', 'fs/promises', 'module',
             'maplibre-gl', 'backproj', 'maplibre-proj', 'proj-wasm'],
  jsx: 'automatic',
  jsxImportSource: 'react',
  target: ['es2022'],
  sourcemap: true,
  minify: false,
  minifyWhitespace: true,
  define: { 'process.env.NODE_ENV': '"production"' },
  outfile: join(out, 'dist', 'cg-app.mjs'),
});
copyFileSync(join(repo, 'cg', 'node_modules', 'web-tree-sitter', 'web-tree-sitter.wasm'),
             join(out, 'dist', 'web-tree-sitter.wasm'));

const cp = (src, dst) => cpSync(src, join(out, dst), { recursive: true });
cp(join(cgApp, 'shared', 'squint', 'dist'), 'canvas');
cp(join(cgApp, 'shared', 'ui', 'public', 'vendor'), 'vendor');
cp(join(cgApp, 'shared', 'ui', 'public', 'css'), 'css');
copyFileSync(join(cgApp, 'shared', 'ui', 'public', 'cg-bundled-browser.mjs'),
             join(out, 'cg-bundled-browser.mjs'));
copyFileSync(join(cgApp, 'node_modules', '@xyflow', 'react', 'dist', 'style.css'),
             join(out, 'vendor', 'react-flow.css'));
copyFileSync(join(repo, 'cg', 'node_modules', '@yogthos', 'tree-sitter-clojure', 'tree-sitter-clojure.wasm'),
             join(out, 'vendor', 'tree-sitter-clojure.wasm'));
cp(join(repo, 'cg', 'node_modules', '@wcohen', 'wasmts', 'dist'), join('vendor', 'wasmts'));
cp(join(cgApp, 'node_modules', 'monaco-editor', 'min'), join('vendor', 'monaco'));
copyFileSync(join(cgApp, 'node_modules', 'maplibre-gl', 'dist', 'maplibre-gl.css'),
             join(out, 'vendor', 'maplibre-gl.css'));
// The Examples menu reads index.json. A file it leaves out, such as boston.cg,
// still opens by ?url=examples/<file>.
mkdirSync(join(out, 'examples'), { recursive: true });
const examples = join(repo, 'cg', 'examples');
for (const f of readdirSync(examples).filter((n) => n.endsWith('.cg') || n === 'index.json')) {
  copyFileSync(join(examples, f), join(out, 'examples', f));
}
// The PWA serves these files from the root.
for (const f of ['manifest.webmanifest', 'sw.js', 'icon.svg']) {
  copyFileSync(join(here, 'public', f), join(out, f));
}

// Make each root-absolute URL relative: href and src attributes, the module
// import, and the importmap values.
let html = readFileSync(join(here, 'public', 'index.html'), 'utf8');
// The offline PWA and the native shell have no CDN: the importmap names the
// copies of `bb vendor-deps`, where the other pages name the CDN.
const vendored = {
  'monaco-editor/min/': '/vendor/monaco/',
  '@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm': '/vendor/tree-sitter-clojure.wasm',
  'squint-cljs/': '/vendor/cdn/squint-cljs/',
  'ffi-wasm': '/vendor/ffi-wasm/ffi-wasm.mjs',
  'worker-router': '/vendor/worker-router/index.mjs',
  'worker-router/worker-bootstrap': '/vendor/worker-router/worker-bootstrap.mjs',
  'comlink': '/vendor/comlink.mjs',
  'resource-tracker': '/vendor/resource-tracker/resource.mjs',
  'web-tree-sitter': '/vendor/web-tree-sitter/web-tree-sitter.js',
  'proj-wasm': '/vendor/proj-wasm/proj.mjs',
  'maplibre-gl': '/vendor/cdn/maplibre-gl.mjs',
  'backproj': '/vendor/cdn/backproj.mjs',
  'maplibre-proj': '/vendor/cdn/maplibre-proj.mjs',
  'lru-cache': '/vendor/cdn/lru-cache.mjs',
  'pbf': '/vendor/cdn/pbf.mjs',
  '@mapbox/vector-tile': '/vendor/cdn/vector-tile.mjs',
  'vt-pbf': '/vendor/cdn/vt-pbf.mjs',
  'gdal-wasm': '/vendor/gdal-wasm/gdal.mjs',
  'gdal-wasm/fndefs': '/vendor/gdal-wasm/fndefs.mjs',
  'gdal-wasm/gdal-handler': '/vendor/gdal-wasm/gdal-handler.mjs',
  'gdal-wasm/proj.db': '/vendor/gdal-wasm/proj.db',
};
for (const sub of ['dispatch', 'handler-env', 'handler-paths', 'macros',
                   'platform-state', 'pool', 'workload-pool']) {
  vendored['ffi-wasm/' + sub] = '/vendor/ffi-wasm/ffi-wasm.mjs';
}
const mapAt = html.indexOf('{ "imports": {') + '{ "imports": {'.length;
html = html.slice(0, mapAt)
  + Object.entries(vendored).map(([k, v]) => `\n    ${JSON.stringify(k)}: ${JSON.stringify(v)},`).join('')
  + html.slice(mapAt);
for (const base of ['vendor', 'css', 'canvas', 'dist']) {
  html = html.replaceAll(`"/${base}`, `"./${base}`);
}
html = html.replaceAll('"/cg-bundled-browser.mjs', '"./cg-bundled-browser.mjs');
writeFileSync(join(out, 'index.html'), html);

console.log('Built static bundle at', out);
