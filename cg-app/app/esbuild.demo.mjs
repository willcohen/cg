// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Build the GitHub Pages demo into docs/ at the repo root. One local bundle
// holds what is not on npm (the app shell, the canvas and cg). The page's
// importmap loads every npm package from a CDN at the version installed here:
// the demo runs the same code as the dev app.
//
// The page is app/public/index.html with its local URLs replaced: the two
// pages cannot drift apart. Each replacement must match, or the build stops.
import * as esbuild from 'esbuild';
import * as sass from 'sass';
import { entries, imports, jsd } from '../importmap.mjs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import {
  existsSync, mkdirSync, rmSync, copyFileSync, readFileSync, writeFileSync, readdirSync,
} from 'node:fs';

const here = dirname(fileURLToPath(import.meta.url)); // cg-app/app
const cgApp = join(here, '..');
const repo = join(cgApp, '..');
const cg = join(repo, 'cg');
const out = join(repo, 'docs');

const fail = (msg) => { console.error('build:demo: ' + msg); process.exit(1); };


// Imports of Node built-ins sit behind Node checks and never run in a browser.
const nodeOnly = /^(node:|fs$|fs\/promises$|module$|path$|url$|worker_threads$)/;
const covered = (spec) => spec in imports || nodeOnly.test(spec)
  || Object.keys(imports).some((k) => k.endsWith('/') && spec.startsWith(k));

const local = {
  // cg's sources, not dist/cg.mjs: the app shell imports cg's parser files
  // by path, and one copy of each module serves both.
  '@wcohen/cg': join(cg, 'src', 'cg', 'index.mjs'),
  'cg-app/canvas': join(cgApp, 'shared', 'squint', 'js', 'cg_gui', 'canvas', 'canvas.jsx'),
};
const inlineLocal = {
  name: 'inline-local',
  setup(build) {
    build.onResolve({ filter: /^[@a-z]/ }, (args) =>
      local[args.path] ? { path: local[args.path] } : { path: args.path, external: true });
  },
};

const externalsOf = (metafile) => {
  const s = new Set();
  for (const input of Object.values(metafile.inputs)) {
    for (const i of input.imports) if (i.external && !i.path.startsWith('<')) s.add(i.path);
  }
  return s;
};

mkdirSync(out, { recursive: true });
// vendor/ held gdal-wasm before it was on npm.
for (const stale of ['cg-app.mjs', 'cg-wasmts-handler.mjs', 'index.html', 'css', 'examples',
                     'modules', 'vendor']) {
  rmSync(join(out, stale), { recursive: true, force: true });
}

const result = await esbuild.build({
  entryPoints: [join(here, 'js', 'cg_app', 'ui', 'app.jsx')],
  bundle: true,
  format: 'esm',
  platform: 'browser',
  target: ['es2022'],
  jsx: 'automatic',
  jsxImportSource: 'react',
  loader: { '.js': 'js', '.mjs': 'js', '.jsx': 'jsx' },
  define: { 'process.env.NODE_ENV': '"production"' },
  // cg's own build keeps every top-level form (cg/esbuild.config.mjs).
  treeShaking: false,
  keepNames: true,
  minify: true,
  metafile: true,
  plugins: [inlineLocal],
  logLevel: 'error',
  outfile: join(out, 'cg-app.mjs'),
});

// Each bare import of the bundle, and of each CDN file that the importmap
// names, must resolve through the importmap.
const missing = new Set([...externalsOf(result.metafile)].filter((s) => !covered(s)));
for (const [spec, e] of Object.entries(entries)) {
  if (!e.file || spec.endsWith('/') || e.file.endsWith('.wasm')) continue;
  const r = await esbuild.build({
    entryPoints: [e.file], bundle: true, format: 'esm', platform: 'browser',
    packages: 'external', write: false, metafile: true, logLevel: 'error', outdir: '/',
  });
  for (const s of externalsOf(r.metafile)) if (!covered(s)) missing.add(`${s} (from ${spec})`);
}
if (missing.size > 0) fail('the importmap does not cover: ' + [...missing].join(', '));

let html = readFileSync(join(here, 'public', 'index.html'), 'utf8');
const replace = (from, to) => {
  if (!html.includes(from)) fail('app/public/index.html has no ' + JSON.stringify(from.slice(0, 60)));
  html = html.replace(from, to);
};
const mapStart = html.indexOf('<!-- Importmap covers');
const mapEnd = html.indexOf('</script>', html.indexOf('<script type="importmap">'));
if (mapStart < 0 || mapEnd < 0) fail('app/public/index.html has no importmap');
html = html.slice(0, mapStart)
  + '<script>window.__CG_JS_ONLY__ = true;</script>\n'
  + '<script type="importmap">\n' + JSON.stringify({ imports }, null, 2) + '\n'
  + html.slice(mapEnd);
replace('href="/vendor/react-flow.css"', `href="${jsd('@xyflow/react', 'dist/style.css').url}"`);
replace('href="/canvas/cg.css"', 'href="css/cg.css"');
replace('href="/css/styles.css"', 'href="css/styles.css"');
replace('href="/css/cg-shell.css"', 'href="css/cg-shell.css"');
replace('href="/vendor/maplibre-gl.css"', `href="${jsd('maplibre-gl', 'dist/maplibre-gl.css').url}"`);
replace('<link rel="manifest" href="./manifest.webmanifest">\n', '');
const swStart = html.indexOf('<script>\n  // PWA only');
if (swStart < 0) fail('app/public/index.html has no service worker script');
html = html.slice(0, swStart) + html.slice(html.indexOf('</script>\n', swStart) + '</script>\n'.length);
replace('src="/vendor/wasmts/wasmts.js"', `src="${jsd('@wcohen/wasmts', 'dist/wasmts.js').url}"`);
replace('from "/dist/cg-app.mjs"', 'from "./cg-app.mjs"');
if (/(href|src)="\/[^/]|from\s+"\/[^/]/.test(html)) fail('docs/index.html keeps a root-absolute URL');
writeFileSync(join(out, 'index.html'), html);

// Carbon's @font-face rules name /fonts/IBM-Plex-*; the families are on npm.
const plex = { Sans: 'plex-sans', Mono: 'plex-mono', Serif: 'plex-serif' };
let css = sass.compile(join(cgApp, 'shared', 'ui', 'src', 'styles.scss'), {
  loadPaths: [join(cgApp, 'node_modules')], style: 'compressed', quietDeps: true,
}).css;
for (const [fam, name] of Object.entries(plex)) {
  css = css.replaceAll(`/fonts/IBM-Plex-${fam}/`, jsd('@ibm/' + name, '').url);
}
if (css.includes('url(/fonts/') || css.includes('url("/fonts/')) fail('styles.css keeps a /fonts URL');
mkdirSync(join(out, 'css', 'cg-brand'), { recursive: true });
writeFileSync(join(out, 'css', 'styles.css'), css);
copyFileSync(join(cgApp, 'shared', 'ui', 'public', 'css', 'cg-shell.css'), join(out, 'css', 'cg-shell.css'));
copyFileSync(join(cgApp, 'shared', 'squint', 'src', 'cg_gui', 'canvas', 'cg.css'), join(out, 'css', 'cg.css'));
copyFileSync(join(cgApp, 'shared', 'ui', 'public', 'css', 'cg-brand', 'icons.svg'),
             join(out, 'css', 'cg-brand', 'icons.svg'));

// index.json lists what the Examples menu shows. A file it leaves out, such as
// boston.cg, still opens by ?url=examples/<file>.
mkdirSync(join(out, 'examples'), { recursive: true });
const examples = join(cg, 'examples');
for (const f of readdirSync(examples).filter((n) => n.endsWith('.cg') || n === 'index.json')) {
  copyFileSync(join(examples, f), join(out, 'examples', f));
}

// run/load-module reads its files from modules/src/cg/.
mkdirSync(join(out, 'modules', 'src', 'cg'), { recursive: true });
for (const f of readdirSync(join(cg, 'src', 'cg')).filter((n) => n.endsWith('.cg'))) {
  copyFileSync(join(cg, 'src', 'cg', f), join(out, 'modules', 'src', 'cg', f));
}

// cg.run finds the WasmTS worker handler next to the bundle that holds it.
copyFileSync(join(cg, 'dist', 'cg-wasmts-handler.mjs'), join(out, 'cg-wasmts-handler.mjs'));

writeFileSync(join(out, '.nojekyll'), '');

const bytes = Object.values(result.metafile.outputs)[0].bytes;
console.log(`Built docs/: cg-app.mjs ${(bytes / 1024).toFixed(0)} KB, ` +
            `${Object.keys(imports).length} importmap entries`);
