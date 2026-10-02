// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// The one importmap of the npm packages: the demo, the dev web app and the
// cg-vscode webviews load each package from a CDN at the version installed
// here. `node importmap.mjs` writes it to shared/ui/public/importmap.json,
// which the cg-vscode map page reads.
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { existsSync, readFileSync, writeFileSync, mkdirSync } from 'node:fs';

const cgApp = dirname(fileURLToPath(import.meta.url));
const cg = join(cgApp, '..', 'cg');

const fail = (msg) => { throw new Error('importmap: ' + msg); };

// A package's installed name and version. The npm alias worker-router
// installs @wcohen/worker-router, and the CDN knows only the real name.
// `under` finds a copy nested in another package (lru-cache in backproj).
export function pkg(name, under) {
  const dirs = [];
  for (const root of [cgApp, cg]) {
    if (under) dirs.push(join(root, 'node_modules', under, 'node_modules', name));
    dirs.push(join(root, 'node_modules', name));
  }
  const found = dirs.filter((d) => existsSync(join(d, 'package.json')))
    .map((d) => ({ dir: d, ...JSON.parse(readFileSync(join(d, 'package.json'), 'utf8')) }));
  if (found.length === 0) fail(`${name} is not installed`);
  // cg-app and cg each resolve their own copy; one importmap entry serves both.
  if (!under && new Set(found.map((p) => p.version)).size > 1) {
    fail(`${name}: cg-app and cg install different versions (${found.map((p) => p.version)})`);
  }
  return found[0];
}

export const jsd = (name, path, under) => {
  const p = pkg(name, under);
  return { url: `https://cdn.jsdelivr.net/npm/${p.name}@${p.version}/${path}`, file: join(p.dir, path) };
};
// esm.sh converts CommonJS. React stays a bare import: one copy serves all.
export const esm = (name, sub = '', query = '?external=react,react-dom', under) => {
  const p = pkg(name, under);
  return { url: `https://esm.sh/${p.name}@${p.version}${sub}${query}` };
};

const ffiWasm = jsd('ffi-wasm', 'dist/ffi-wasm.mjs');
export const entries = {
  'squint-cljs': jsd('squint-cljs', 'index.js'),
  'squint-cljs/': jsd('squint-cljs', ''),
  'cljc-flow/flow': jsd('cljc-flow', 'out/cljc_flow/flow.mjs'),
  'cljc-flow/core': jsd('cljc-flow', 'out/cljc_flow/core.mjs'),
  // esm.sh applies arquero's browser field, which drops its Node streams.
  'arquero': esm('arquero', '', '?external=@uwdata/flechette,acorn'),
  '@uwdata/flechette': jsd('@uwdata/flechette', 'src/index.js'),
  'acorn': jsd('acorn', 'dist/acorn.mjs'),
  'web-tree-sitter': jsd('web-tree-sitter', 'web-tree-sitter.js'),
  '@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm':
    jsd('@yogthos/tree-sitter-clojure', 'tree-sitter-clojure.wasm'),
  'ffi-wasm': ffiWasm,
  'worker-router': jsd('worker-router', 'dist/index.mjs'),
  'worker-router/worker-bootstrap': jsd('worker-router', 'dist/worker-bootstrap.mjs'),
  'comlink': jsd('comlink', 'dist/esm/comlink.mjs'),
  'comlink/': jsd('comlink', ''),
  'resource-tracker': jsd('resource-tracker', 'resource.mjs'),
  'proj-wasm': jsd('proj-wasm', 'dist/proj.mjs'),
  'backproj': jsd('backproj', 'dist/backproj.mjs'),
  'maplibre-proj': jsd('maplibre-proj', 'dist/maplibre-proj.mjs'),
  'lru-cache': jsd('lru-cache', 'dist/esm/index.js', 'backproj'),
  'pbf': esm('pbf', '', '', 'backproj'),
  '@mapbox/vector-tile': esm('@mapbox/vector-tile', '', '', 'backproj'),
  'maplibre-gl': esm('maplibre-gl', '', '?bundle-deps'),
  'react': esm('react', '', ''),
  'react/jsx-runtime': esm('react', '/jsx-runtime', ''),
  'react-dom': esm('react-dom', '', '?external=react'),
  'react-dom/client': esm('react-dom', '/client', '?external=react'),
  '@xyflow/react': esm('@xyflow/react'),
  '@carbon/react': esm('@carbon/react'),
  '@carbon/react/icons': esm('@carbon/react', '/icons'),
  '@carbon-labs/react-resizer': esm('@carbon-labs/react-resizer'),
  '@monaco-editor/react': esm('@monaco-editor/react'),
  'elkjs/lib/elk.bundled.js': esm('elkjs', '/lib/elk.bundled.js', ''),
  'monaco-editor/min/': jsd('monaco-editor', 'min/'),
  'gdal-wasm': jsd('gdal-wasm', 'gdal.mjs'),
  'gdal-wasm/fndefs': jsd('gdal-wasm', 'fndefs.mjs'),
  // A worker loads the handler, and a worker has no importmap: its own
  // imports are not checked here. The handler gets ffi-wasm from a URL in its
  // init args, and it loads libgdal.mjs next to itself, on the CDN.
  'gdal-wasm/gdal-handler': { url: jsd('gdal-wasm', 'gdal-handler.mjs').url },
  'gdal-wasm/proj.db': { url: jsd('gdal-wasm', 'proj.db').url },
};
// gdal-wasm 0.0.1 imports ffi-wasm by subpath. The ffi-wasm bundle exports
// each of them (cg-app/esbuild.app-bundle.mjs, plugin one-ffi-wasm).
for (const sub of ['dispatch', 'handler-env', 'handler-paths', 'macros',
                   'platform-state', 'pool', 'workload-pool']) {
  entries['ffi-wasm/' + sub] = ffiWasm;
}
export const imports = Object.fromEntries(Object.entries(entries).map(([k, v]) => [k, v.url]));

// The files that index.html names outside the importmap.
export const pageUrls = {
  '/vendor/react-flow.css': jsd('@xyflow/react', 'dist/style.css').url,
  '/vendor/maplibre-gl.css': jsd('maplibre-gl', 'dist/maplibre-gl.css').url,
  '/vendor/wasmts/wasmts.js': jsd('@wcohen/wasmts', 'dist/wasmts.js').url,
};

// The hosts of the URLs above, for a Content-Security-Policy.
export const hosts = [...new Set(Object.values(imports).map((u) => new URL(u).origin))].sort();

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const out = join(cgApp, 'shared', 'ui', 'public', 'importmap.json');
  mkdirSync(dirname(out), { recursive: true });
  writeFileSync(out, JSON.stringify({ imports, pageUrls, hosts }, null, 2) + '\n');
  console.log(`Wrote ${out}: ${Object.keys(imports).length} entries`);
}
