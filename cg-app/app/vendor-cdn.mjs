// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Write the third-party modules of the importmap into the gitignored vendor
// tree from node_modules, with no network access.
//   - esbuild bundles each leaf library into a self-contained file. vt-pbf
//     keeps its own pbf@3 and vector-tile@1, which differ from pbf@4.
//   - backproj and maplibre-proj are ESM bundles with external imports and are
//     copied as-is. The importmap of the host page resolves their imports.
//     backproj loads its pool handlers from URLs next to backproj.mjs. The
//     copy includes each .mjs file of its dist.
// squint-cljs is copied as a directory for the importmap prefix "squint-cljs/".
//
// backproj needs pbf@4, lru-cache@11 and vector-tile@2. npm installs them in
// node_modules/backproj/node_modules, because the top level holds the older
// majors that vt-pbf needs.
import * as esbuild from 'esbuild';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';
import { mkdirSync, rmSync, cpSync, copyFileSync, readdirSync } from 'node:fs';
import { createRequire } from 'node:module';

const here = dirname(fileURLToPath(import.meta.url)); // app/
const root = join(here, '..');                        // cg-app/
const nm = join(root, 'node_modules');
const dest = join(root, 'shared', 'ui', 'public', 'vendor', 'cdn');

mkdirSync(dest, { recursive: true });

// maplibre-gl ships only a UMD build, and an esbuild bundle of UMD has one
// default export. maplibre-proj imports { addProtocol, removeProtocol } by
// name. This entry also exports each runtime key of the package by name.
const mlKeys = Object.keys(createRequire(import.meta.url)(join(nm, 'maplibre-gl')));
const maplibreEntry = [
  "import ml from 'maplibre-gl';",
  'export default ml;',
  ...mlKeys.map((k, i) => `const k${i} = ml[${JSON.stringify(k)}];`),
  `export { ${mlKeys.map((k, i) => `k${i} as ${k}`).join(', ')} };`,
].join('\n');

const leaves = [
  { out: 'maplibre-gl.mjs', stdin: { contents: maplibreEntry, resolveDir: root } },
  { out: 'pbf.mjs',         entry: join(nm, 'backproj', 'node_modules', 'pbf') },
  { out: 'lru-cache.mjs',   entry: join(nm, 'backproj', 'node_modules', 'lru-cache') },
  { out: 'vector-tile.mjs', entry: join(nm, 'backproj', 'node_modules', '@mapbox', 'vector-tile') },
  { out: 'vt-pbf.mjs',      entry: join(nm, 'vt-pbf') },
];

for (const { out, entry, stdin } of leaves) {
  await esbuild.build({
    ...(stdin ? { stdin } : { entryPoints: [entry] }),
    bundle: true,
    format: 'esm',
    platform: 'browser',
    target: ['es2022'],
    outfile: join(dest, out),
    sourcemap: false,
    minify: false,
    minifyWhitespace: true,
    define: { 'process.env.NODE_ENV': '"production"' },
    logLevel: 'warning',
  });
  console.log('bundled', out);
}

const backprojFiles = readdirSync(join(nm, 'backproj', 'dist')).filter((f) => f.endsWith('.mjs'));
for (const f of backprojFiles) copyFileSync(join(nm, 'backproj', 'dist', f), join(dest, f));
copyFileSync(join(nm, 'maplibre-proj', 'dist', 'maplibre-proj.mjs'), join(dest, 'maplibre-proj.mjs'));
console.log('copied', [...backprojFiles, 'maplibre-proj.mjs'].join(', '));

const sq = join(dest, 'squint-cljs');
rmSync(sq, { recursive: true, force: true });
mkdirSync(sq, { recursive: true });
copyFileSync(join(nm, 'squint-cljs', 'core.js'), join(sq, 'core.js'));
cpSync(join(nm, 'squint-cljs', 'src'), join(sq, 'src'), { recursive: true });
console.log('mirrored squint-cljs/');
