// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Bundle the app shell into one browser ESM file. React is in the bundle.
import * as esbuild from 'esbuild';
import { mkdirSync, copyFileSync, writeFileSync } from 'fs';

mkdirSync('dist', { recursive: true });

const result = await esbuild.build({
  entryPoints: ['js/cg_app/ui/app.jsx'],
  bundle: true,
  format: 'esm',
  platform: 'browser',
  // The importmap of the host page resolves these at run time. fs/promises
  // and module are node-only imports of web-tree-sitter. They are behind
  // isNode checks and never run in a browser.
  external: ['@wcohen/cg', 'cg-app/canvas', 'fs/promises', 'module',
             'maplibre-gl', 'backproj', 'maplibre-proj', 'proj-wasm'],
  jsx: 'automatic',
  jsxImportSource: 'react',
  // squint emits top-level `await import(...)` for requires. Top-level await
  // needs es2022.
  target: ['es2022'],
  sourcemap: true,
  minify: false,
  minifyWhitespace: true,
  define: { 'process.env.NODE_ENV': '"production"' },
  metafile: true,
  outfile: 'dist/cg-app.mjs',
});

// bb bundle:check and serve.mjs read this metafile to find a stale bundle.
// The bundle holds the parser and signatures of cg core, and a recompile of
// cg does not rebuild it.
writeFileSync('dist/cg-app.meta.json', JSON.stringify(result.metafile));

console.log('Built dist/cg-app.mjs');

// web-tree-sitter finds its wasm file relative to import.meta.url, which is
// the URL of the bundle. The wasm file must be next to dist/cg-app.mjs.
copyFileSync('../../cg/node_modules/web-tree-sitter/web-tree-sitter.wasm',
             'dist/web-tree-sitter.wasm');
console.log('Copied web-tree-sitter.wasm');
