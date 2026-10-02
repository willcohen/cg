// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Bundle cg-gui.map.core into a browser ESM file for the cg-vscode execution
// webview. The cg-app bundle has its own copy. maplibre-gl and squint-cljs stay
// external, because the importmap of the webview serves them.
import * as esbuild from 'esbuild';
import { mkdirSync } from 'fs';

mkdirSync('../shared/ui/public', { recursive: true });

await esbuild.build({
  entryPoints: ['../shared/squint/src/cg_gui/map/core.mjs'],
  bundle: true,
  format: 'esm',
  platform: 'browser',
  external: ['maplibre-gl', 'squint-cljs', 'squint-cljs/core.js'],
  // squint emits top-level await, which needs es2022.
  target: ['es2022'],
  sourcemap: true,
  minify: false,
  minifyWhitespace: true,
  define: { 'process.env.NODE_ENV': '"production"' },
  outfile: '../shared/ui/public/cg-map.mjs',
});

console.log('Built shared/ui/public/cg-map.mjs');
