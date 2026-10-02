// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Bundle the canvas into one ESM file for the VS Code editor webview. React and
// @xyflow/react are in the bundle, because the webview has no module resolver
// and its CSP blocks bare-specifier fetches. Run after `squint compile`, which
// writes js/cg_gui/canvas/*.jsx.
import * as esbuild from 'esbuild';
import { mkdirSync, copyFileSync } from 'fs';

mkdirSync('dist', { recursive: true });

await esbuild.build({
  entryPoints: ['js/cg_gui/canvas/canvas.jsx'],
  bundle: true,
  format: 'esm',
  platform: 'browser',
  external: [],
  jsx: 'automatic',
  jsxImportSource: 'react',
  // squint emits top-level `await import(...)` for requires. Top-level await
  // needs es2022.
  target: ['es2022'],
  sourcemap: true,
  minify: false,
  minifyWhitespace: true,
  // The webview has no `process.env`.
  define: { 'process.env.NODE_ENV': '"production"' },
  outfile: 'dist/cg-canvas.mjs',
});

// The webview loads cg.css with a <link>. The cg-vscode build copies it from
// here.
copyFileSync('src/cg_gui/canvas/cg.css', 'dist/cg.css');

console.log('Built dist/cg-canvas.mjs + dist/cg.css');
