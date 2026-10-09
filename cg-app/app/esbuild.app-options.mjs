// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// esbuild.app.mjs and esbuild.app-static.mjs use these options, so that
// app/dist and app/dist-static get the same bundle.
import { fileURLToPath } from 'node:url';

export const appOptions = {
  entryPoints: [fileURLToPath(new URL('js/cg_app/ui/app.jsx', import.meta.url))],
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
  minifyWhitespace: true,
  define: { 'process.env.NODE_ENV': '"production"' },
};
