// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

import * as esbuild from 'esbuild';
import { mkdirSync, writeFileSync, unlinkSync } from 'fs';

mkdirSync('dist', { recursive: true });

const buildConfig = {
  entryPoints: ['./src/cg/index.mjs'],
  bundle: true,
  format: 'esm',
  platform: 'neutral',  // Use neutral for browser + Node.js compatibility
  outfile: 'dist/cg.mjs',
  external: [
    // WasmTS must be external - it's not an ES6 module
    '@wcohen/wasmts',
    'squint-cljs',
    // cljc-flow: core.async.flow dialect for squint
    'cljc-flow',
    // Arquero is dynamically imported for dataset operations
    'arquero',
    // PROJ coordinate transformation library (clj-proj WASM)
    'proj-wasm',
    // GDAL / OGR vector + raster I/O (clj-gdal WASM)
    'gdal-wasm',
    // web-tree-sitter: tree-sitter WASM runtime for the .cg parser
    'web-tree-sitter',
    // ffi-wasm (npm name for clj-native): worker pool + handler runtime for
    // the joint pool (proj + wasmts + gdal handlers co-resident per worker)
    'ffi-wasm',
    // Node.js built-ins used on Node-only paths. Marked external so esbuild
    // emits the prefixed specifiers verbatim; they resolve at runtime when
    // those paths actually fire under Node, and stay inert in the browser.
    'node:fs', 'node:url', 'node:path', 'node:worker_threads'
  ],
  // No banner - Node.js-specific setup should be done in a wrapper script
  // Browser will load WasmTS directly via CDN if needed
  loader: {
    '.js': 'js',
    '.mjs': 'js',
  },
  keepNames: true,
  treeShaking: false,  // Disable tree-shaking to preserve protocol methods
  metafile: true,
  sourcemap: false,
};

// Separate bundle for the wasmts joint-pool handler.
//
// Worker-router workers dynamically `import()` the handler module URL
// at pool-init time; the module must be a standalone ESM with its
// imports resolved (handler_runtime inlined). Bundling here pulls in
// clj-native/handler-runtime + squint-cljs/core but keeps @wcohen/wasmts
// external — the handler loads wasmts itself via a dynamic import of
// the URL the page passes in initArgs.wasmtsJsUrl.
const wasmtsHandlerConfig = {
  entryPoints: ['./src/cg/wasmts_handler.mjs'],
  bundle: true,
  format: 'esm',
  platform: 'neutral',
  outfile: 'dist/cg-wasmts-handler.mjs',
  external: [
    // The worker dynamically imports wasmts from the URL passed in
    // initArgs.wasmtsJsUrl; bundling it here would double-load.
    '@wcohen/wasmts',
    'node:*',
  ],
  loader: { '.js': 'js', '.mjs': 'js' },
  keepNames: true,
  treeShaking: false,
  sourcemap: false,
};

async function build() {
  try {
    console.log('Building @wcohen/cg bundle...');

    const result = await esbuild.build(buildConfig);

    const text = await esbuild.analyzeMetafile(result.metafile);
    console.log(text);

    console.log('\nBuild complete! Distribution in dist/cg.mjs');

    console.log('Building cg-wasmts-handler (joint-pool worker module)...');
    await esbuild.build(wasmtsHandlerConfig);
    console.log('Build complete! Handler in dist/cg-wasmts-handler.mjs');
  } catch (error) {
    console.error('Build failed:', error);
    process.exit(1);
  }
}

build();
