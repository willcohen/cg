// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

import * as esbuild from 'esbuild';
import { mkdirSync } from 'fs';

mkdirSync('shared/ui/public', { recursive: true });

// gdal-wasm 0.0.1 imports ffi-wasm by subpath. ffi-wasm ships one bundle
// that exports each of those subpaths: the bundles import the one
// specifier and a page maps only "ffi-wasm". The worker-side subpaths are
// separate files and keep their names.
const oneFfiWasm = {
  name: 'one-ffi-wasm',
  setup(build) {
    const ownFiles = new Set(['ffi-wasm/handler', 'ffi-wasm/fetch-worker', 'ffi-wasm/test-runner']);
    build.onResolve({ filter: /^ffi-wasm\// }, (args) =>
      ownFiles.has(args.path) ? undefined : { path: 'ffi-wasm', external: true });
  },
};

const sharedConfig = {
  entryPoints: ['../cg/dist/cg.mjs'],
  plugins: [oneFfiWasm],
  bundle: true,
  format: 'esm',
  external: [
    '@wcohen/wasmts',
    'proj-wasm',
    'backproj',
    'maplibre-proj',
    'resource-tracker',
    'threads',
    // Same as cg/esbuild.config.mjs. node_modules resolves these in the
    // extension host, and the importmap resolves them in the webview.
    'ffi-wasm',        // clj-native on npm
    'worker-router',
    'comlink',
    'web-tree-sitter',
  ],
  loader: { '.js': 'js', '.mjs': 'js' },
  keepNames: true,
  treeShaking: false,
  metafile: true,
  sourcemap: true,
  minify: false,
  minifyWhitespace: true,
  minifySyntax: true,
  target: ['es2020'],
};

const nodeBundleConfig = {
  ...sharedConfig,
  platform: 'node',
  outfile: 'shared/ui/public/cg-bundled-node.mjs',
};

// platform 'browser' does not make node:* external, and esbuild cannot remove
// the dynamic imports behind Node checks. As externals they stay as
// `import('node:fs')` and run only in a Node host.
const browserBundleConfig = {
  ...sharedConfig,
  platform: 'browser',
  // gdal-wasm finds proj.db and its handler next to its own URL, which an
  // importmap gives and a bundle does not.
  external: [...sharedConfig.external, 'node:*', 'gdal-wasm', 'gdal-wasm/*'],
  outfile: 'shared/ui/public/cg-bundled-browser.mjs',
};

async function build() {
  try {
    console.log('Building CG app bundles...');
    console.log('  Input:    ../cg/dist/cg.mjs');
    console.log('  Outputs:  shared/ui/public/cg-bundled-node.mjs');
    console.log('            shared/ui/public/cg-bundled-browser.mjs');
    console.log('');

    const [nodeResult, browserResult] = await Promise.all([
      esbuild.build(nodeBundleConfig),
      esbuild.build(browserBundleConfig),
    ]);

    console.log('--- node bundle ---');
    console.log(await esbuild.analyzeMetafile(nodeResult.metafile));
    console.log('--- browser bundle ---');
    console.log(await esbuild.analyzeMetafile(browserResult.metafile));

    console.log('\n✓ Bundles complete.');
    console.log('  cg-vscode → cg-bundled-node.mjs (extension host is Node)');
    console.log('  cg-app webview → cg-bundled-browser.mjs');
  } catch (error) {
    console.error('Build failed:', error);
    process.exit(1);
  }
}

build();
