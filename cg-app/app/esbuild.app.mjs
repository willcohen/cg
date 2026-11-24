// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Bundle the app shell into one browser ESM file. React is in the bundle.
import * as esbuild from 'esbuild';
import { copyFileSync, writeFileSync } from 'fs';
import { appOptions } from './esbuild.app-options.mjs';

const result = await esbuild.build({
  ...appOptions,
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
