// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Compile the cg-vscode sources with squint and bundle the extension host
// with esbuild. The output is CJS, because the VS Code extension host loads
// only CommonJS (microsoft/vscode#135450).
import { readdirSync } from 'node:fs';
import { execFileSync } from 'node:child_process';
import * as esbuild from 'esbuild';

const minify = process.argv.includes('--minify');

// The sources import the shared GUI namespaces of cg-app by path, compiled
// in place to .mjs. The canvas namespaces are JSX and stay in cg-app's
// bundle:canvas.
const shared = '../cg-app/shared/squint';
const sharedFiles = readdirSync(`${shared}/src`, { recursive: true })
  .filter((f) => /\.(cljs|cljc)$/.test(f) && !f.startsWith('cg_gui/canvas/'))
  .map((f) => `src/${f}`)
  .sort();
execFileSync('npx', ['squint', 'compile', ...sharedFiles, '--extension', '.mjs', '--output-dir', 'src'],
  { cwd: shared, stdio: 'inherit' });

const srcFiles = readdirSync('src', { recursive: true })
  .filter((f) => /\.(cljs|cljc)$/.test(f))
  .map((f) => `src/${f}`)
  .sort();

execFileSync('npx', ['squint', 'compile', ...srcFiles], { stdio: 'inherit' });

await esbuild.build({
  entryPoints: ['src/cg_vscode/extension.mjs'],
  bundle: true,
  platform: 'node',
  format: 'cjs',
  target: 'node18',
  outfile: 'dist/extension.js',
  external: ['vscode', 'web-tree-sitter', '@yogthos/tree-sitter-clojure'],
  // The .mjs files of cg resolve squint-cljs from cg/node_modules. The alias
  // keeps one squint core in the bundle.
  alias: { 'squint-cljs': './node_modules/squint-cljs' },
  // CJS has no import.meta. The shim gives the file URL of the bundle.
  define: { 'import.meta.url': 'import_meta_url' },
  inject: ['./import-meta-url-shim.js'],
  minify,
  logLevel: 'info',
});
