// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Node test runner for the app shell. squint compiles each file in place to
// .mjs, because node cannot import .jsx.
// - src/ compiles from the app root. The CLI flags override squint.edn, and
//   cross-ns requires become relative './state.mjs' imports. A compile per
//   file would emit bare 'cg-app.ui.state' package imports.
// - test/ compiles per file in the directory of the file. Test files import
//   the src .mjs by path, never by ns require.
import { join, dirname, basename } from 'node:path';
import { cljFiles, runEach, squintCompiler } from '../shared/squint-cli.mjs';

const squint = squintCompiler(import.meta.url);

const compileInPlace = (root, files) => {
  for (const f of files) squint([basename(f)], join(root, dirname(f)));
};

squint([...cljFiles('src').map((f) => join('src', f)), '--extension', '.mjs', '--output-dir', 'src']);

const testFiles = cljFiles('test').filter((f) => /_test\.clj[sc]$/.test(basename(f)));
compileInPlace('test', testFiles);

// The shared GUI tree compiles the same way. Its canvas/ namespaces are JSX,
// which node cannot import. The browser probes test them.
const SHARED = join('..', 'shared', 'squint');
const sharedSrc = cljFiles(join(SHARED, 'src')).filter((f) => !f.startsWith('cg_gui/canvas/'));
squint([...sharedSrc.map((f) => join('src', f)), '--extension', '.mjs', '--output-dir', 'src'], SHARED);
const sharedTests = cljFiles(join(SHARED, 'test')).filter((f) => /_test\.clj[sc]$/.test(basename(f)));
compileInPlace(join(SHARED, 'test'), sharedTests);

runEach([
  ...testFiles.map((f) => join('test', f)),
  ...sharedTests.map((f) => join(SHARED, 'test', f)),
].map((f) => f.replace(/\.clj[sc]$/, '.mjs')));
