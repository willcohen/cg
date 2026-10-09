// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Node test runner for the app shell. squint compiles each file in place to
// .mjs, because node cannot import .jsx.
// - src/ compiles from the root of its tree. The CLI flags override
//   squint.edn, and cross-ns requires become relative './state.mjs' imports.
//   A compile per file would emit bare 'cg-app.ui.state' package imports.
// - test/ compiles per file in the directory of the file. Test files import
//   the src .mjs by path, never by ns require.
import { join, dirname, basename } from 'node:path';
import { cljFiles, runEach, squintCompiler } from '../shared/squint-cli.mjs';

const squint = squintCompiler(import.meta.url);

const compileTree = (root, keepSrc = () => true) => {
  const src = cljFiles(join(root, 'src')).filter(keepSrc);
  squint([...src.map((f) => join('src', f)), '--extension', '.mjs', '--output-dir', 'src'], root);
  const tests = cljFiles(join(root, 'test')).filter((f) => /_test\.clj[sc]$/.test(basename(f)));
  for (const f of tests) squint([basename(f)], join(root, 'test', dirname(f)));
  return tests.map((f) => join(root, 'test', f).replace(/\.clj[sc]$/, '.mjs'));
};

const appTests = compileTree('.');
// The canvas/ namespaces of the shared GUI tree are JSX, which node cannot
// import. The browser probes test them.
const sharedTests = compileTree(join('..', 'shared', 'squint'), (f) => !f.startsWith('cg_gui/canvas/'));

runEach([...appTests, ...sharedTests]);
