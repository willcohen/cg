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
// Each test file runs its deftests and exits non-zero on a failure.
import { readdirSync } from 'node:fs';
import { spawnSync, execFileSync } from 'node:child_process';
import { join, dirname, basename } from 'node:path';

const cljFiles = (root) =>
  readdirSync(root, { recursive: true })
    .filter((f) => /\.clj[sc]$/.test(f))
    .sort();

const compileInPlace = (root, files) => {
  for (const f of files) {
    execFileSync('npx', ['squint', 'compile', basename(f)], {
      cwd: join(root, dirname(f)),
      stdio: 'inherit',
    });
  }
};

execFileSync('npx',
  ['squint', 'compile',
   ...cljFiles('src').map((f) => join('src', f)),
   '--extension', '.mjs', '--output-dir', 'src'],
  { stdio: 'inherit' });

const testFiles = cljFiles('test').filter((f) => /_test\.clj[sc]$/.test(basename(f)));
compileInPlace('test', testFiles);

// The shared GUI tree compiles the same way. Its canvas/ namespaces are JSX,
// which node cannot import. The browser probes test them.
const SHARED = join('..', 'shared', 'squint');
const sharedSrc = cljFiles(join(SHARED, 'src')).filter((f) => !f.startsWith('cg_gui/canvas/'));
if (sharedSrc.length > 0) {
  execFileSync('npx',
    ['squint', 'compile', ...sharedSrc.map((f) => join('src', f)),
     '--extension', '.mjs', '--output-dir', 'src'],
    { cwd: SHARED, stdio: 'inherit' });
}
const sharedTests = cljFiles(join(SHARED, 'test')).filter((f) => /_test\.clj[sc]$/.test(basename(f)));
compileInPlace(join(SHARED, 'test'), sharedTests);

const testMjs = [
  ...testFiles.map((f) => join('test', f)),
  ...sharedTests.map((f) => join(SHARED, 'test', f)),
].map((f) => f.replace(/\.clj[sc]$/, '.mjs'));

let failed = 0;
for (const mjs of testMjs) {
  console.log(`\n=== ${mjs} ===`);
  const r = spawnSync('node', [mjs], { stdio: 'inherit' });
  if (r.status !== 0) failed++;
}

if (failed > 0) {
  console.error(`\n${failed} test file(s) failed`);
  process.exit(1);
}
console.log('\nAll test files passed');
