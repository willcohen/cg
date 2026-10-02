// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Compile each cljs.test namespace in place to .mjs and run it with node.
// Each file runs its deftests and exits non-zero on a failure.
import { readdirSync } from 'node:fs';
import { spawnSync, execFileSync } from 'node:child_process';
import { join } from 'node:path';

const testDir = 'test/cg_vscode';
const files = readdirSync(testDir)
  .filter((f) => /_test\.clj[sc]$/.test(f))
  .sort();

execFileSync('npx', ['squint', 'compile', ...files], {
  cwd: testDir,
  stdio: 'inherit',
});

let failed = 0;
for (const f of files) {
  const mjs = f.replace(/\.clj[sc]$/, '.mjs');
  console.log(`\n=== ${mjs} ===`);
  const r = spawnSync('node', [join(testDir, mjs)], { stdio: 'inherit' });
  if (r.status !== 0) failed++;
}

if (failed > 0) {
  console.error(`\n${failed} test file(s) failed`);
  process.exit(1);
}
console.log('\nAll test files passed');
