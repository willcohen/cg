// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

import { readdirSync } from 'node:fs';
import { createRequire } from 'node:module';
import { spawnSync, execFileSync } from 'node:child_process';
import { dirname, join } from 'node:path';

export const cljFiles = (root) =>
  readdirSync(root, { recursive: true })
    .map((f) => f.replaceAll('\\', '/'))
    .filter((f) => /\.clj[sc]$/.test(f))
    .sort();

// `base` is the import.meta.url of the caller, because cg-app and cg-vscode
// each pin their own squint-cljs. Not npx: on win32, Node runs npx.cmd only
// through a shell.
export const squintCompiler = (base) => {
  const cli = join(dirname(createRequire(base).resolve('squint-cljs')), 'node_cli.js');
  return (args, cwd) =>
    execFileSync(process.execPath, [cli, 'compile', ...args], { cwd, stdio: 'inherit' });
};

export const runEach = (mjsFiles) => {
  let failed = 0;
  for (const mjs of mjsFiles) {
    console.log(`\n=== ${mjs} ===`);
    if (spawnSync('node', [mjs], { stdio: 'inherit' }).status !== 0) failed++;
  }
  if (failed > 0) {
    console.error(`\n${failed} test file(s) failed`);
    process.exit(1);
  }
  console.log('\nAll test files passed');
};
