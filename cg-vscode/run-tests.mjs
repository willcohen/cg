// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Compile each cljs.test namespace in place to .mjs and run it with node.
// Each file runs its deftests and exits non-zero on a failure.
import { readdirSync } from 'node:fs';
import { join } from 'node:path';
import { runEach, squintCompiler } from '../cg-app/shared/squint-cli.mjs';

const squint = squintCompiler(import.meta.url);
const testDir = 'test/cg_vscode';
const files = readdirSync(testDir)
  .filter((f) => /_test\.clj[sc]$/.test(f))
  .sort();

squint(files, testDir);
runEach(files.map((f) => join(testDir, f.replace(/\.clj[sc]$/, '.mjs'))));
