// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Runs `bb build:vscode` in ../cg-app: the cg bundles, the canvas, and the map
// and run cores that the webviews load. These files are not in git, and a
// copy older than the shared code breaks the webviews at run time.
//
// F5 starts this from the environment of VS Code, which can lack the dev
// shell and the PATH of a login shell. Outside the dev shell the command runs
// through `$SHELL -lc` and `direnv exec <repo root>`, the same route as the
// backend that runtime.cljs spawns.
import { spawnSync } from 'node:child_process';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = dirname(fileURLToPath(import.meta.url));
const repo = resolve(here, '..');
const cgApp = resolve(repo, 'cg-app');
const inDevShell = Boolean(process.env.IN_NIX_SHELL || process.env.DIRENV_DIR);

const quote = (s) => `'${s.replaceAll("'", "'\\''")}'`;

const r = inDevShell
  ? spawnSync('bb', ['build:vscode'], { cwd: cgApp, stdio: 'inherit' })
  : spawnSync(process.env.SHELL || '/bin/zsh',
      ['-lc', `exec direnv exec ${quote(repo)} bb build:vscode`],
      { cwd: cgApp, stdio: 'inherit' });

if (r.error) {
  console.error(`bb build:vscode did not start: ${r.error.message}`);
  process.exit(1);
}
process.exit(r.status ?? 1);
