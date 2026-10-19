// Warn when more than one copy of ffi-wasm (clj-native) is present. Fail the
// install when no copy is present.
//
// clj-native must be a singleton: dispatch holds per-library registries and the
// pool ref, so a second copy gives a second set of registries. cg names
// ffi-wasm 0.0.3 from the registry, gdal-wasm names it as a peer dependency,
// and `overrides` in package.json pins the version. A second copy comes from a
// package that is linked from a local checkout, which carries its own
// node_modules.
//
// A second copy can lack exports that the GDAL bindings call, and the failure
// surfaces far from its cause as "TypeError: dispatch.library is not a
// function" with zero tests run. This check makes the duplicate visible.

import { readdirSync, realpathSync, existsSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = join(dirname(fileURLToPath(import.meta.url)), '..');
const nodeModules = join(root, 'node_modules');
const found = new Set();

function record(path) {
  if (existsSync(path)) found.add(realpathSync(path));
}

function scan(dir) {
  if (!existsSync(dir)) return;
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    if (!entry.isDirectory() && !entry.isSymbolicLink()) continue;
    if (entry.name === '.bin') continue;
    const pkgDir = join(dir, entry.name);
    if (entry.name.startsWith('@')) { scan(pkgDir); continue; }
    if (entry.name === 'ffi-wasm') { record(pkgDir); continue; }
    record(join(pkgDir, 'node_modules', 'ffi-wasm'));
  }
}

scan(nodeModules);

// Warn rather than fail: a linked package loads the copy under its real
// path. The failure that this check catches is otherwise silent.
if (found.size > 1) {
  console.warn('\nWARNING: ffi-wasm (clj-native) resolves to more than one copy:');
  for (const p of found) console.warn('  ' + p);
  console.warn(
    '\nEach copy carries its own dispatch registries and pool ref, and packages\n' +
    'resolving different copies do not share clj-native state. Confirm the\n' +
    'copies are the same build:\n' +
    '  cmp <copy-a>/dist/ffi-wasm.mjs <copy-b>/dist/ffi-wasm.mjs\n' +
    'A copy nested under another package is the dangerous case; clear it with:\n' +
    '  rm -rf node_modules/*/node_modules/ffi-wasm && npm install\n'
  );
}

if (found.size === 0) {
  console.error('ffi-wasm (clj-native) is not installed.');
  process.exit(1);
}
