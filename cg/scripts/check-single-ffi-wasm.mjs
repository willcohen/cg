// Fail the install when more than one copy of ffi-wasm (clj-native) is present.
//
// clj-native must be a singleton: dispatch holds per-library registries and the
// pool ref, so a second copy gives a second set of registries. The two ways a
// duplicate appears here are both silent:
//
//  1. Spec mismatch. cg declares ffi-wasm as a directory (symlinked, live)
//     while the packed gdal-wasm declares it as a tarball. npm cannot dedupe
//     two different specs, so it nests the tarball copy under gdal-wasm, where
//     it shadows the symlink.
//  2. Stale npm cache. ffi-wasm-0.0.1.tgz keeps the same version across every
//     rebuild, so npm's cache can serve an older build under the same key.
//
// Either way the nested copy is missing exports that the GDAL bindings call, and the
// failure surfaces far from its cause as "TypeError: dispatch.library is not a
// function" with zero tests run. The `overrides` block in package.json prevents
// both. This check confirms the prevention held.

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

// Warn rather than fail. A symlinked dev dependency (proj-wasm) legitimately
// carries its own node_modules, and Node resolves symlinks to their real path,
// and proj-wasm loads that copy. Blocking every install on a longstanding
// duplicate helps nobody. The value here is visibility: the failure this
// catches is otherwise silent.
if (found.size > 1) {
  console.warn('\nWARNING: ffi-wasm (clj-native) resolves to more than one copy:');
  for (const p of found) console.warn('  ' + p);
  console.warn(
    '\nEach copy carries its own dispatch registries and pool ref, and packages\n' +
    'resolving different copies do not share clj-native state. Confirm the\n' +
    'copies are the same build:\n' +
    '  cmp <copy-a>/src/cljc/net/willcohen/native/dispatch.mjs \\\n' +
    '      <copy-b>/src/cljc/net/willcohen/native/dispatch.mjs\n' +
    'A copy nested under a tarball-installed package shadows the symlink and\n' +
    'is the dangerous case; clear it with:\n' +
    '  rm -rf node_modules/*/node_modules/ffi-wasm && npm install\n' +
    'If it returns, the npm cache is serving a stale ffi-wasm-0.0.1.tgz:\n' +
    '  npm cache clean --force && rm -rf node_modules package-lock.json && npm install\n'
  );
}

if (found.size === 0) {
  console.error('ffi-wasm (clj-native) is not installed.');
  process.exit(1);
}
