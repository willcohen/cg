// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Copy the webview assets into dist/webview, where editor.cljs can pass them
// to asWebviewUri. cg-app builds the canvas bundle (`bb bundle:canvas`).
import { mkdirSync, copyFileSync, existsSync } from 'fs';

mkdirSync('dist/webview', { recursive: true });
mkdirSync('dist/webview/cg-brand', { recursive: true });

copyFileSync('node_modules/@xyflow/react/dist/style.css',
             'dist/webview/react-flow.css');

copyFileSync('../cg-app/shared/ui/public/css/cg-brand/icons.svg',
             'dist/webview/cg-brand/icons.svg');

// The copy keeps the canvas under localResourceRoots. A missing file is fatal,
// because there is no fallback renderer.
const squintDist = '../cg-app/shared/squint/dist';
for (const [src, dst] of [
  [`${squintDist}/cg-canvas.mjs`, 'dist/webview/cg-canvas.mjs'],
  [`${squintDist}/cg.css`,        'dist/webview/cg.css'],
]) {
  if (!existsSync(src)) {
    console.error(`MISSING squint canvas asset: ${src}`);
    console.error(`  Run \`bb bundle:canvas\` in cg-app first.`);
    process.exit(1);
  }
  copyFileSync(src, dst);
}

console.log('Webview assets staged: dist/webview (cg-canvas.mjs, cg.css, react-flow.css, cg-brand/icons.svg)');
