// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Serve the built PWA bundle (app/dist-static/) over http. Native WebViews
// load the same files through app://. http://localhost is a secure context,
// which lets the service worker register. serve.mjs serves the dev tree.
import { createServer } from 'node:http';
import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { dirname, join, resolve, sep, extname } from 'node:path';

const here = dirname(fileURLToPath(import.meta.url));
const root = join(here, 'dist-static');
const port = Number(process.env.PORT || 3100);

const MIME = {
  '.html': 'text/html', '.mjs': 'text/javascript', '.js': 'text/javascript',
  '.css': 'text/css', '.wasm': 'application/wasm', '.svg': 'image/svg+xml',
  '.json': 'application/json', '.map': 'application/json',
  '.webmanifest': 'application/manifest+json', '.ttf': 'font/ttf',
  '.woff': 'font/woff', '.woff2': 'font/woff2', '.png': 'image/png',
};

createServer(async (req, res) => {
  try {
    let p = decodeURIComponent(req.url.split('?')[0]);
    if (p === '/') p = '/index.html';
    const resolved = resolve(root, '.' + p);
    if (resolved !== root && !resolved.startsWith(root + sep)) {
      res.writeHead(403);
      res.end('forbidden');
      return;
    }
    const body = await readFile(resolved);
    res.writeHead(200, { 'content-type': MIME[extname(resolved)] || 'application/octet-stream' });
    res.end(body);
  } catch {
    res.writeHead(404);
    res.end('not found');
  }
}).listen(port, '127.0.0.1', () => {
  console.log(`Serving app/dist-static at http://localhost:${port}`);
});
