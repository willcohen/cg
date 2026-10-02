// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// The harness of the canvas probes. It mounts dist/cg-canvas.mjs in headless
// Chromium on a page shaped like the cg-vscode webview, with a stub host that
// records each message the canvas posts in window.__posted. Each section of
// run([...sections]) gets a fresh page and two more checks: the canvas posts
// cg/ready, and the page logs no console or page error.
'use strict';

const fs = require('fs');
const path = require('path');
const { chromium } = require('playwright');

const SQUINT = path.resolve(__dirname, '..');
const CG_APP = path.resolve(SQUINT, '..', '..');

// page.route answers every request to this origin. No server runs and no
// file is written.
const ORIGIN = 'http://canvas.probe/';

const FILES = {
  'cg-canvas.mjs': path.join(SQUINT, 'dist', 'cg-canvas.mjs'),
  'cg.css': path.join(SQUINT, 'dist', 'cg.css'),
  'react-flow.css': path.join(CG_APP, 'node_modules', '@xyflow', 'react', 'dist', 'style.css'),
};

const SPRITE = fs.readFileSync(
  path.join(CG_APP, 'shared', 'ui', 'public', 'css', 'cg-brand', 'icons.svg'), 'utf8')
  .replace(/<\?xml[^>]*>/, '')
  .replace(/<!--[\s\S]*?-->/g, '');

const TOKENS = {
  dark: { surface: '#1e1e1e', 'surface-raised': '#2a2a2a', text: '#d4d4d4', ink: '#d4d4d4',
          'ink-muted': '#9a9a9a', line: '#444444', 'line-soft': '#3a3a3a', accent: '#4fc1ff',
          run: '#d2a64a', 'run-bg': '#332b18', complete: '#4ec97f', 'complete-bg': '#16301f',
          error: '#e05661', 'error-bg': '#34181b', radius: '6px' },
  light: { surface: '#ffffff', 'surface-raised': '#f3f3f3', text: '#1f2933', ink: '#1f2933',
           'ink-muted': '#6b7785', line: '#cccccc', 'line-soft': '#dddddd', accent: '#0066cc',
           run: '#9a6700', 'run-bg': '#fff4d6', complete: '#1a7f37', 'complete-bg': '#e2f5e9',
           error: '#cf222e', 'error-bg': '#ffe5e7', radius: '6px' },
};
const CODE_TOKENS = {
  dark: { keyword: '#c586c0', string: '#ce9178' },
  light: { keyword: '#af00db', string: '#a31515' },
};

const vars = (prefix, obj) =>
  Object.entries(obj).map(([k, v]) => `    --${prefix}${k}: ${v};`).join('\n');

// The classic script must run before the module script: host.cljs memoizes
// acquireVsCodeApi when the bundle loads.
function pageHtml(tokens, codeTokens) {
  return `<!DOCTYPE html>
<html lang="en" data-theme="dark">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1.0">
<link rel="stylesheet" href="./react-flow.css">
<link rel="stylesheet" href="./cg.css">
<style id="cg-tokens">
:root {
${vars('cg-', tokens.dark)}
${vars('code-', codeTokens.dark)}
}
</style>
<style>
html, body, #root { width: 100%; height: 100%; margin: 0; overflow: hidden; }
.react-flow__node { font-size: 12px; }
:root.cg-pulse-locked .cg-node.running .cg-stat span { animation: none; opacity: var(--cg-pulse-opacity, 1); }
</style>
<script>
window.__CG_TOKENS__ = ${JSON.stringify(tokens)};
window.__CG_CODE_TOKENS__ = ${JSON.stringify(codeTokens)};
window.__posted = [];
window.acquireVsCodeApi = () => ({ postMessage: (m) => window.__posted.push(m) });
</script>
</head>
<body>
<div id="cg-icon-sprite" style="position:absolute;width:0;height:0;overflow:hidden;pointer-events:none;visibility:hidden" aria-hidden="true">${SPRITE}</div>
<div id="root"></div>
<script type="module">
import { mount } from "./cg-canvas.mjs";
mount("root");
</script>
</body>
</html>`;
}

const CONTENT_TYPES = { '.mjs': 'text/javascript', '.css': 'text/css', '.html': 'text/html' };

async function serve(page, html) {
  await page.route(ORIGIN + '**', (route) => {
    const name = new URL(route.request().url()).pathname.slice(1);
    if (name === 'index.html') {
      return route.fulfill({ status: 200, contentType: 'text/html', body: html });
    }
    const file = FILES[name];
    if (!file || !fs.existsSync(file)) return route.fulfill({ status: 404, body: 'no ' + name });
    return route.fulfill({
      status: 200,
      contentType: CONTENT_TYPES[path.extname(file)] || 'application/octet-stream',
      body: fs.readFileSync(file),
    });
  });
}

// "ResizeObserver loop" is a Chromium notice that React Flow layouts trigger;
// it does not come from canvas code.
const benign = (text) => /ResizeObserver loop/.test(text);

function checker() {
  let failures = 0;
  const check = (desc, ok, detail) => {
    if (ok) console.log('PASS ' + desc);
    else { failures += 1; console.log('FAIL ' + desc + (detail ? ' :: ' + detail : '')); }
  };
  return { check, failures: () => failures };
}

async function runSection(browser, section, check) {
  const page = await browser.newPage({ viewport: section.viewport || { width: 1400, height: 900 } });
  page.setDefaultTimeout(10000);
  const errors = [];
  page.on('console', (m) => {
    if (m.type() === 'error' && !benign(m.text())) errors.push('console: ' + m.text());
  });
  page.on('pageerror', (e) => errors.push('pageerror: ' + e.message));
  try {
    await serve(page, pageHtml(section.tokens || TOKENS, section.codeTokens || CODE_TOKENS));
    await page.goto(ORIGIN + 'index.html');
    const ready = await page.waitForFunction(
      () => window.__posted.some((m) => m && m.type === 'cg/ready'), null, { timeout: 20000 })
      .then(() => true).catch(() => false);
    check('canvas mounts and posts cg/ready', ready);
    if (ready) await section.body(page, check);
  } catch (e) {
    check('section ran to completion', false, e && e.stack ? e.stack.split('\n').slice(0, 3).join(' | ') : String(e));
  }
  // A late async error needs a moment to land.
  await page.waitForTimeout(300);
  check('no console or page errors', errors.length === 0, JSON.stringify(errors.slice(0, 6)));
  await page.close();
}

async function run(sections) {
  const { check, failures } = checker();
  const browser = await chromium.launch({ headless: true });
  try {
    for (const section of sections) {
      console.log('== ' + section.name);
      await runSection(browser, section, check);
    }
  } finally {
    await browser.close();
  }
  console.log(failures() === 0 ? 'ALL PASS' : failures() + ' FAILED');
  process.exitCode = failures() === 0 ? 0 : 1;
}

async function send(page, message) {
  await page.evaluate((m) => window.postMessage(m, '*'), message);
}

const sendFlow = (page, flow) => send(page, { type: 'cg/flow', flow });

const sendTrace = (page, event) => send(page, { type: 'cg/trace-event', event });

async function postedSince(page, index) {
  return page.evaluate((i) => window.__posted.slice(i), index);
}

const postedCount = (page) => page.evaluate(() => window.__posted.length);

async function edits(page) {
  return page.evaluate(() => window.__posted.filter((m) => m && m.type === 'cg/edit'));
}

async function waitForEditCount(page, n) {
  return page.waitForFunction(
    (k) => window.__posted.filter((m) => m && m.type === 'cg/edit').length >= k,
    n, { timeout: 5000 }).then(() => true).catch(() => false);
}

async function nodeIds(page) {
  return page.evaluate(() =>
    Array.from(document.querySelectorAll('.react-flow__node')).map((n) => n.getAttribute('data-id')));
}

const hasNode = (page, id) =>
  page.evaluate((nid) => !!document.querySelector(`.react-flow__node[data-id="${nid}"]`), id);

async function waitForNodes(page, ids) {
  await page.waitForFunction(
    (want) => want.every((id) => document.querySelector(`.react-flow__node[data-id="${id}"]`)),
    ids, { timeout: 15000 });
}

// A node wrapper's translate is its position in flow coordinates.
async function nodePos(page, id) {
  return page.evaluate((nid) => {
    const el = document.querySelector(`.react-flow__node[data-id="${nid}"]`);
    if (!el) return null;
    const m = /translate\(\s*(-?[\d.]+)px\s*,?\s*(-?[\d.]+)px\s*\)/.exec(el.style.transform || '');
    return m ? { x: parseFloat(m[1]), y: parseFloat(m[2]) } : null;
  }, id);
}

async function zoom(page) {
  return page.evaluate(() => {
    const vp = document.querySelector('.react-flow__viewport');
    const m = vp && /scale\(\s*(-?[\d.eE+-]+)\s*\)/.exec(vp.style.transform || '');
    return m ? parseFloat(m[1]) : 1;
  });
}

async function nodeClass(page, id) {
  return page.evaluate((nid) => {
    const el = document.querySelector(`.react-flow__node[data-id="${nid}"] .cg-node`);
    return el ? el.className : null;
  }, id);
}

async function statText(page, id) {
  return page.evaluate((nid) => {
    const el = document.querySelector(`.react-flow__node[data-id="${nid}"] .cg-stat`);
    return el ? el.textContent.trim() : null;
  }, id);
}

// The canvas fits the view 250 ms after a flow lands. Mouse math needs the
// viewport transform to hold still first.
async function settle(page) {
  await page.waitForTimeout(450);
  await page.evaluate(() => { window.__vpT = null; window.__vpStable = 0; });
  await page.waitForFunction(() => {
    const vp = document.querySelector('.react-flow__viewport');
    if (!vp) return false;
    const t = vp.style.transform;
    if (window.__vpT === t) window.__vpStable += 1;
    else { window.__vpStable = 0; window.__vpT = t; }
    return window.__vpStable >= 2;
  }, null, { polling: 200, timeout: 10000 });
}

// Press on a node and move in steps, which clears React Flow's drag
// threshold. grip is the press point in screen px from the node's top-left;
// the default is its center. The caller releases the mouse.
async function press(page, id, dx, dy, grip) {
  const el = await page.waitForSelector(`.react-flow__node[data-id="${id}"]`, { timeout: 5000 });
  const box = await el.boundingBox();
  const sx = box.x + (grip ? grip.x : box.width / 2);
  const sy = box.y + (grip ? grip.y : box.height / 2);
  await page.mouse.move(sx, sy);
  await page.mouse.down();
  const steps = 10;
  for (let i = 1; i <= steps; i += 1) {
    await page.mouse.move(sx + (dx * i) / steps, sy + (dy * i) / steps);
    await page.waitForTimeout(20);
  }
}

async function drag(page, id, dx, dy, grip) {
  await press(page, id, dx, dy, grip);
  await page.mouse.up();
  await page.waitForTimeout(200);
}

// Math.random fixes the jittered drop position of a toolbar op.
async function withRandom(page, value, f) {
  await page.evaluate((v) => { window.__random = Math.random; Math.random = () => v; }, value);
  try { return await f(); } finally {
    await page.evaluate(() => { Math.random = window.__random; });
  }
}

const near = (a, b, tol = 1.5) =>
  typeof a === 'number' && typeof b === 'number' && Math.abs(a - b) <= tol;

const moved = (a, b) => !!(a && b && (Math.abs(a.x - b.x) > 5 || Math.abs(a.y - b.y) > 5));

module.exports = {
  run, send, sendFlow, sendTrace, postedSince, postedCount, edits, waitForEditCount,
  nodeIds, hasNode, waitForNodes, nodePos, zoom, nodeClass, statText, settle,
  press, drag, withRandom, near, moved,
};
