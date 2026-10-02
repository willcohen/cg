// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// Checks that an operation (a defn with the metadata ^:op) runs on the
// app's JS backend. The fixture defines measure-parcel as
// cg/test/fixtures/typed-clean.cg does and uses it as the rung of an item->
// model over one 10 x 10 square. The canvas
// must draw the call site as a resolved op, and Run must give 100, the
// square's area, on the JS backend. Offline: no basemap and no backend. Spawns
// serve.mjs on PORT 3190.
const path = require('path');
const http = require('http');
const { spawn } = require('child_process');
const { chromium } = require('playwright');

const PORT = 3190;
const BASE = `http://localhost:${PORT}`;
const FIXTURE = path.join(__dirname, 'fixtures', 'op-rung.cg');
const CALL_SITE = 'areas__measure-parcel-1';
const N = 5;

const results = [];
function check(desc, ok, detail) {
  results.push(!!ok);
  console.log((ok ? 'PASS ' : 'FAIL ') + desc + (ok ? '' : (detail ? ' ' + detail : '')));
}

function waitForServer(timeoutMs) {
  const t0 = Date.now();
  return new Promise((resolve, reject) => {
    (function poll() {
      const req = http.get(BASE + '/', (res) => { res.resume(); resolve(); });
      req.on('error', () => {
        if (Date.now() - t0 > timeoutMs) reject(new Error('server never came up'));
        else setTimeout(poll, 200);
      });
    })();
  });
}

(async () => {
  let browser;
  let server;
  const errors = [];
  try {
    server = spawn('node', ['serve.mjs'], {
      cwd: path.join(__dirname, '..'),
      env: { ...process.env, PORT: String(PORT), CG_BACKEND_AUTODETECT: '0' },
      stdio: 'ignore',
    });
    await waitForServer(10000);
    browser = await chromium.launch({ headless: true });
    const page = await browser.newPage({ viewport: { width: 1400, height: 900 } });
    page.on('console', (msg) => {
      const text = msg.text();
      if (msg.type() === 'error' && !/ResizeObserver loop|net::ERR_CONNECTION_REFUSED|Failed to load resource/.test(text)) {
        errors.push('console: ' + text);
      }
    });
    page.on('pageerror', (err) => errors.push('pageerror: ' + (err && err.message)));

    await page.goto(BASE + '/');
    const up = await page.waitForSelector('.cg-shell-load', { timeout: 15000 })
      .then(() => true).catch(() => false);
    check('1 page loads', up);

    // Check 2: the canvas draws the op call site, resolved
    await page.setInputFiles('[data-testid=file-input]', FIXTURE);
    const drawn = await page.waitForFunction(
      (id) => Array.from(document.querySelectorAll('.react-flow__node'))
        .some((n) => n.getAttribute('data-id') === id),
      CALL_SITE, { timeout: 40000 }
    ).then(() => true).catch(() => false);
    const node = await page.evaluate((id) => {
      const n = Array.from(document.querySelectorAll('.react-flow__node'))
        .find((x) => x.getAttribute('data-id') === id);
      return n ? { unresolved: !!n.querySelector('.cg-node.unresolved-op'), text: n.textContent } : null;
    }, CALL_SITE);
    check('2 the canvas draws measure-parcel as a resolved op',
      drawn && node && !node.unresolved, 'node=' + JSON.stringify(node));

    await page.waitForFunction(() => {
      const b = document.querySelector('[data-testid=run-button]');
      return b && !b.disabled;
    }, null, { timeout: 30000 });
    await page.click('[data-testid=run-button]');
    const ran = await page.waitForFunction(() => {
      const el = document.querySelector('[data-testid=exec-status]');
      return el && el.getAttribute('data-exec-status') !== 'running' && el.getAttribute('data-exec-status');
    }, null, { timeout: 60000 }).then(() => true).catch(() => false);
    const exec = await page.evaluate(() => {
      const el = document.querySelector('[data-testid=exec-status]');
      return el ? { status: el.getAttribute('data-exec-status'), text: el.textContent } : null;
    });
    check('3 Run completes ok on the JS backend',
      ran && exec && exec.status === 'ok' && /\(js\)/.test(exec.text), 'exec=' + JSON.stringify(exec));

    // Check 4: the result is the square's area
    // The shell opens the Results panel when a run gives a value, and a
    // click on an open panel closes it.
    if ((await page.getAttribute('[data-testid=panel-toggle-results]', 'aria-pressed')) !== 'true') {
      await page.click('[data-testid=panel-toggle-results]').catch(() => {});
    }
    const badge = await page.waitForSelector('[data-testid=results-scalar]', { timeout: 10000 })
      .then(() => page.evaluate(() => document.querySelector('[data-testid=results-scalar]').textContent))
      .catch(() => null);
    check('4 the result badge shows areas = 100', badge === 'areas100', 'badge=' + JSON.stringify(badge));

    if (process.env.PROBE_SHOTS) {
      await page.screenshot({ path: path.join(process.env.PROBE_SHOTS, 'op-js.png') });
    }

    // Check 5: no console/page errors
    check('5 no console/page errors', errors.length === 0, JSON.stringify(errors));
  } catch (e) {
    console.log('FAIL probe crashed: ' + (e && e.stack ? e.stack.split('\n')[0] : e));
    while (results.length < N) results.push(false);
  } finally {
    if (browser) await browser.close();
    if (server) server.kill();
  }

  const allPass = results.length === N && results.every(Boolean);
  console.log(allPass ? 'ALL PASS' : 'SOME FAILED');
  process.exit(allPass ? 0 : 1);
})();
