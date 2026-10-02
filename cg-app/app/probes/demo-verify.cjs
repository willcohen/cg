// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// demo-verify: the GitHub Pages demo in docs/, served the way Pages serves it.
// The files sit under /cg/: a root-absolute URL in the app gets a 404 here
// as it would on Pages. The server has no /vendor or /api route: each package
// on npm must come from its CDN through the importmap. Needs the network.
//
// The URL checks open .cg files from a second origin: the same server as
// 127.0.0.1 in place of localhost. /files/ serves cg/examples with a CORS
// header, and /files-no-cors/ serves it with none. The probe opens boston.cg
// but never runs it, because a Run fetches its layers from public GIS servers.
// A Run of parcel.cg sends one request to the parcels layer of the city. The
// probe answers that request with a parcel that it makes in memory, because
// a probe must not depend on a public server.
//
// Run: node cg-app/app/probes/demo-verify.cjs (after bb build:demo)
const path = require('path');
const http = require('http');
const fs = require('fs');
const { chromium } = require('playwright');

const PORT = 3193;
const PREFIX = '/cg/';
const BASE = `http://localhost:${PORT}`;
const DOCS = path.resolve(__dirname, '..', '..', '..', 'docs');
const EXAMPLES = path.resolve(__dirname, '..', '..', '..', 'cg', 'examples');
const FILES = `http://127.0.0.1:${PORT}`;
// The 404 and the CORS checks make the browser log these failures.
const EXPECTED = /missing\.cg|files-no-cors/;
const CITY = 'https://gis.bostonplans.org/**';

const ring = [[-71.059, 42.360], [-71.059, 42.361], [-71.058, 42.361],
  [-71.058, 42.360], [-71.059, 42.360]];
const parcelReply = JSON.stringify({
  objectIdFieldName: 'OBJECTID',
  geometryType: 'esriGeometryPolygon',
  spatialReference: { wkid: 4326 },
  fields: [
    { name: 'OBJECTID', type: 'esriFieldTypeOID' },
    { name: 'MAP_PAR_ID', type: 'esriFieldTypeString', length: 26 },
    { name: 'FULL_ADDRESS', type: 'esriFieldTypeString', length: 199 },
    { name: 'OWNER', type: 'esriFieldTypeString', length: 500 },
    { name: 'YR_BUILT', type: 'esriFieldTypeSmallInteger' },
  ],
  features: [{
    attributes: { OBJECTID: 1, MAP_PAR_ID: 'TEST-1', FULL_ADDRESS: 'TEST PARCEL',
      OWNER: 'TEST OWNER', YR_BUILT: 1970 },
    geometry: { rings: [ring] },
  }],
});

const results = [];
function check(desc, ok, detail) {
  results.push(!!ok);
  console.log((ok ? 'PASS ' : 'FAIL ') + desc + (ok ? '' : (detail ? ' ' + detail : '')));
}

const TYPES = {
  '.html': 'text/html', '.mjs': 'text/javascript', '.js': 'text/javascript',
  '.css': 'text/css', '.json': 'application/json', '.wasm': 'application/wasm',
  '.svg': 'image/svg+xml', '.cg': 'text/plain', '.woff2': 'font/woff2',
};

function serve() {
  return http.createServer((req, res) => {
    const url = new URL(req.url, BASE);
    let p = decodeURIComponent(url.pathname);
    const files = p.match(/^\/(files|files-no-cors)\/([^/]+)$/);
    if (files) {
      const head = files[1] === 'files' ? { 'access-control-allow-origin': '*' } : {};
      const f = path.join(EXAMPLES, files[2]);
      if (!fs.existsSync(f)) { res.writeHead(404, head); res.end(); return; }
      res.writeHead(200, { ...head, 'content-type': 'text/plain' });
      fs.createReadStream(f).pipe(res);
      return;
    }
    if (!p.startsWith(PREFIX)) { res.writeHead(404); res.end(); return; }
    p = p.slice(PREFIX.length) || 'index.html';
    if (p.endsWith('/')) p += 'index.html';
    const f = path.join(DOCS, p);
    if (!f.startsWith(DOCS + path.sep) || !fs.existsSync(f) || fs.statSync(f).isDirectory()) {
      res.writeHead(404); res.end(); return;
    }
    res.writeHead(200, { 'content-type': TYPES[path.extname(f)] || 'application/octet-stream' });
    fs.createReadStream(f).pipe(res);
  }).listen(PORT);
}

// The basemap host is slow at times, and a slow tile is not a demo defect.
const NOISE = /ResizeObserver loop|net::ERR_|Failed to load resource.*(tiles|openstreetmap)|AbortError/i;

const ready = (page) => page.waitForFunction(() => {
  const b = document.querySelector('[data-testid=run-button]');
  return b && !b.disabled;
}, null, { timeout: 60000 }).then(() => true).catch(() => false);

const statusText = (page) => page.evaluate(() =>
  (document.querySelector('[data-testid=status]') || {}).textContent);

const drawnParcel = (page) => page.waitForFunction(() => {
  const m = window.__cgMap;
  const src = m && m.getLayer('cgdata-city-hall') && m.getStyle().sources['cgsrc-city-hall'];
  const fs = src && src.data && src.data.features;
  return fs ? fs.map((f) => f.properties || {}) : null;
}, null, { timeout: 15000 }).then((h) => h.jsonValue()).catch(() => null);

const loadedAs = (page, name) => page.waitForFunction((n) => {
  const el = document.querySelector('[data-testid=status]');
  return el && el.textContent.startsWith(n);
}, name, { timeout: 30000 }).then(() => true).catch(() => false);

const urlError = (page) => page.waitForSelector('[data-testid=url-error]', { timeout: 30000 })
  .then((h) => h.textContent()).catch(() => null);

const runOk = async (page) => {
  await page.click('[data-testid=run-button]');
  const ok = await page.waitForFunction(() => {
    const el = document.querySelector('[data-testid=exec-status]');
    const s = el && el.getAttribute('data-exec-status');
    return s === 'ok' || s === 'error' || s === 'refused';
  }, null, { timeout: 90000 }).then(() => page.evaluate(() =>
    document.querySelector('[data-testid=exec-status]').getAttribute('data-exec-status') === 'ok'))
    .catch(() => false);
  const text = await page.evaluate(() => {
    const el = document.querySelector('[data-testid=exec-status]');
    return el ? el.textContent : null;
  });
  return { ok, text };
};

(async () => {
  let browser;
  let server;
  const errors = [];
  const local = [];
  const backend = [];
  const city = [];
  try {
    if (!fs.existsSync(path.join(DOCS, 'index.html'))) {
      throw new Error('no docs/index.html (run bb build:demo in cg-app)');
    }
    server = serve();
    browser = await chromium.launch({ headless: true });
    const page = await browser.newPage({ viewport: { width: 1600, height: 1000 } });
    await page.context().route(CITY, (route) => {
      city.push(route.request().url());
      route.fulfill({ status: 200, contentType: 'application/json',
        headers: { 'access-control-allow-origin': '*' }, body: parcelReply });
    });
    page.on('console', (msg) => {
      const where = msg.text() + ' ' + ((msg.location() || {}).url || '');
      if (msg.type() === 'error' && !NOISE.test(msg.text()) && !EXPECTED.test(where)) {
        errors.push('console: ' + msg.text());
      }
    });
    page.on('pageerror', (err) => {
      if (!NOISE.test(err.message || '')) errors.push('pageerror: ' + err.message);
    });
    page.on('request', (req) => {
      const u = new URL(req.url());
      if (u.port === '3000' || u.pathname.startsWith('/api/')) backend.push(req.url());
    });
    page.on('response', (res) => {
      const u = new URL(res.url());
      if (u.protocol === 'http:' && u.origin === BASE) local.push({ path: u.pathname, status: res.status() });
    });

    await page.goto(BASE + PREFIX, { waitUntil: 'domcontentloaded' });
    const booted = await page.waitForSelector('.cg-shell-load', { timeout: 60000 })
      .then(() => true).catch(() => false);
    check('1 the shell renders', booted);

    const options = await page.waitForFunction(() => {
      const s = document.querySelector('[data-testid=examples-menu]');
      return s && s.options.length > 0 ? Array.from(s.options).map((o) => o.value) : null;
    }, null, { timeout: 30000 }).then((h) => h.jsonValue()).catch(() => null);
    // boston.cg is not in the menu, because a Run of it sends many requests
    // to public GIS servers.
    check('2 the Examples menu lists parcel.cg and simple.cg, and not boston.cg',
      Array.isArray(options) && options.some((v) => v.endsWith('/parcel.cg') || v === 'parcel.cg')
        && options.some((v) => v.endsWith('simple.cg'))
        && !options.some((v) => v.endsWith('boston.cg')),
      'options=' + JSON.stringify(options));

    const empty = await page.evaluate(() =>
      (document.querySelector('[data-testid=status]') || {}).textContent);
    await page.selectOption('[data-testid=examples-menu]',
      options ? options.find((v) => v.endsWith('parcel.cg')) : 'parcel.cg').catch(() => null);
    const first = await page.waitForFunction(() => {
      const el = document.querySelector('[data-testid=status]');
      return el && el.textContent.includes('parcel.cg');
    }, null, { timeout: 30000 }).then(() => true).catch(() => false);
    check('3 nothing loads on start, and the menu loads parcel.cg',
      empty === 'no file loaded' && first, 'status at start=' + JSON.stringify(empty));

    // Monaco loads from the CDN after the file. Its editor must show the
    // text of the file, and not the text that it had before the load. The
    // check reads one time and does not poll, because a later change of the
    // flow puts the text in place and hides the defect.
    const firstLine = fs.readFileSync(path.join(EXAMPLES, 'parcel.cg'), 'utf8').split('\n')[0];
    await page.waitForFunction(() => window.monaco && window.monaco.editor.getEditors().length > 0,
      null, { timeout: 60000 }).catch(() => null);
    await page.waitForTimeout(3000);
    const codeLine = await page.evaluate(() => {
      const eds = window.monaco && window.monaco.editor.getEditors();
      return eds && eds.length > 0 ? eds[0].getModel().getLineContent(1) : null;
    });
    check('4 the code panel shows the first line of parcel.cg after the Examples load',
      codeLine === firstLine, 'line 1=' + JSON.stringify(codeLine));

    check('5 JS-only: no runtime chip', (await page.$('[data-testid=runtime-chip]')) === null);

    if ((await page.getAttribute('[data-testid=panel-toggle-map]', 'aria-pressed')) !== 'true') {
      await page.click('[data-testid=panel-toggle-map]');
    }
    const mapUp = await page.waitForFunction(
      () => window.__cgMap && window.__cgMap.isStyleLoaded && window.__cgMap.isStyleLoaded(),
      null, { timeout: 90000 }).then(() => true).catch(() => false);
    check('6 the map boots', mapUp);

    const canRun = await ready(page);
    const run = canRun ? await runOk(page) : { ok: false, text: 'Run never enabled' };
    check('7 Run of parcel.cg completes in JavaScript with one request to the city, which the probe answers',
      run.ok && /\(js\)/.test(run.text || '') && city.length === 1
        && /where=MAP_PAR_ID/.test(decodeURIComponent(city[0]).replace(/\+/g, ' ').replace(/ /g, '')),
      'exec=' + JSON.stringify(run.text) + ' requests=' + JSON.stringify(city));

    const drawn = run.ok ? await drawnParcel(page) : null;
    check('8 the map shows cgdata-city-hall with the one parcel and its attributes',
      Array.isArray(drawn) && drawn.length === 1 && drawn[0].FULL_ADDRESS === 'TEST PARCEL'
        && drawn[0].OWNER === 'TEST OWNER' && drawn[0].YR_BUILT === 1970,
      'properties=' + JSON.stringify(drawn));

    await page.selectOption('[data-testid=examples-menu]',
      options ? options.find((v) => v.endsWith('simple.cg')) : 'simple.cg').catch(() => null);
    const second = await page.waitForFunction(() => {
      const el = document.querySelector('[data-testid=status]');
      return el && el.textContent === 'simple.cg';
    }, null, { timeout: 30000 }).then(() => true).catch(() => false);
    const run2 = second && (await ready(page)) ? await runOk(page) : { ok: false, text: 'not loaded' };
    const buffers = run2.ok ? await page.waitForFunction(() => {
      const m = window.__cgMap;
      const src = m && m.getLayer('cgdata-buffered') && m.getStyle().sources['cgsrc-buffered'];
      return src && src.data && src.data.features ? src.data.features.length : null;
    }, null, { timeout: 15000 }).then((h) => h.jsonValue()).catch(() => null) : null;
    check('9 simple.cg loads with no diagnostics, runs, and draws three buffers',
      second && run2.ok && buffers === 3,
      'loaded=' + second + ' exec=' + JSON.stringify(run2.text) + ' features=' + buffers);

    await page.click('[data-testid=open-url-button]');
    await page.fill('[data-testid=url-input]', FILES + '/files/boston.cg');
    await page.click('[data-testid=url-open]');
    const boston = await loadedAs(page, 'boston.cg');
    const bostonStatus = await statusText(page);
    const zoningModule = local.some((r) => r.path === PREFIX + 'modules/src/cg/zoning.cg' && r.status === 200);
    const barClosed = (await page.$('[data-testid=url-bar]')) === null;
    const notRun = (await page.$('[data-testid=exec-status]')) === null;
    const search = await page.evaluate(() => new URLSearchParams(location.search).get('url'));
    const bostonLine = fs.readFileSync(path.join(EXAMPLES, 'boston.cg'), 'utf8').split('\n')[0];
    const bostonCode = await page.waitForFunction((want) => {
      const eds = window.monaco && window.monaco.editor.getEditors();
      return eds && eds.length > 0 && eds[0].getModel().getLineContent(1) === want;
    }, bostonLine, { timeout: 30000 }).then(() => true).catch(() => false);
    check('10 the URL box opens boston.cg from another origin, shows its code, reads its modules from modules/, and does not run it',
      boston && !/error/.test(bostonStatus || '') && zoningModule && barClosed && notRun && bostonCode
        && search === FILES + '/files/boston.cg',
      JSON.stringify({ boston, bostonStatus, zoningModule, barClosed, notRun, bostonCode, search }));

    await page.goto(BASE + PREFIX + '?url=' + encodeURIComponent(FILES + '/files/parcel.cg'),
      { waitUntil: 'domcontentloaded' });
    const byParam = await loadedAs(page, 'parcel.cg');
    await page.waitForTimeout(1500);
    check('11 ?url= opens parcel.cg and does not run it',
      byParam && (await page.$('[data-testid=exec-status]')) === null && city.length === 1,
      'status=' + JSON.stringify(await statusText(page)) + ' requests=' + city.length);

    if ((await page.getAttribute('[data-testid=panel-toggle-map]', 'aria-pressed')) !== 'true') {
      await page.click('[data-testid=panel-toggle-map]');
    }
    await page.waitForFunction(
      () => window.__cgMap && window.__cgMap.isStyleLoaded && window.__cgMap.isStyleLoaded(),
      null, { timeout: 90000 }).catch(() => null);
    const run3 = byParam && (await ready(page)) ? await runOk(page) : { ok: false, text: 'not loaded' };
    const drawn3 = run3.ok ? await drawnParcel(page) : null;
    check('12 Run of parcel.cg opened by URL completes and draws the one parcel',
      run3.ok && Array.isArray(drawn3) && drawn3.length === 1 && city.length === 2,
      'exec=' + JSON.stringify(run3.text) + ' properties=' + JSON.stringify(drawn3)
        + ' requests=' + city.length);

    const missing = FILES + '/files/missing.cg';
    await page.goto(BASE + PREFIX + '?url=' + encodeURIComponent(missing), { waitUntil: 'domcontentloaded' });
    const err404 = await urlError(page);
    const status404 = await statusText(page);
    const kept = await page.inputValue('[data-testid=url-input]').catch(() => null);
    check('13 a URL with no file gives HTTP 404 in the URL box and in the status line',
      /HTTP 404/.test(err404 || '') && /HTTP 404/.test(status404 || '') && kept === missing,
      JSON.stringify({ err404, status404, kept }));

    await page.fill('[data-testid=url-input]', FILES + '/files-no-cors/simple.cg');
    await page.click('[data-testid=url-open]');
    const errCors = await page.waitForFunction(() => {
      const el = document.querySelector('[data-testid=url-error]');
      return el && /CORS/.test(el.textContent) ? el.textContent : null;
    }, null, { timeout: 30000 }).then((h) => h.jsonValue()).catch(() => null);
    check('14 a server with no CORS header gives the CORS message', !!errCors,
      'url-error=' + JSON.stringify(await page.textContent('[data-testid=url-error]').catch(() => null)));

    check('15 no request reaches a backend', backend.length === 0, JSON.stringify(backend.slice(0, 5)));

    // This call loads the gdal handler, libgdal.wasm and proj.db from the
    // CDN into a worker, which has no importmap.
    const gdalRelease = await page.evaluate(() => Promise.race([
      (async () => {
        const gdal = await import('gdal-wasm');
        await gdal.init();
        return await gdal.gdal_version_info('RELEASE_NAME');
      })(),
      new Promise((resolve) => setTimeout(() => resolve('timeout'), 120000)),
    ])).catch((e) => 'error: ' + (e && e.message));
    check('16 gdal-wasm from the CDN runs a GDAL call in a worker',
      /^3\.11\./.test(gdalRelease), 'release=' + JSON.stringify(gdalRelease));

    const bad = local.filter((r) => !r.path.startsWith(PREFIX) || r.status >= 400
      || r.path.startsWith(PREFIX + 'vendor/'));
    check('17 each local request is a file under /cg/ (no /vendor)',
      bad.length === 0, JSON.stringify(bad.slice(0, 8)));

    check('18 no console or page errors', errors.length === 0, JSON.stringify(errors.slice(0, 5)));
  } catch (e) {
    console.log('FAIL probe crashed: ' + (e && e.stack ? e.stack.split('\n')[0] : e));
  } finally {
    while (results.length < 18) results.push(false);
    if (browser) await browser.close();
    if (server) server.close();
  }
  const pass = results.filter(Boolean).length;
  console.log(pass === results.length ? 'ALL PASS' : `SOME FAILED (${pass}/${results.length})`);
  process.exit(pass === results.length ? 0 : 1);
})();
