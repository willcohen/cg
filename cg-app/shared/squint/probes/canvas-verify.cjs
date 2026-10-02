// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception


// The canvas probes: drawing and run states, node drags, the toolbar and the
// palette, op call sites, and the theme switch. Each section mounts
// dist/cg-canvas.mjs on a fresh page.
// Run from cg-app/shared/squint: node probes/canvas-verify.cjs
'use strict';

const h = require('./canvas-page.cjs');

// The flows the canvas probes post, in the shape that convert.cljs reads. The
// hyphenated keys are the literal property names that convert.cljs reads with
// aget.
const loader = {
  name: 'parcels',
  'body-shape': 'function_call',
  'head-sym': 'geo/read-file',
  args: [],
  location: { line: 1, column: 0 },
};

// A loader and a two-op model. buffer-1 has a stored position.
const SIMPLE = {
  bindings: [
    loader,
    { name: 'simple-buffer-model',
      'body-shape': 'threading_macro',
      input: 'parcels',
      mode: 'table',
      ops: [
        { 'qualified-name': 'geo/buffer',
          args: [],
          location: { line: 3, column: 2 },
          visual: { pos: [400, 250] } },
        { 'qualified-name': 'geo/simplify',
          args: [],
          location: { line: 4, column: 2 } },
      ] },
  ],
  operations: [],
  sinks: [],
};

// A model that calls the op prepare-parcel, whose body is buffer then
// simplify. label names the toolbar tab of the module; with no label the tab
// is "Ops". span gives the call site a full parser span: a Cmd-click on it
// then posts cg/open-defop.
function opFlow({ label, span } = {}) {
  return {
    bindings: [
      loader,
      { name: 'parcel-model',
        'body-shape': 'threading_macro',
        input: 'parcels',
        mode: 'table',
        ops: [
          { 'qualified-name': 'parcel/prepare-parcel',
            args: [],
            location: span
              ? { 'start-line': 3, 'start-column': 2, 'end-line': 3, 'end-column': 30 }
              : { line: 3, column: 2 } },
        ] },
    ],
    operations: [
      { name: 'prepare-parcel',
        inputs: ['g'],
        outputs: ['g'],
        'source-module': label
          ? { path: '/workspace/parcel.cg', label }
          : { path: '/workspace/parcel.cg' },
        'body-graph': {
          nodes: [
            { id: 'buffer-1', fn: 'geo/buffer', location: { line: 2, column: 2 } },
            { id: 'simplify-2', fn: 'geo/simplify', location: { line: 3, column: 2 } },
          ],
          edges: [{ from: 'buffer-1', to: 'simplify-2' }],
        } },
    ],
    sinks: [],
  };
}

const ref = (name) => ({ kind: 'parameter_ref', 'parameter-name': name });

// A def thread and an op thread whose steps take side inputs: a literal, a
// card of the file, a literal map, and an input of the op.
const THREAD = {
  parameters: [
    { name: 'limits', value: { kind: 'mapv', raw: '{:max 3}' }, location: { line: 1, column: 0 } },
  ],
  bindings: [
    loader,
    { name: 'checked',
      'body-shape': 'threading_macro',
      input: 'parcels',
      mode: 'table',
      ops: [
        { 'qualified-name': 'geo/buffer',
          args: [{ kind: 'literal', literal: { kind: 'number', raw: '0.5' } }],
          location: { line: 5, column: 2 } },
        { 'qualified-name': 'check',
          args: [ref('limits'),
                 { kind: 'inline_map', 'inline-map': { kind: 'mapv', raw: '{:name "a" :kind :fill}' } }],
          location: { line: 6, column: 2 } },
      ] },
  ],
  operations: [
    { name: 'check',
      inputs: [{ name: 'row', role: 'row' }, { name: 'schema' }, { name: 'style' }],
      outputs: [{ name: 'result' }] },
    { name: 'tidy-all',
      inputs: [{ name: 'rows', role: 'table' }, { name: 'extra' }],
      outputs: [{ name: 'result', type: 'dataset' }],
      'body-graph': {
        mode: 'table',
        input: 'rows',
        nodes: [
          { id: 'check-1', fn: 'check', args: [ref('extra'), ref('limits')],
            location: { line: 9, column: 4 } },
        ],
        edges: [],
      } },
  ],
  sinks: [],
};

// The canvas node ids of these flows.
const IDS = {
  loader: 'binding-parcels',
  simpleHeader: 'model-simple-buffer-model',
  buffer: 'simple-buffer-model__buffer-1',
  simplify: 'simple-buffer-model__simplify-2',
  opHeader: 'model-parcel-model',
  call: 'parcel-model__prepare-parcel-1',
  body1: 'prepare-parcel-body__buffer-1',
  body2: 'prepare-parcel-body__simplify-2',
  limits: 'param-limits',
  frame: 'model-checked',
  frameBuffer: 'checked__buffer-1',
  frameCheck: 'checked__check-2',
  opFrame: 'op-tidy-all',
  opCheck: 'tidy-all-body__check-1',
};

const SIMPLE_IDS = [IDS.loader, IDS.simpleHeader, IDS.buffer, IDS.simplify];

async function nodesAndRunStates(page, check) {
  await h.sendFlow(page, SIMPLE);
  const drawn = await page.waitForFunction((want) => {
    const ids = Array.from(document.querySelectorAll('.react-flow__node'))
      .map((w) => w.getAttribute('data-id')).sort();
    return document.querySelectorAll('.cg-node').length === want.length
      && JSON.stringify(ids) === JSON.stringify(want.slice().sort());
  }, SIMPLE_IDS).then(() => true).catch(() => false);
  check('the flow draws 4 .cg-node cards: ' + SIMPLE_IDS.join(', '), drawn,
    JSON.stringify(await h.nodeIds(page)));

  await h.sendTrace(page, { kind: 'proc-start', pid: IDS.buffer });
  const running = await page.waitForFunction((pid) =>
    !!document.querySelector(`.react-flow__node[data-id="${pid}"] .cg-node.running`), IDS.buffer)
    .then(() => true).catch(() => false);
  check('proc-start makes the node .running', running, await h.nodeClass(page, IDS.buffer));

  const locked = await page.evaluate(() =>
    document.documentElement.classList.contains('cg-pulse-locked'));
  check('<html> has cg-pulse-locked while a node runs', locked);

  // Three samples at uneven gaps: the cosine cannot give three equal values.
  const samples = [];
  for (const wait of [0, 200, 220]) {
    if (wait) await page.waitForTimeout(wait);
    samples.push(await page.evaluate(() =>
      document.documentElement.style.getPropertyValue('--cg-pulse-opacity')));
  }
  const nums = samples.map(parseFloat);
  check('--cg-pulse-opacity on <html> is in [0, 1] and changes over time',
    nums.every((n) => Number.isFinite(n) && n >= 0 && n <= 1)
      && new Set(nums.map((n) => n.toFixed(6))).size >= 2,
    JSON.stringify(samples));

  await h.sendTrace(page, { kind: 'proc-complete', pid: IDS.buffer });
  const complete = await page.waitForFunction((pid) => {
    const el = document.querySelector(`.react-flow__node[data-id="${pid}"] .cg-node`);
    return !!el && el.classList.contains('complete') && !el.classList.contains('running');
  }, IDS.buffer).then(() => true).catch(() => false);
  check('proc-complete makes the node .complete', complete, await h.nodeClass(page, IDS.buffer));

  await h.send(page, { type: 'cg/run-started' });
  const reset = await page.waitForFunction((n) =>
    document.querySelectorAll('.cg-node.running, .cg-node.complete, .cg-node.error').length === 0
      && document.querySelectorAll('.cg-node.idle').length === n, SIMPLE_IDS.length)
    .then(() => true).catch(() => false);
  check('cg/run-started makes every node idle', reset);

  const before = await h.postedCount(page);
  await page.click('button.cg-run-btn');
  const ran = await page.waitForFunction((b) =>
    window.__posted.slice(b).some((m) => m && m.type === 'cg/run'), before)
    .then(() => true).catch(() => false);
  check('the Run button posts cg/run', ran, JSON.stringify(await h.postedSince(page, before)));
}

async function rowsProgress(page, check) {
  const TOTAL = 98423;
  const progress = (pid, done) => h.sendTrace(page, { kind: 'rows-progress', pid, done, total: TOTAL });
  const pause = () => page.waitForTimeout(120);
  const fill = (id) => page.evaluate((nid) => {
    const el = document.querySelector(`.react-flow__node[data-id="${nid}"] .cg-progress-fill`);
    return el ? el.style.width : null;
  }, id);
  // The stat line formats counts with the locale of the page.
  const rows = (done) => page.evaluate(([d, t]) =>
    d.toLocaleString() + ' / ' + t.toLocaleString() + ' rows', [done, TOTAL]);

  await h.sendFlow(page, SIMPLE);
  await h.waitForNodes(page, [IDS.loader, IDS.buffer, IDS.simplify]);
  await page.waitForTimeout(400);

  await progress(IDS.simplify, 10);
  await pause();
  check('rows-progress on a node that is not running paints no bar and no status',
    (await fill(IDS.simplify)) === null && (await h.statText(page, IDS.simplify)) === null);

  await h.sendTrace(page, { kind: 'proc-start', pid: IDS.buffer });
  await pause();
  check('proc-start reads as running', (await h.statText(page, IDS.buffer)) === 'running');

  await progress(IDS.buffer, 12340);
  await pause();
  const stat1 = await h.statText(page, IDS.buffer);
  check('rows-progress replaces "running" with the row count', stat1 === await rows(12340), stat1);
  check('the bar fills to done / total', (await fill(IDS.buffer)) === '13%', await fill(IDS.buffer));

  // cg.css draws the bar. A stale cg.css leaves the markup right and the bar
  // invisible. Measure the bar.
  const box = await page.evaluate((nid) => {
    const el = document.querySelector(`.react-flow__node[data-id="${nid}"] .cg-progress`);
    if (!el) return null;
    const r = el.getBoundingClientRect();
    return { h: r.height, w: r.width };
  }, IDS.buffer);
  check('the bar renders with a size', !!(box && box.h >= 2 && box.w > 0), JSON.stringify(box));

  await progress(IDS.buffer, 49212);
  await pause();
  const stat2 = await h.statText(page, IDS.buffer);
  check('a later event advances the row count', stat2 === await rows(49212), stat2);
  check('a later event advances the bar', (await fill(IDS.buffer)) === '50%', await fill(IDS.buffer));
  check('progress is per node', (await h.statText(page, IDS.simplify)) === null);

  await h.sendTrace(page, { kind: 'proc-complete', pid: IDS.buffer, ms: 1 });
  await pause();
  check('proc-complete removes the bar and the row count',
    (await h.statText(page, IDS.buffer)) === 'complete' && (await fill(IDS.buffer)) === null);

  await h.sendTrace(page, { kind: 'proc-start', pid: IDS.buffer });
  await pause();
  await progress(IDS.buffer, 777);
  await pause();
  await h.send(page, { type: 'cg/run-started', file: 'f.cg' });
  await pause();
  check('cg/run-started clears the progress and the status with the states',
    (await h.statText(page, IDS.buffer)) === null && (await fill(IDS.buffer)) === null);
}

// cg/run-ended has one shape for the two hosts: {outcome, count?, exitCode?}.
// A backend run gives outcome and count; the bb run subprocess gives exitCode.
async function runEndStatus(page, check) {
  await h.sendFlow(page, SIMPLE);
  const status = () => page.evaluate(() => {
    const el = document.querySelector('[data-testid="console-status"]');
    return el ? el.textContent : null;
  });
  const endWith = async (ended) => {
    await h.send(page, { type: 'cg/run-started' });
    await h.send(page, Object.assign({ type: 'cg/run-ended' }, ended));
    await page.waitForTimeout(100);
    return status();
  };

  // SIMPLE prints nothing. The console stays a tab until a click.
  const quiet = await endWith({ outcome: 'success', count: 3 });
  const tabOnly = await page.evaluate(() => !document.querySelector('[data-testid="console-pane"]')
    && !!document.querySelector('[data-testid="console-show"]'));
  check('a quiet run leaves the console as its tab', quiet === null && tabOnly,
    JSON.stringify({ quiet, tabOnly }));
  await page.click('[data-testid="console-show"]');
  await page.waitForTimeout(100);
  const counted = await status();
  check('a backend run that ends with a count shows "ok, 3 result(s)"',
    counted === 'ok, 3 result(s)', counted);

  const exited = await endWith({ outcome: 'error', exitCode: 1 });
  check('a subprocess run shows its exit code', exited === 'exited 1', exited);

  const bare = await endWith({ outcome: 'success' });
  check('a backend run with no count shows "ok"', bare === 'ok', bare);
}

async function modelSelection(page, check) {
  await h.sendFlow(page, SIMPLE);
  await h.waitForNodes(page, SIMPLE_IDS);
  const before = await h.postedCount(page);
  await page.click(`.react-flow__node[data-id="${IDS.buffer}"]`);
  const picked = await page.waitForFunction((b) =>
    window.__posted.slice(b).some((m) => m && m.type === 'cg/select-model'
                                        && m.model === 'simple-buffer-model'), before)
    .then(() => true).catch(() => false);
  check('a plain click on a node posts cg/select-model with its model', picked,
    JSON.stringify(await h.postedSince(page, before)));

  const beforeLoader = await h.postedCount(page);
  await page.click(`.react-flow__node[data-id="${IDS.loader}"]`);
  await page.waitForTimeout(300);
  const loaderPicks = (await h.postedSince(page, beforeLoader))
    .filter((m) => m && m.type === 'cg/select-model');
  check('a click on a loader card posts no cg/select-model', loaderPicks.length === 0,
    JSON.stringify(loaderPicks));

  await h.send(page, { type: 'cg/run-model', model: 'simple-buffer-model' });
  const label = await page.waitForFunction(() => {
    const b = document.querySelector('button.cg-run-btn');
    return b && b.textContent === 'Run simple-buffer-model';
  }).then(() => true).catch(() => false);
  check('cg/run-model names the model on the Run button', label,
    await page.$eval('button.cg-run-btn', (b) => b.textContent));
  const marked = await page.waitForFunction((pid) =>
    !!document.querySelector(`.react-flow__node[data-id="${pid}"] .cg-node.run-model`), IDS.buffer)
    .then(() => true).catch(() => false);
  check('the nodes of that model carry .run-model', marked, await h.nodeClass(page, IDS.buffer));
}

// The box of an element in flow coordinates.
const flowBox = (page, selector) => page.evaluate((sel) => {
  const el = document.querySelector(sel);
  const vp = document.querySelector('.react-flow__viewport');
  if (!el || !vp) return null;
  const m = new DOMMatrixReadOnly(getComputedStyle(vp).transform);
  const o = document.querySelector('.react-flow').getBoundingClientRect();
  const r = el.getBoundingClientRect();
  const fx = (x) => (x - o.x - m.e) / m.a;
  const fy = (y) => (y - o.y - m.f) / m.a;
  return { x: fx(r.x), y: fy(r.y), right: fx(r.right), bottom: fy(r.bottom),
           cx: fx(r.x + r.width / 2), cy: fy(r.y + r.height / 2) };
}, selector);

const node = (id) => `.react-flow__node[data-id="${id}"]`;

// The points of an edge path, in flow coordinates.
const edgePoints = (page, id) => page.evaluate((eid) => {
  const p = document.querySelector(`.react-flow__edge[data-id="${eid}"] path.react-flow__edge-path`);
  if (!p) return null;
  return (p.getAttribute('d').match(/-?[\d.]+,-?[\d.]+/g) || [])
    .map((t) => t.split(',').map(parseFloat));
}, id);

const edgeEnd = (page, id) => page.evaluate((eid) => {
  const p = document.querySelector(`.react-flow__edge[data-id="${eid}"] path.react-flow__edge-path`);
  if (!p) return null;
  const pt = p.getPointAtLength(p.getTotalLength());
  return [pt.x, pt.y];
}, id);

const portRows = (page, id) => page.evaluate((sel) =>
  Array.from(document.querySelectorAll(sel + ' .cg-side-row')).map((row) => {
    const text = (c) => { const el = row.querySelector(c); return el ? el.textContent : null; };
    const wire = row.querySelector('.cg-wire');
    return { name: text('.cg-side-name'), value: text('.cg-side-val'), label: text('.cg-wire-label'),
             wire: !!wire, frameIn: !!wire && wire.classList.contains('frame-in') };
  }), node(id));

async function threadFrames(page, check) {
  const ids = [IDS.loader, IDS.limits, IDS.frame, IDS.frameBuffer, IDS.frameCheck, IDS.opFrame, IDS.opCheck];
  await h.sendFlow(page, THREAD);
  await h.waitForNodes(page, ids);
  await h.settle(page);
  check('a thread draws one frame and its steps, with no card for the name: ' + ids.join(', '),
    JSON.stringify((await h.nodeIds(page)).sort()) === JSON.stringify(ids.slice().sort()),
    JSON.stringify(await h.nodeIds(page)));

  const head = await page.evaluate((sel) => {
    const f = document.querySelector(sel);
    return f && { title: f.querySelector('.cg-thread-title').textContent,
                  tags: Array.from(f.querySelectorAll('.cg-tag')).map((t) => t.textContent),
                  ports: Array.from(f.querySelectorAll('.cg-thread-port')).map((t) => t.textContent) };
  }, node(IDS.frame));
  check('the frame title is the bound name "checked", with the tags table-> and def, and the ports parcels and result',
    !!head && head.title === 'checked' && head.tags.join() === 'table->,def' && head.ports.join() === 'parcels,result',
    JSON.stringify(head));

  const titles = await page.evaluate(() =>
    Array.from(document.querySelectorAll('.cg-head')).map((el) =>
      ({ text: el.textContent, cut: el.scrollWidth > el.clientWidth + 1 })));
  check('no card title has an arrow and no card title is cut',
    titles.length === 5 && titles.every((t) => !/[←→]/.test(t.text) && !t.cut), JSON.stringify(titles));

  const frame = await flowBox(page, node(IDS.frame));
  const src = await flowBox(page, node(IDS.frame) + ' .cg-thread-port.source');
  const s1 = await flowBox(page, node(IDS.frameBuffer));
  const s2 = await flowBox(page, node(IDS.frameCheck));
  const res = await flowBox(page, node(IDS.frame) + ' .cg-thread-port.result');
  const inOrder = !!(frame && src && s1 && s2 && res)
    && src.bottom <= s1.y && s1.bottom <= s2.y && s2.bottom <= res.y
    && [s1, s2].every((b) => b.x >= frame.x && b.right <= frame.right && b.bottom <= frame.bottom);
  check('the steps are inside the frame, in source order, between the source port and the result port',
    inOrder, JSON.stringify({ frame, src, s1, s2, res }));
  check('the source port, the steps and the result port are on one vertical line',
    !!(src && s1 && s2 && res) && [s1, s2, res].every((b) => h.near(b.cx, src.cx, 2)),
    JSON.stringify({ src: src && src.cx, s1: s1 && s1.cx, s2: s2 && s2.cx, res: res && res.cx }));

  const line = await page.evaluate((fid) =>
    [0, 1, 2].map((i) => {
      const g = document.querySelector(`.react-flow__edge[data-id="${fid}__line-${i}"]`);
      const p = g && g.querySelector('path.react-flow__edge-path');
      return p ? { cls: g.getAttribute('class'), width: getComputedStyle(p).strokeWidth } : null;
    }), IDS.frame);
  check('the thread line has 3 segments, 4px wide, with the class cg-thread-line mode-table',
    line.every((l) => l && /cg-thread-line/.test(l.cls) && /mode-table/.test(l.cls) && l.width === '4px'),
    JSON.stringify(line));

  const srcEnd = await edgeEnd(page, 'src__parcels->' + IDS.frame);
  check('the source line ends at the top of the frame, above the source port',
    !!(srcEnd && frame && src) && h.near(srcEnd[0], src.cx, 2) && h.near(srcEnd[1], frame.y, 2),
    JSON.stringify({ srcEnd, frame, src }));

  const bufferRows = await portRows(page, IDS.frameBuffer);
  check('a literal arg of a step with no known signature is one row with the value and no wire',
    JSON.stringify(bufferRows) === JSON.stringify([{ name: null, value: '0.5', label: null, wire: false, frameIn: false }]),
    JSON.stringify(bufferRows));

  const checkRows = await portRows(page, IDS.frameCheck);
  check('each parameter is one row: "schema" wired to limits, and the literal map open to its keys',
    JSON.stringify(checkRows) === JSON.stringify([
      { name: 'schema', value: null, label: 'limits', wire: true, frameIn: false },
      { name: ':name', value: '"a"', label: null, wire: false, frameIn: false },
      { name: ':kind', value: ':fill', label: null, wire: false, frameIn: false }]),
    JSON.stringify(checkRows));

  const side = await page.evaluate((eid) => {
    const g = document.querySelector(`.react-flow__edge[data-id="${eid}"]`);
    const p = g && g.querySelector('path.react-flow__edge-path');
    return p ? { cls: g.getAttribute('class'), width: getComputedStyle(p).strokeWidth } : null;
  }, 'oa-limits-' + IDS.frameCheck + '-in-0');
  const sidePts = await edgePoints(page, 'oa-limits-' + IDS.frameCheck + '-in-0');
  const sideEnd = sidePts && sidePts[sidePts.length - 1];
  const row0 = await flowBox(page, node(IDS.frameCheck) + ' .cg-side-row');
  check('the side line from limits is thin (1.5px) and ends at the left edge of the frame, at its port row',
    !!(side && sideEnd && frame && row0) && /cg-side-line/.test(side.cls) && side.width === '1.5px'
      && h.near(sideEnd[0], frame.x, 2) && h.near(sideEnd[1], row0.cy, 2),
    JSON.stringify({ side, sideEnd, frame, row0 }));
  check('the side line turns only outside the frame',
    !!(sidePts && frame) && sidePts.slice(0, -1).every((pt) => pt[0] < frame.x || pt[1] < frame.y),
    JSON.stringify({ sidePts, frame }));

  const wire = await flowBox(page, node(IDS.frameCheck) + ' .cg-wire');
  check('the wire of the port runs from the left edge of the frame to the left edge of the step',
    !!(wire && frame && s2) && h.near(wire.x, frame.x, 2) && h.near(wire.right, s2.x, 2),
    JSON.stringify({ wire, frame, s2 }));

  const opRows = await portRows(page, IDS.opCheck);
  check('in an op frame, an input of the op is a frame port on the wire, and a card of the file is a line',
    JSON.stringify(opRows) === JSON.stringify([
      { name: 'schema', value: null, label: 'extra', wire: true, frameIn: true },
      { name: 'style', value: null, label: 'limits', wire: true, frameIn: false }]),
    JSON.stringify(opRows));
  const opSide = await page.evaluate((ids) =>
    ids.map((eid) => !!document.querySelector(`.react-flow__edge[data-id="${eid}"]`)),
    ['oa-limits-' + IDS.opCheck + '-in-1', 'oa-extra-' + IDS.opCheck + '-in-0']);
  check('only the card of the file gets a line into the op frame', opSide[0] && !opSide[1], JSON.stringify(opSide));

  const fits = await page.evaluate(() =>
    Array.from(document.querySelectorAll('.react-flow__node .cg-node.in-thread')).map((card) => {
      const box = card.getBoundingClientRect();
      const inner = Array.from(card.querySelectorAll('.cg-head, .cg-sub, .cg-side-name, .cg-side-val, .cg-stat'))
        .map((el) => el.getBoundingClientRect());
      return inner.every((r) => r.x >= box.x - 0.5 && r.right <= box.right + 0.5
        && r.y >= box.y - 0.5 && r.bottom <= box.bottom + 0.5);
    }));
  check('the text of each step is inside its card', fits.length === 3 && fits.every((x) => x), JSON.stringify(fits));

  const idle = await page.evaluate(() => document.querySelectorAll('.cg-stat').length);
  check('a card that did not run shows no status line', idle === 0, String(idle));
  const before = await flowBox(page, node(IDS.frameCheck));
  await h.sendTrace(page, { kind: 'proc-start', pid: IDS.frameCheck });
  await page.waitForFunction((sel) => !!document.querySelector(sel + ' .cg-node.running .cg-stat'), node(IDS.frameCheck));
  const during = await flowBox(page, node(IDS.frameCheck));
  const stat = await flowBox(page, node(IDS.frameCheck) + ' .cg-stat');
  check('a running step shows "running" inside its card, and the card keeps its size',
    (await h.statText(page, IDS.frameCheck)) === 'running' && !!(before && during && stat)
      && h.near(before.bottom - before.y, during.bottom - during.y, 0.5)
      && stat.right <= during.right && stat.bottom <= during.bottom && stat.y >= during.y,
    JSON.stringify({ before, during, stat }));
}

// The host writes the edit back to the .cg file. The message has exactly
// these keys.
function isDragNode(m, model, nodeId) {
  return !!(m && m.type === 'cg/edit' && m.editType === 'DragNode' && m.model === model
    && m.payload && m.payload.nodeId === nodeId && m.payload.position
    && typeof m.payload.position.x === 'number' && typeof m.payload.position.y === 'number'
    && Object.keys(m).sort().join(',') === 'editType,model,payload,type'
    && Object.keys(m.payload).sort().join(',') === 'nodeId,position');
}

// React Flow gives the position of a thread step relative to its thread
// frame, its parent.
async function inFrame(page, id) {
  const f = await h.nodePos(page, IDS.simpleHeader);
  const n = await h.nodePos(page, id);
  return f && n ? { x: n.x - f.x, y: n.y - f.y } : null;
}

async function threadStepDrag(page, check) {
  const model = 'simple-buffer-model';
  await h.sendFlow(page, SIMPLE);
  await h.waitForNodes(page, [IDS.loader, IDS.buffer, IDS.simplify]);
  await h.settle(page);

  const start = await h.nodePos(page, IDS.simplify);
  await h.press(page, IDS.simplify, 80, 60);
  const midDrag = await h.edits(page);
  await page.mouse.up();
  await h.waitForEditCount(page, 1);
  const settled = await h.nodePos(page, IDS.simplify);
  const settledIn = await inFrame(page, IDS.simplify);
  const edits = await h.edits(page);
  check('a drag moves the node', h.moved(start, settled),
    JSON.stringify({ start, settled }));
  check('nothing posts before the mouse is released', midDrag.length === 0, JSON.stringify(midDrag));
  check('the release posts exactly one DragNode for the dragged node',
    edits.length === 1 && isDragNode(edits[0], model, IDS.simplify), JSON.stringify(edits));
  const pos = edits[0] && edits[0].payload && edits[0].payload.position;
  check('the posted position is where the node settled in its thread frame',
    !!(pos && settledIn && h.near(pos.x, settledIn.x, 0.75) && h.near(pos.y, settledIn.y, 0.75)),
    JSON.stringify({ pos, settledIn }));

  await h.sendTrace(page, { kind: 'proc-start', pid: IDS.loader });
  const otherRunning = await page.waitForFunction((id) => {
    const el = document.querySelector(`.react-flow__node[data-id="${id}"] .cg-node`);
    return !!(el && el.classList.contains('running'));
  }, IDS.loader, { timeout: 5000 }).then(() => true).catch(() => false);
  const afterTrace = await h.nodePos(page, IDS.simplify);
  check('a trace event for another node does not move the dragged node back',
    !!(afterTrace && settled && h.near(afterTrace.x, settled.x, 0.75)
       && h.near(afterTrace.y, settled.y, 0.75)),
    JSON.stringify({ settled, afterTrace }));
  check('the other node becomes .running', otherRunning, await h.nodeClass(page, IDS.loader));

  await h.drag(page, IDS.simplify, 50, 35);
  await h.waitForEditCount(page, 2);
  const edits2 = await h.edits(page);
  const settled2 = await h.nodePos(page, IDS.simplify);
  const settled2In = await inFrame(page, IDS.simplify);
  const pos2 = edits2[1] && edits2[1].payload && edits2[1].payload.position;
  check('a second drag during the run posts a second DragNode at the new position',
    edits2.length === 2 && isDragNode(edits2[1], model, IDS.simplify) && h.moved(settled, settled2)
      && !!(pos2 && h.near(pos2.x, settled2In.x, 0.75) && h.near(pos2.y, settled2In.y, 0.75)),
    JSON.stringify({ edits: edits2, settled2In }));

  const stored = await inFrame(page, IDS.buffer);
  check('a step with a stored position is drawn at exactly [400, 250] in its thread frame',
    !!(stored && h.near(stored.x, 400, 0.01) && h.near(stored.y, 250, 0.01)), JSON.stringify(stored));
}

async function opBodyDrag(page, check) {
  const { call, body1, body2 } = IDS;
  await h.sendFlow(page, opFlow());
  await h.waitForNodes(page, [IDS.loader, call]);
  await h.settle(page);

  const chevron = `.react-flow__node[data-id="${call}"] .cg-defop-chevron`;
  const collapsed = await page.evaluate(([c, b1, b2]) => {
    const q = (id) => document.querySelector(`.react-flow__node[data-id="${id}"]`);
    const chev = document.querySelector(c);
    return !q(b1) && !q(b2) && !!chev && !chev.classList.contains('expanded');
  }, [chevron, body1, body2]);
  check('a call site starts collapsed: no body steps, chevron not expanded', collapsed);

  await page.click(chevron);
  await page.waitForFunction(([c, b1, b2]) => {
    const q = (id) => document.querySelector(`.react-flow__node[data-id="${id}"]`);
    const chev = document.querySelector(c);
    return !!(q(b1) && q(b2) && chev && chev.classList.contains('expanded'));
  }, [chevron, body1, body2]);

  const card = await page.evaluate((id) => {
    const w = document.querySelector(`.react-flow__node[data-id="${id}"]`);
    const inner = w && w.querySelector('.cg-node');
    const head = w && w.querySelector('.cg-head');
    const sub = w && w.querySelector('.cg-sub');
    return { cls: inner ? inner.className : '', head: head ? head.textContent : null,
             sub: sub ? sub.textContent : null };
  }, call);
  check('the call site is a .defop-call card titled "prepare-parcel" over its qualifier "parcel"',
    card.cls.split(/\s+/).includes('defop-call') && card.head === 'prepare-parcel' && card.sub === 'parcel',
    JSON.stringify(card));

  // On expand React Flow clamps the children against the parent height of the
  // last frame. The first paint can put them too high. Wait for the layout.
  const rel = async (id) => {
    const p = await h.nodePos(page, call);
    const c = await h.nodePos(page, id);
    return p && c ? { x: c.x - p.x, y: c.y - p.y } : null;
  };
  await page.waitForFunction(([c, b1, b2]) => {
    const t = (id) => {
      const w = document.querySelector(`.react-flow__node[data-id="${id}"]`);
      const m = w && /translate\(\s*(-?[\d.]+)px\s*,?\s*(-?[\d.]+)px/.exec(w.style.transform || '');
      return m ? { x: parseFloat(m[1]), y: parseFloat(m[2]) } : null;
    };
    const p = t(c), s1 = t(b1), s2 = t(b2);
    return !!(p && s1 && s2 && Math.abs(s1.x - p.x - 40) <= 1 && Math.abs(s1.y - p.y - 100) <= 1
      && Math.abs(s2.x - p.x - 40) <= 1 && Math.abs(s2.y - p.y - 190) <= 1);
  }, [call, body1, body2], { timeout: 5000 }).catch(() => {});
  const r1 = await rel(body1);
  const r2 = await rel(body2);
  check('the body steps sit inside the call site at (40, 100) and (40, 190)',
    !!(r1 && r2 && h.near(r1.x, 40) && h.near(r1.y, 100) && h.near(r2.x, 40) && h.near(r2.y, 190)),
    JSON.stringify({ r1, r2 }));

  const ids = await h.nodeIds(page);
  check('the canvas has 5 nodes, with body steps named <op>-body__<step>',
    ids.length === 5 && [IDS.loader, IDS.opHeader, call, body1, body2].every((id) => ids.includes(id)),
    JSON.stringify(ids));

  const before = await h.nodePos(page, body1);
  await h.drag(page, body1, 40, 25);
  await h.waitForEditCount(page, 1);
  const after = await h.nodePos(page, body1);
  check('a drag moves a body step', h.moved(before, after), JSON.stringify({ before, after }));

  const stepEdit = (await h.edits(page))[0];
  check('the body-step drag posts a DragNode of model prepare-parcel-body',
    isDragNode(stepEdit, 'prepare-parcel-body', body1), JSON.stringify(stepEdit));

  const parent = await h.nodePos(page, call);
  const stepPos = stepEdit && stepEdit.payload && stepEdit.payload.position;
  const inParent = after && parent ? { x: after.x - parent.x, y: after.y - parent.y } : null;
  check('the posted body-step position is relative to the call site',
    !!(stepPos && inParent && h.near(stepPos.x, inParent.x) && h.near(stepPos.y, inParent.y)
       && Math.abs(stepPos.y - after.y) > 50),
    JSON.stringify({ stepPos, inParent, absolute: after }));

  // Grip the card 20 flow px below its top edge: the body steps start at 100.
  const z = await h.zoom(page);
  const box = await (await page.waitForSelector(`.react-flow__node[data-id="${call}"]`)).boundingBox();
  await h.drag(page, call, 35, 30, { x: box.width * 0.5, y: 20 * z });
  await h.waitForEditCount(page, 2);
  const callEdit = (await h.edits(page))[1];
  const parent2 = await h.nodePos(page, call);
  const frame2 = await h.nodePos(page, IDS.opHeader);
  const r2After = await rel(body2);
  const callPos = callEdit && callEdit.payload && callEdit.payload.position;
  check('a call-site drag posts its position in its thread frame, and its body steps move with it',
    isDragNode(callEdit, 'parcel-model', call) && h.moved(parent, parent2)
      && !!(callPos && frame2 && h.near(callPos.x, parent2.x - frame2.x) && h.near(callPos.y, parent2.y - frame2.y))
      && !!(r2After && h.near(r2After.x, 40) && h.near(r2After.y, 190)),
    JSON.stringify({ callEdit, parent2, frame2, r2After }));
}

const esc = (s) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
const openToolbar = async (page) => {
  if (await page.$('.cg-toolbar[data-open="false"]')) await page.click('.cg-toolbar-toggle');
  await page.waitForSelector('.cg-toolbar-tab', { timeout: 5000 });
};
const tab = (page, label) => ({
  click: async () => {
    await openToolbar(page);
    await page.locator('.cg-toolbar-tab')
      .filter({ hasText: new RegExp('^' + esc(label) + ' \\(') }).first().click();
  },
});
const op = (page, name) => page.locator('.cg-toolbar-op')
  .filter({ hasText: new RegExp('^' + esc(name) + '$') });
const opStrip = (page) => page.$$eval('.cg-toolbar-op', (els) =>
  els.map((e) => ({ text: e.textContent, title: e.title, draggable: e.getAttribute('draggable') })));
const ghostIds = async (page) => (await h.nodeIds(page)).filter((id) => id && id.startsWith('ghost-'));
// A body step sits in the box of its call site. Only top-level cards count.
const ghostPlacement = (page, gid) => page.evaluate((id) => {
  const g = document.querySelector(`.react-flow__node[data-id="${id}"]`);
  if (!g) return null;
  const pane = document.querySelector('.react-flow').getBoundingClientRect();
  const r = g.getBoundingClientRect();
  const hit = (a, b) => a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom;
  const others = Array.from(document.querySelectorAll('.react-flow__node'))
    .filter((n) => n !== g && !(n.getAttribute('data-id') || '').includes('__'));
  return {
    overlaps: others.filter((n) => hit(r, n.getBoundingClientRect())).map((n) => n.getAttribute('data-id')),
    inView: r.left >= pane.left && r.right <= pane.right && r.top >= pane.top && r.bottom <= pane.bottom,
  };
}, gid);

async function tabsAndGhosts(page, check) {
  await h.sendFlow(page, opFlow({ label: 'Parcel' }));
  await h.waitForNodes(page, [IDS.loader]);
  await h.settle(page);

  const shut = await page.evaluate(() => ({
    toggle: (document.querySelector('.cg-toolbar-toggle') || {}).textContent,
    tags: document.querySelectorAll('.cg-toolbar-tab').length,
    ops: document.querySelectorAll('.cg-toolbar-op').length,
  }));
  check('the toolbar starts closed: one Browse ops button, no tags, no ops',
    shut.toggle === 'Browse ops' && shut.tags === 0 && shut.ops === 0, JSON.stringify(shut));

  await openToolbar(page);
  const tabs = await page.$$eval('.cg-toolbar-tab', (els) => els.map((e) => e.textContent));
  // I/O lists 3: write-geoparquet is not a registered op (op_catalog.cljs).
  const builtins = ['Geo (21)', 'Dataset (9)', 'Feature (3)', 'I/O (3)', 'Threading (4)'];
  check('the toolbar has the built-in tabs with their op counts',
    builtins.every((t) => tabs.includes(t)), JSON.stringify(tabs));
  check('the module label gives a "Parcel (1)" tab after the built-in tabs',
    tabs.indexOf('Parcel (1)') >= builtins.length, JSON.stringify(tabs));

  const closed = (await opStrip(page)).length === 0;
  await tab(page, 'Geo').click();
  let strip = await opStrip(page);
  check('the op strip starts closed, and the Geo tab opens its 21 draggable ops',
    closed && strip.length === 21 && strip.some((o) => o.text === 'geo/buffer')
      && strip.every((o) => o.draggable === 'true'
        && o.title.includes('click or drag onto the canvas')),
    JSON.stringify(strip.map((o) => o.text)));

  await tab(page, 'Parcel').click();
  strip = await opStrip(page);
  check('the Parcel tab shows the op prepare-parcel, "1 in / 1 out"',
    strip.length === 1 && strip[0].text === 'prepare-parcel' && strip[0].title.includes('1 in / 1 out'),
    JSON.stringify(strip));

  await tab(page, 'Threading').click();
  const threading = (await opStrip(page)).map((o) => o.text);
  check('the Threading tab shows item->, coll->, table-> and layers->',
    threading.length === 4 && ['item->', 'coll->', 'table->', 'layers->'].every((n) => threading.includes(n)),
    JSON.stringify(threading));

  await tab(page, 'Geo').click();
  strip = await opStrip(page);
  check('the Geo tab shows its 21 ops again',
    strip.length === 21 && strip.some((o) => o.text === 'geo/valid?'), String(strip.length));

  await tab(page, 'Geo').click();
  const reclosed = (await opStrip(page)).length;
  await tab(page, 'Geo').click();
  check('a second click on the open tab closes its ops, and a third opens them',
    reclosed === 0 && (await opStrip(page)).length === 21, String(reclosed));

  await op(page, 'geo/buffer').click();
  const appeared = await page.waitForSelector('.react-flow__node[data-id="ghost-1"]', { timeout: 5000 })
    .then(() => true).catch(() => false);
  await page.waitForTimeout(400);
  const g1 = await ghostPlacement(page, 'ghost-1');
  check('a click on an op drops ghost-1 on no card and in view',
    appeared && !!g1 && g1.overlaps.length === 0 && g1.inView, JSON.stringify(g1));

  const card = await page.evaluate(() => {
    const w = document.querySelector('.react-flow__node[data-id="ghost-1"]');
    const inner = w && w.querySelector('.cg-node');
    const cs = inner && getComputedStyle(inner);
    const text = (sel) => (w && w.querySelector(sel) ? w.querySelector(sel).textContent : null);
    return w && inner ? {
      wrapper: w.className, classes: Array.from(inner.classList), head: text('.cg-head'),
      stat: text('.cg-stat'), chevron: !!w.querySelector('.cg-defop-chevron'),
      padding: cs.paddingTop + ' ' + cs.paddingRight,
    } : null;
  });
  check('ghost-1 is a plain idle card: head geo/buffer, no status line, cg.css padding',
    !!card && card.wrapper.includes('react-flow__node-cg')
      && card.classes.includes('cg-node') && card.classes.includes('idle')
      && !card.classes.includes('defop-call') && !card.chevron
      && card.head === 'geo/buffer' && card.stat === null && card.padding === '8px 16px',
    JSON.stringify(card));

  const before = await h.nodePos(page, 'ghost-1');
  await h.drag(page, 'ghost-1', 70, 45);
  const after = await h.nodePos(page, 'ghost-1');
  const ghostEdits = await h.edits(page);
  check('a ghost moves on a drag and posts no edit',
    h.moved(before, after) && ghostEdits.length === 0, JSON.stringify({ before, after, ghostEdits }));

  await h.drag(page, IDS.loader, 80, 40);
  await h.waitForEditCount(page, 1);
  const edits = await h.edits(page);
  const e = edits[0];
  check('a parsed node still posts its DragNode',
    edits.length === 1 && e.editType === 'DragNode' && e.model === IDS.loader
      && e.payload && e.payload.nodeId === IDS.loader && h.moved(e.payload.position, { x: 0, y: 20 }),
    JSON.stringify(edits));

  await op(page, 'geo/area').click();
  const head2 = await page.waitForSelector('.react-flow__node[data-id="ghost-2"] .cg-head', { timeout: 5000 })
    .then((el) => el.textContent()).catch(() => null);
  await page.waitForTimeout(400);
  const g2 = await ghostPlacement(page, 'ghost-2');
  check('the next op drops ghost-2, headed geo/area, on no card and not on ghost-1',
    head2 === 'geo/area' && !!g2 && g2.overlaps.length === 0, JSON.stringify({ head2, g2 }));
}

async function dragDrop(page, check) {
  const [x, y] = [480, 320];
  await h.sendFlow(page, SIMPLE);
  await page.waitForFunction(() => document.querySelectorAll('.react-flow__node').length === 4);
  await h.settle(page);
  check('no ghost before the drop', (await ghostIds(page)).length === 0);
  const postedBefore = await h.postedCount(page);

  // Read the pane rect and the viewport in the same task as the drop. Then the
  // expected position cannot race the fit.
  const drop = await page.evaluate(({ cx, cy }) => {
    const pane = document.querySelector('.react-flow__pane');
    const rect = document.querySelector('.react-flow').getBoundingClientRect();
    const t = /translate\((-?[\d.e+]+)px,\s*(-?[\d.e+]+)px\)\s*scale\((-?[\d.e+]+)\)/
      .exec(document.querySelector('.react-flow__viewport').style.transform);
    const dt = new DataTransfer();
    dt.setData('application/cg-op', 'geo/buffer');
    for (const type of ['dragover', 'drop']) {
      pane.dispatchEvent(new DragEvent(type,
        { bubbles: true, cancelable: true, clientX: cx, clientY: cy, dataTransfer: dt }));
    }
    return { left: rect.left, top: rect.top, tx: +t[1], ty: +t[2], zoom: +t[3] };
  }, { cx: x, cy: y });

  await page.waitForSelector('.react-flow__node[data-id^="ghost-"]', { timeout: 5000 }).catch(() => {});
  const ghosts = await ghostIds(page);
  check('the drop adds one ghost, ghost-1', ghosts.length === 1 && ghosts[0] === 'ghost-1', JSON.stringify(ghosts));
  const head = await page.evaluate(() => {
    const el = document.querySelector('.react-flow__node[data-id="ghost-1"] .cg-head');
    return el ? el.textContent : null;
  });
  check('the ghost is headed geo/buffer', head === 'geo/buffer', String(head));

  const pos = await h.nodePos(page, 'ghost-1');
  const want = { x: (x - drop.left - drop.tx) / drop.zoom, y: (y - drop.top - drop.ty) / drop.zoom };
  check('the ghost sits at the flow position of the drop point',
    !!pos && h.near(pos.x, want.x, 2) && h.near(pos.y, want.y, 2), JSON.stringify({ pos, want }));

  await page.waitForTimeout(400);
  const posted = await h.postedSince(page, postedBefore);
  check('the drop posts nothing to the host', posted.length === 0, JSON.stringify(posted));
}

async function palette(page, check) {
  const overlays = () => page.evaluate(() => Array.from(document.querySelectorAll('div'))
    .filter((d) => { const s = getComputedStyle(d); return s.position === 'fixed' && s.zIndex === '1000'; })
    .length);
  const waitOverlays = (n) => page.waitForFunction((k) => Array.from(document.querySelectorAll('div'))
    .filter((d) => { const s = getComputedStyle(d); return s.position === 'fixed' && s.zIndex === '1000'; })
    .length === k, n, { timeout: 8000 }).then(() => true).catch(() => false);
  const input = 'input[placeholder="Search ops…"]';
  const inputValue = () => page.evaluate((sel) => {
    const i = document.querySelector(sel);
    return i ? i.value : null;
  }, input);
  const inputFocused = () => page.evaluate((sel) =>
    !!document.querySelector(sel) && document.activeElement === document.querySelector(sel), input);
  // Each result row is a div whose first span is the op name.
  const results = () => page.evaluate((sel) => {
    const i = document.querySelector(sel);
    const list = i && i.parentElement.children[1];
    return list ? Array.from(list.children)
      .filter((el) => el.tagName === 'DIV' && el.querySelector('span'))
      .map((el) => el.querySelector('span').textContent) : null;
  }, input);
  const subsequence = (q, name) => {
    let i = 0;
    for (const c of name.toLowerCase()) if (c === q[i]) i += 1;
    return i === q.length;
  };

  await h.sendFlow(page, SIMPLE);
  await page.waitForFunction(() => document.querySelectorAll('.react-flow__node').length === 4);
  await h.settle(page);
  await page.evaluate(() => document.activeElement && document.activeElement.blur());

  await page.keyboard.press('/');
  check("'/' opens the palette", await waitOverlays(1));
  check('the palette search field has the focus', await inputFocused());

  const all = await results();
  await page.keyboard.type('buf');
  await page.waitForFunction((sel) => document.querySelector(sel).value === 'buf', input);
  const buf = await results();
  check("'buf' keeps only fuzzy subsequence matches",
    !!all && !!buf && buf.length > 0 && buf.length < all.length && buf.every((n) => subsequence('buf', n)),
    JSON.stringify(buf));
  check("the 'buf' results have geo/buffer and none of the ops that do not match",
    buf.includes('geo/buffer') && ['geo/area', 'ds/filter', 'item->', 'geo/simplify'].every((n) => !buf.includes(n)),
    JSON.stringify(buf));

  const before = await h.nodeIds(page);
  await page.keyboard.press('Enter');
  const dropped = await page.waitForFunction(() => {
    const el = document.querySelector('.react-flow__node[data-id="ghost-1"] .cg-head');
    return !!el && el.textContent === 'geo/buffer';
  }, null, { timeout: 5000 }).then(() => true).catch(() => false);
  const afterEnter = await h.nodeIds(page);
  check('Enter drops geo/buffer as ghost-1', dropped && afterEnter.length === before.length + 1,
    JSON.stringify(afterEnter));
  check('the palette closes after Enter', await waitOverlays(0));

  await page.keyboard.press('/');
  const reopened = (await waitOverlays(1)) && (await inputFocused());
  await page.keyboard.press('Escape');
  const closed = await waitOverlays(0);
  const afterEscape = await h.nodeIds(page);
  check('Escape closes the palette and drops nothing',
    reopened && closed && afterEscape.length === afterEnter.length, JSON.stringify(afterEscape));

  // Select ghost-1 first: with the input guard broken, Backspace deletes it.
  await page.click('.react-flow__node[data-id="ghost-1"]');
  const selected = (await h.nodeClass(page, 'ghost-1') || '').includes('selected');
  await page.keyboard.press('/');
  const open = (await waitOverlays(1)) && (await inputFocused());
  await page.keyboard.press('/');
  const slash = (await inputValue()) === '/';
  const one = (await overlays()) === 1;
  await page.keyboard.press('Backspace');
  const empty = (await inputValue()) === '';
  await page.waitForTimeout(200);
  const kept = (await h.nodeIds(page)).includes('ghost-1');
  check("in the search field, '/' and Backspace type text: one palette, no node deleted",
    selected && open && slash && one && empty && kept,
    JSON.stringify({ selected, open, slash, one, empty, kept }));
  await page.keyboard.press('Escape');
}

async function opGhost(page, check) {
  const { call, body1, body2 } = IDS;
  const ghostBody = ['ghost-1__buffer-1', 'ghost-1__simplify-2'];
  await h.sendFlow(page, opFlow());
  await h.waitForNodes(page, [IDS.loader, call]);
  await h.settle(page);
  check('the parsed call site starts collapsed',
    !(await h.hasNode(page, body1)) && !(await h.hasNode(page, body2)));

  // Expand it: the body steps of the ghost then sit next to real body steps.
  await page.click(`.react-flow__node[data-id="${call}"] .cg-defop-chevron`);
  await h.waitForNodes(page, [body1, body2]);

  // With no module label the op goes under the "Ops" tab.
  await tab(page, 'Ops').click();
  await h.withRandom(page, 0, () =>
    page.locator('.cg-toolbar-op', { hasText: 'prepare-parcel' }).first().click());
  const card = await page.waitForSelector('.react-flow__node[data-id="ghost-1"]', { timeout: 5000 })
    .then(() => page.evaluate(() => {
      const w = document.querySelector('.react-flow__node[data-id="ghost-1"]');
      return { cls: w.querySelector('.cg-node').className, head: w.querySelector('.cg-head').textContent };
    })).catch(() => null);
  // The canvas pans to a ghost that lands outside the view, over 250 ms. The
  // rects and the drags below need the view to hold still.
  await h.settle(page);
  check('the op ghost is a .defop-call card headed "prepare-parcel · 1 in / 1 out"',
    !!card && card.cls.split(/\s+/).includes('defop-call') && card.head === 'prepare-parcel · 1 in / 1 out',
    JSON.stringify(card));

  const counts = await page.evaluate((ids) => {
    const out = {};
    for (const id of ids.nodes) out[id] = document.querySelectorAll(`.react-flow__node[data-id="${id}"]`).length;
    for (const id of ids.edges) {
      out['edge ' + id] = document.querySelectorAll(
        `.react-flow__edge[data-id="${id}"], [data-testid="rf__edge-${id}"]`).length;
    }
    return out;
  }, { nodes: [...ghostBody, body1, body2],
       edges: ['ghost-1__body-entry', 'ghost-1__body-e0-buffer-1-simplify-2', 'ghost-1__body-exit'] });
  check('the ghost body steps render once each, next to the real body steps',
    [...ghostBody, body1, body2].every((id) => counts[id] === 1), JSON.stringify(counts));
  check('the ghost body edges render (entry, step, exit)',
    Object.keys(counts).filter((k) => k.startsWith('edge ')).every((k) => counts[k] >= 1), JSON.stringify(counts));

  const rects = await page.evaluate((ids) => {
    const r = (id) => {
      const el = document.querySelector(`.react-flow__node[data-id="${id}"]`);
      const b = el && el.getBoundingClientRect();
      return b ? { x: b.x, y: b.y, w: b.width, h: b.height } : null;
    };
    return { parent: r('ghost-1'), b1: r(ids[0]), b2: r(ids[1]) };
  }, ghostBody);
  const z = await h.zoom(page);
  const { parent, b1, b2 } = rects;
  check('the ghost body steps sit inside the ghost at (40, 100) and (40, 190)',
    !!(parent && b1 && b2) && h.near(b1.x - parent.x, 40 * z, 3) && h.near(b1.y - parent.y, 100 * z, 3)
      && h.near(b2.x - parent.x, 40 * z, 3) && h.near(b2.y - parent.y, 190 * z, 3)
      && [b1, b2].every((c) => c.y <= parent.y + parent.h && c.x <= parent.x + parent.w),
    JSON.stringify({ z, rects }));

  // Control: this drag simulation does move React Flow nodes. The empty result
  // of the ghost drags below then means something.
  let from = await h.postedCount(page);
  await h.drag(page, IDS.loader, 50, 30);
  const control = await h.postedSince(page, from);
  check('control: a drag of a parsed node posts a DragNode',
    control.some((m) => m && m.editType === 'DragNode' && m.payload && m.payload.nodeId === IDS.loader),
    JSON.stringify(control));

  // Grip the ghost by its head: the chevron and the body steps take clicks.
  from = await h.postedCount(page);
  const gbox = await page.locator('.react-flow__node[data-id="ghost-1"]').boundingBox();
  await h.drag(page, 'ghost-1', 40, 25, { x: gbox.width * 0.3, y: 12 });
  await h.drag(page, ghostBody[0], 15, 10);
  const ghostPosts = await h.postedSince(page, from);
  check('drags of the ghost and of a ghost body step post nothing', ghostPosts.length === 0, JSON.stringify(ghostPosts));

  from = await h.postedCount(page);
  const gbox2 = await page.locator('.react-flow__node[data-id="ghost-1"]').boundingBox();
  await page.mouse.click(gbox2.x + gbox2.width * 0.3, gbox2.y + 12);
  const selected = await page.waitForFunction(() => {
    const el = document.querySelector('.react-flow__node[data-id="ghost-1"] .cg-node');
    return !!el && el.className.split(/\s+/).includes('selected');
  }, null, { timeout: 3000 }).then(() => true).catch(() => false);
  check('a click selects the ghost', selected, await h.nodeClass(page, 'ghost-1'));

  await page.keyboard.press('Backspace');
  const removed = await page.waitForFunction(
    () => document.querySelectorAll('.react-flow__node[data-id^="ghost-"], .react-flow__edge[data-id^="ghost-1__"],'
      + ' [data-testid^="rf__edge-ghost-1__"]').length === 0,
    null, { timeout: 3000 }).then(() => true).catch(() => false);
  check('Backspace removes the ghost with its body steps and edges', removed, JSON.stringify(await ghostIds(page)));
  const kept = await Promise.all([IDS.loader, call, body1, body2].map((id) => h.hasNode(page, id)));
  check('the parsed nodes stay, the real body steps too', kept.every(Boolean), JSON.stringify(kept));
  const removePosts = await h.postedSince(page, from);
  check('the selection and the removal post nothing', removePosts.length === 0, JSON.stringify(removePosts));
}

const { call, body1, body2 } = IDS;
const GHOST_BODY = ['ghost-1__buffer-1', 'ghost-1__simplify-2'];
const EDGES = ['prepare-parcel-body__body-e0-buffer-1-simplify-2', call + '__body-entry', call + '__body-exit'];

async function cardState(page, id, bodyIds) {
  return page.evaluate(([nid, ids]) => {
    const w = document.querySelector(`.react-flow__node[data-id="${nid}"]`);
    const chev = w && w.querySelector('.cg-defop-chevron');
    const use = chev && chev.querySelector('svg use');
    return {
      chevron: chev ? Array.from(chev.classList) : null,
      chevronHref: use ? use.getAttribute('href') : null,
      width: w ? w.style.width : null,
      height: w ? w.style.height : null,
      body: ids.map((b) => !!document.querySelector(`.react-flow__node[data-id="${b}"]`)),
    };
  }, [id, bodyIds]);
}

async function edgeStates(page) {
  return page.evaluate((ids) => Object.fromEntries(ids.map((id) => {
    const el = document.querySelector(`.react-flow__edge[data-id="${id}"], [data-testid="rf__edge-${id}"]`);
    if (!el) return [id, 'absent'];
    const cs = getComputedStyle(el);
    return [id, cs.display === 'none' || cs.visibility === 'hidden' ? 'hidden' : 'visible'];
  })), EDGES);
}

async function waitExpanded(page, id, bodyIds, expanded) {
  return page.waitForFunction(([nid, ids, want]) => {
    const chev = document.querySelector(`.react-flow__node[data-id="${nid}"] .cg-defop-chevron`);
    const present = ids.map((b) => !!document.querySelector(`.react-flow__node[data-id="${b}"]`));
    return !!chev && chev.classList.contains('expanded') === want && present.every((p) => p === want);
  }, [id, bodyIds, expanded], { timeout: 5000 }).then(() => true).catch(() => false);
}

async function chevronAndIcons(page, check) {
  // What each chevron click posts; all of it must be empty.
  const chevronPosts = [];
  const clickChevron = async (id) => {
    const from = await h.postedCount(page);
    await page.click(`.react-flow__node[data-id="${id}"] .cg-defop-chevron`);
    await page.waitForTimeout(350);
    chevronPosts.push(...(await h.postedSince(page, from)));
  };

  const flow = opFlow({ label: 'Parcel', span: true });
  await h.sendFlow(page, flow);
  await h.waitForNodes(page, [IDS.loader, call]);
  await h.settle(page);

  let st = await cardState(page, call, [body1, body2]);
  check('the call site has a chevron that uses #i-chevron-right',
    !!st.chevron && st.chevronHref === '#i-chevron-right', JSON.stringify(st));
  check('the call site starts collapsed: no body steps, chevron not expanded',
    st.body.every((p) => !p) && !st.chevron.includes('expanded'), JSON.stringify(st));
  check('a collapsed call site has no body-sized style',
    st.width !== '320px' && !(parseFloat(st.height || '') >= 190), JSON.stringify(st));
  const edgesCollapsed = await edgeStates(page);

  await clickChevron(call);
  const expanded = (await waitExpanded(page, call, [body1, body2], true))
    && await page.evaluate((id) => {
      const w = document.querySelector(`.react-flow__node[data-id="${id}"]`);
      return w.style.width === '320px' && parseFloat(w.style.height || '0') >= 190;
    }, call);
  check('a chevron click expands the call site: body steps, "expanded", body-sized style',
    expanded, JSON.stringify(await cardState(page, call, [body1, body2])));

  const bodyEdge = await page.waitForFunction((id) => {
    const el = document.querySelector(`.react-flow__edge[data-id="${id}"], [data-testid="rf__edge-${id}"]`);
    const cs = el && getComputedStyle(el);
    return !!cs && cs.display !== 'none' && cs.visibility !== 'hidden';
  }, EDGES[0], { timeout: 5000 }).then(() => true).catch(() => false);
  check('the body edges are not shown while collapsed, and the step edge shows when expanded',
    Object.values(edgesCollapsed).every((s) => s !== 'visible') && bodyEdge,
    JSON.stringify({ collapsed: edgesCollapsed, expanded: await edgeStates(page) }));

  await clickChevron(call);
  check('a second chevron click collapses the call site',
    await waitExpanded(page, call, [body1, body2], false), JSON.stringify(await cardState(page, call, [body1, body2])));

  // Control: a Cmd-click on the card posts cg/open-defop, because the call site
  // has a full span. The empty chevron posts then mean that the chevron stops
  // the click.
  const from = await h.postedCount(page);
  const box = await (await page.waitForSelector(`.react-flow__node[data-id="${call}"]`)).boundingBox();
  await page.keyboard.down('Meta');
  await page.mouse.click(box.x + box.width * 0.4, box.y + box.height * 0.6);
  await page.keyboard.up('Meta');
  await page.waitForFunction((i) => window.__posted.slice(i).some((m) => m && m.type === 'cg/open-defop'),
    from, { timeout: 3000 }).catch(() => {});
  const open = (await h.postedSince(page, from)).find((m) => m && m.type === 'cg/open-defop');
  check('control: a Cmd-click on the card posts cg/open-defop at line 3',
    !!(open && open.location && open.location.startLine === 3), JSON.stringify(open));

  // Move the expanded call site off its layout position, then post the same
  // flow again: the layout reruns, and the call site stays expanded.
  await clickChevron(call);
  await h.waitForNodes(page, [body1]);
  const layout = await h.nodePos(page, call);
  const z = await h.zoom(page);
  const box2 = await (await page.waitForSelector(`.react-flow__node[data-id="${call}"]`)).boundingBox();
  await h.drag(page, call, 50, 35, { x: box2.width * 0.5, y: 20 * z });
  const dragged = await h.nodePos(page, call);
  await h.sendFlow(page, flow);
  const back = await page.waitForFunction(([id, p]) => {
    const w = document.querySelector(`.react-flow__node[data-id="${id}"]`);
    const m = w && /translate\(\s*(-?[\d.]+)px,\s*(-?[\d.]+)px\)/.exec(w.style.transform || '');
    return !!m && Math.abs(parseFloat(m[1]) - p.x) <= 1.5 && Math.abs(parseFloat(m[2]) - p.y) <= 1.5;
  }, [call, layout], { timeout: 8000 }).then(() => true).catch(() => false);
  await h.settle(page);
  st = await cardState(page, call, [body1, body2]);
  check('a new flow puts the dragged call site back at its layout position, still expanded',
    h.moved(layout, dragged) && back && st.body.every(Boolean) && st.chevron.includes('expanded'),
    JSON.stringify({ layout, dragged, back, st }));

  await clickChevron(call);
  await waitExpanded(page, call, [body1, body2], false);
  const editsBefore = (await h.edits(page)).length;
  const before = await h.nodePos(page, call);
  await h.drag(page, call, 45, 35);
  await h.waitForEditCount(page, editsBefore + 1);
  const e = (await h.edits(page))[editsBefore];
  const after = await h.nodePos(page, call);
  // The call site is a step of the parcel-model thread frame: its position is
  // relative to the frame.
  const frame = await h.nodePos(page, IDS.opHeader);
  check('a drag of the collapsed call site posts its DragNode, relative to its thread frame',
    !!(e && e.editType === 'DragNode' && e.model === 'parcel-model' && e.payload && e.payload.nodeId === call
       && after && frame && h.near(e.payload.position.x, after.x - frame.x)
       && h.near(e.payload.position.y, after.y - frame.y))
      && h.moved(before, after),
    JSON.stringify({ e, before, after, frame }));

  const icons = await page.evaluate(([loader, id]) => {
    const q = (nid, sel) => document.querySelector(`.react-flow__node[data-id="${nid}"] ${sel}`);
    const use = q(loader, '.cg-head svg use');
    return { loader: use ? use.getAttribute('href') : null,
             symbol: !!document.querySelector('#i-map'),
             callIcon: !!q(id, '.cg-head svg use') };
  }, [IDS.loader, call]);
  check('a geo/* node shows #i-map from the sprite, and the call site shows no icon',
    icons.loader === '#i-map' && icons.symbol && !icons.callIcon, JSON.stringify(icons));

  await page.click('.cg-toolbar-toggle');
  await page.locator('.cg-toolbar-tab').filter({ hasText: /^Parcel \(/ }).click();
  await h.withRandom(page, 0.5, () =>
    page.locator('.cg-toolbar-op').filter({ hasText: /^prepare-parcel$/ }).click());
  const ghostOpen = await waitExpanded(page, 'ghost-1', GHOST_BODY, true);
  const ghostBefore = await cardState(page, 'ghost-1', GHOST_BODY);
  await clickChevron('ghost-1');
  const ghostClosed = await waitExpanded(page, 'ghost-1', GHOST_BODY, false);
  check('an op ghost starts expanded, and its chevron collapses it',
    ghostOpen && ghostClosed, JSON.stringify(ghostBefore));

  check('no chevron click posts anything to the host (5 clicks)',
    chevronPosts.length === 0, JSON.stringify(chevronPosts));
}

// Each value is unique on the page. A value that stays after the switch can
// only come from the old token set.
const THEME_TOKENS = {
  dark: { bg: '#101418', accent: '#3aa0ff', surface: '#181d24' },
  light: { bg: '#fafaf0', accent: '#0a66cc', surface: '#fffff5' },
};
const THEME_CODE_TOKENS = {
  dark: { keyword: '#c586c0', string: '#ce9178' },
  light: { keyword: '#7a1f8f', string: '#a31515' },
};

const decls = (prefix, obj) => Object.entries(obj).map(([k, v]) => `--${prefix}${k}: ${v};`);

async function themeSwitch(page, check) {
  const tokensCss = () => page.evaluate(() => document.getElementById('cg-tokens').textContent);
  const theme = () => page.evaluate(() => document.documentElement.getAttribute('data-theme'));
  const setMode = async (mode) => {
    await h.send(page, { type: 'cg/theme-kind', mode });
    return page.waitForFunction((m) => document.documentElement.getAttribute('data-theme') === m,
      mode, { timeout: 10000 }).then(() => true).catch(() => false);
  };

  await h.sendFlow(page, SIMPLE);
  const drawn = await page.waitForFunction(() => document.querySelectorAll('.cg-node').length === 4)
    .then(() => true).catch(() => false);
  check('the flow is on the canvas during the switches', drawn);

  check('the page starts dark', (await theme()) === 'dark');
  check('#cg-tokens starts with the dark --cg-bg', (await tokensCss()).includes(`--cg-bg: ${THEME_TOKENS.dark.bg};`));

  check('cg/theme-kind light sets data-theme="light"', await setMode('light'), await theme());
  const light = await tokensCss();
  check('#cg-tokens has the light --cg-* values', decls('cg-', THEME_TOKENS.light).every((d) => light.includes(d)), light);
  const stale = [...Object.values(THEME_TOKENS.dark), ...Object.values(THEME_CODE_TOKENS.dark)].filter((v) => light.includes(v));
  check('no dark value stays in #cg-tokens', stale.length === 0, JSON.stringify(stale));
  check('#cg-tokens has the light --code-* values', decls('code-', THEME_CODE_TOKENS.light).every((d) => light.includes(d)), light);

  check('cg/theme-kind dark sets data-theme="dark" again', await setMode('dark'), await theme());
  const dark = await tokensCss();
  check('#cg-tokens has the dark --cg-* and --code-* values again',
    [...decls('cg-', THEME_TOKENS.dark), ...decls('code-', THEME_CODE_TOKENS.dark)].every((d) => dark.includes(d)), dark);
}

h.run([
  { name: 'thread frames and side inputs', body: threadFrames },
  { name: 'nodes and run states', body: nodesAndRunStates },
  { name: 'model selection', body: modelSelection },
  { name: 'rows progress', body: rowsProgress },
  { name: 'run end status', body: runEndStatus },
  { name: 'thread step drag', body: threadStepDrag },
  { name: 'op body drag', body: opBodyDrag },
  { name: 'toolbar tabs and ghosts', body: tabsAndGhosts },
  { name: 'drag-drop from the toolbar', body: dragDrop },
  { name: "the '/' palette", body: palette },
  { name: 'op ghost', body: opGhost },
  { name: 'chevron and icons', body: chevronAndIcons },
  { name: 'theme switch', tokens: THEME_TOKENS, codeTokens: THEME_CODE_TOKENS, body: themeSwitch },
]);
