#!/usr/bin/env node
// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception


import { resolve } from 'path';
import * as fs from 'fs';
import { fileURLToPath, pathToFileURL } from 'url';


// util.cljc takes its fs from globalThis.nodeFs when cg.mjs loads.
globalThis.nodeFs = fs;

const args = process.argv.slice(2);

async function initWasmTS() {
  // npm can hoist @wcohen/wasmts, so find it by module resolution.
  const wasmJsFile = fileURLToPath(import.meta.resolve('@wcohen/wasmts/dist/wasmts.js'));
  const wasmBinary = fs.readFileSync(wasmJsFile + '.wasm');

  globalThis.__filename = wasmJsFile;

  // The wasmts loader fetches its wasm by URL, and node cannot fetch a file.
  const originalFetch = globalThis.fetch;
  globalThis.fetch = function(url, ...args) {
    if (url && (url.endsWith('.wasm') || url.includes('wasmts.js.wasm'))) {
      return Promise.resolve({
        ok: true,
        arrayBuffer: () => Promise.resolve(
          wasmBinary.buffer.slice(
            wasmBinary.byteOffset,
            wasmBinary.byteOffset + wasmBinary.byteLength
          )
        )
      });
    }
    return originalFetch ? originalFetch(url, ...args) : Promise.reject(new Error('fetch not available'));
  };

  await import('@wcohen/wasmts/dist/wasmts.js');

  // The loader sets globalThis.wasmts when its wasm is ready, not on import.
  const deadline = Date.now() + 20000;
  while (!(globalThis.wasmts && globalThis.wasmts.geom)) {
    if (Date.now() > deadline) {
      throw new Error('wasmts failed to initialize - globalThis.wasmts.geom not available');
    }
    await new Promise((r) => setTimeout(r, 50));
  }
  return { jsFile: wasmJsFile, binary: wasmBinary };
}

function fail(label, error) {
  console.error(label, error.message);
  if (error.stack) {
    console.error(error.stack);
  }
  process.exit(1);
}

function showHelp() {
  console.error(`cg - the cg command line tool (Node.js)

COMMANDS:
  run <file> [--model NAME] [--timeout-ms N]
      Run a model of the file and print its result. The model is the last
      threading-macro binding of the file (item->, coll->, table->), or the
      binding that --model names. The default timeout is 60000 ms.

  show-model <file> [--format edn|json]
      Print the Flow Graph of each model of the file.

  generate <file> [--format edn|code]
      Write the .cg source again from the models of the file.

  validate <file>
      Make sure that code -> model -> code gives the code again.
      The exit code is 0 for a match and 1 for a difference.

EXAMPLES:
  cg run examples/simple.cg
  cg run examples/simple.cg --model buffered
  cg show-model examples/simple.cg`);
}

async function main() {
  if (args.length === 0 || args[0] === '--help' || args[0] === '-h' || args[0] === 'help') {
    showHelp();
    process.exit(args.length === 0 ? 1 : 0);
  }

  const wasmts = await initWasmTS();
  const cg = await import('../dist/cg.mjs');

  async function initializeCG(filePath) {
    await cg.dataset.init_arquero_BANG_();
    // show-model, generate and validate evaluate the file as global code,
    // where a load-module call cannot wait for its module.
    if (filePath && fs.existsSync(filePath)) {
      await cg.run.load_deps_BANG_(fs.readFileSync(filePath, 'utf8'));
    }
  }

  const command = args[0];
  const cmdArgs = args.slice(1);

  const opts = {};
  const positionalArgs = [];

  for (let i = 0; i < cmdArgs.length; i++) {
    if (cmdArgs[i].startsWith('--')) {
      const key = cmdArgs[i].substring(2);
      if (i + 1 < cmdArgs.length && !cmdArgs[i + 1].startsWith('--')) {
        const value = cmdArgs[++i];
        // A squint keyword is its name, so :timeout-ms reads "timeout-ms".
        opts[key] = key === 'timeout-ms' ? parseInt(value, 10) : value;
      } else {
        opts[key] = true;
      }
    } else {
      positionalArgs.push(cmdArgs[i]);
    }
  }

  const commands = {
    'run': ['Error running file:', async (filePath) => {
      await initializeCG();
      // A reprojection step needs the PROJ handler of the joint pool. A
      // worker cannot see the fetch shim of initWasmTS, so it gets the bytes.
      await cg.run.init_cg_workers_BANG_({
        'wasmts-js-url': pathToFileURL(wasmts.jsFile).href,
        'wasmts-wasm-binary': wasmts.binary,
      });
      return cg.cli.run_model_file(filePath, opts);
    }],
    'show-model': ['Error showing model:', async (filePath) => {
      await initializeCG(filePath);
      return cg.cli.show_model(filePath, opts) || 0;
    }],
    'generate': ['Error generating code:', async (filePath) => {
      await initializeCG(filePath);
      return cg.cli.generate(filePath, opts) || 0;
    }],
    'validate': ['Error validating file:', async (filePath) => {
      await initializeCG(filePath);
      return cg.cli.validate(filePath) ? 0 : 1;
    }],
  };

  if (!Object.hasOwn(commands, command)) {
    console.error('Error: Unknown command:', command);
    console.error('');
    showHelp();
    process.exit(1);
  }
  if (positionalArgs.length === 0) {
    console.error('Error: No file path provided');
    process.exit(1);
  }
  const [label, runCommand] = commands[command];
  const filePath = resolve(positionalArgs[0]);
  try {
    process.exit(await runCommand(filePath));
  } catch (error) {
    fail(label, error);
  }
}

main().catch(error => fail('Fatal error:', error));
