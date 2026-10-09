// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// cg: the public exports of the library.
//
// Hand-written, and the only .mjs under src/cg/ that is not squint output.
// `bb squint:clean` skips this file for that reason. Every export is a named
// export; this bundle has no default export.

export * from './util.mjs';
export * from './feature.mjs';
export * from './geo.mjs';
export * from './io.mjs';
export * from './model.mjs';
export * from './macros.mjs';
export * from './codegen.mjs';
export * from './introspect.mjs';
export * from './metadata.mjs';
export * from './dsl.mjs';
export * from 'cljc-flow/flow';
export { add_trace_listener, remove_trace_listener } from './trace_js.mjs';
export * from './run.mjs';
export * from './cli.mjs';
export * from './dataset.mjs';
// Two star exports supply each of these names, and ES modules drop such a
// name from the star exports with no error. The explicit export picks one.
export { write_geojson } from './io.mjs';
export { create_flow } from './run.mjs';

import * as util from './util.mjs';
import * as feature from './feature.mjs';
import * as geo from './geo.mjs';
import * as io from './io.mjs';
import * as model from './model.mjs';
import * as macros from './macros.mjs';
import * as codegen from './codegen.mjs';
import * as introspect from './introspect.mjs';
import * as metadata from './metadata.mjs';
import * as dsl from './dsl.mjs';
import * as flow from 'cljc-flow/flow';
import * as trace_js from './trace_js.mjs';
import * as run from './run.mjs';
import * as cli from './cli.mjs';
import * as dataset from './dataset.mjs';

export { util, feature, geo, io, model, macros, codegen, introspect, metadata, dsl, flow, trace_js, run, cli, dataset };
