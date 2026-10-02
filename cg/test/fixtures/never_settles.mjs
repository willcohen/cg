// Copyright (c) 2026 Will Cohen
//
// Part of cg, under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

// A suite whose setup never settles, as when every pool worker exits
// during init: the runner's exit call never runs. test_runner_test runs it
// and expects exit 1.
import { run_tests_and_exit_BANG_ } from '../cg/test_runner.mjs';

new Promise(() => {}).then(() => run_tests_and_exit_BANG_('never.runs'));
