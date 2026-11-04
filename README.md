# valtan

[![SACLA Tests](https://github.com/cxxxr/valtan/actions/workflows/test.yml/badge.svg)](https://github.com/cxxxr/valtan/actions/workflows/test.yml)

Common Lisp to JavaScript compiler.

Features:
- Code obfuscation
- Code minification after building with webpack
- Source-maps to aid debugging
- Integrates with other javascript libraries
- Can use Common Lisp features including macros
  - Can generate React components with macros

## Installation

With Roswell:

```
$ ros follow-dependency=t install cxxxr/valtan
```

From source:

```
$ git clone https://github.com/cxxxr/valtan ~/common-lisp/valtan
$ cd ~/common-lisp/valtan
$ make build
$ ./valtan
```

## Demo and Examples

### Tic Tac Toe React

https://github.com/cxxxr/valtan/tree/master/example/react-tic-tac-toe

```
$ cd example/react-tic-tac-toe
$ npm install
$ npm run build
$ open index.html
```

### Example used with PlayCanvas, a batteries-included engine built on top of webgl.

https://github.com/jason-chandler/portal-plurality

## Create a project

```
$ valtan init project-name
$ cd project-name
$ npm install

$ npm start # run
```

## Status

Valtan is still in its alpha stage.

**Latest complete upstream ANSI run: 99.43%** (21,812/21,937 cases,
all 24 categories complete; 2026-11-04, eval mode).
[Recorded full result](tests/ansi/full-conformance-results.json).
The independent 99% gate passes, including complete registration coverage and
verification of the measured runtime bundle. All 125 failures remain in the report.

This measures success on a pinned, unmodified test suite, not the percentage of
the ANSI specification implemented. The full report includes every failure;
selected regression tests do not contribute to the overall rate. See the
[ANSI test documentation](tests/ansi/README.md) for the audit and reproduction steps.

**Latest complete SACLA run: 99.36%** (9,953/10,017 cases, all 27 files complete; 2026-11-04).
[Recorded results](tests/sacla-conformance-results.json).

Valtan [aims](https://github.com/cxxxr/valtan/issues/18) to achieve
compatibility with [ANSI Common Lisp](https://www.cliki.net/CLHS). At present,
the primary test suite being used is the
[`ansi-test`](https://gitlab.common-lisp.net/ansi-test/ansi-test). However,
valtan is not yet sufficiently developed to fully support this test suite.
More details and discussions can be found in [Issue
18](https://github.com/cxxxr/valtan/issues/18).

As an alternative, valtan utilizes the [sacla common lisp test
suite](https://minejima.jp/lisp/sacla/index-en.html).
The previous percentage excluded forms after reader errors and omitted some
bundled test files. The runner now includes all 27 test files and reports
incomplete files explicitly. CI requires every expected form to execute and at
least 90% to pass. Expected counts were independently checked with SBCL using
the `:valtan` feature, including the suite’s existing conditional exclusions.
This measures SACLA coverage; it does not establish a percentage of the entire
ANSI specification. The earlier [SACLA baseline](tests/conformance-results.json)
records 9,824/10,017 passes (98.07%). The latest run improves that to 9,953 passes,
with all 64 remaining failures included in the result. Diagnostic printing now
handles circular test forms without changing the bindings used to execute them.

### Run tests
```bash
$ cd valtan/tests
$ npm install
$ ../valtan build tests.system && npx vite build
$ npm start                           # Run all tests
$ node dist/tests.js --category cons  # Run specific category
$ node dist/tests.js --quick          # Quick smoke test
$ node dist/tests.js --json           # Output JSON for CI
$ node dist/tests.js > test-output.txt 2>&1
$ node check-conformance.mjs test-output.txt  # Require full coverage and >=90%
```

### Upstream ANSI tests

The upstream `ansi-test` suite is also integrated at a pinned revision, with a
separate runner and JSON reporting for failures and incomplete execution.
Its full measured success rate is 91.45%, independently of the SACLA percentage
above. The remaining failures are recorded individually. See
[setup, commands, and adapter limitations](tests/ansi/README.md).

```bash
$ git submodule update --init --recursive
$ cd tests
$ npm run build:ansi
$ npm run test:ansi -- --file cons/atom.lsp
$ npm run test:ansi -- --category numbers --timeout 900
$ npm run test:ansi -- --jobs 4 --timeout 7200 --output ansi-results.json
$ npm run test:ansi:check -- ansi-results.json dist-ansi/ansi.js
```

The full-run check requires all 24 upstream categories to finish, validates the
registered test inventory and runtime bundle, and requires at least 99% success.
The runner itself reports a nonzero exit status whenever any test fails.

### Runtime performance

The compiler specializes fixed-arity arithmetic, comparisons, list access and
character equality while retaining type checks and left-to-right argument
evaluation. Single-value returns and backtraces now allocate their Lisp/array
representations only when consumed; vector indexing avoids the general stride
calculation. These optimizations are enabled by default, including in runtime
`COMPILE`/`EVAL` code generation.

Eight compiled Lisp workloads cover arithmetic, recursion, lists, vectors,
closures, hash tables, strings and CLOS. Against a clean rebuild of commit
`55ea3e0b53231387246bbaa44be86aa0c0fd5667`, the measured geometric-mean speedup is
**19.56x**, and the sum of the eight workload medians improves **14.89x**.
Individual speedups range from **3.59x to 52.01x**; this is a representative
benchmark result, not a guarantee that every application runs ten times faster.
Measurements use Node 24.21.0 on an AMD Ryzen AI Max+ 395. See the
[baseline](tests/performance/baseline.json) and
[optimized samples](tests/performance/optimized.json), including bundle hashes
and build-time workload/semantic-test hashes.

The final build preserves all 21,937 upstream ANSI and 10,017 SACLA test outcomes
from the baseline. The 42 existing regression/coverage test groups, new kernel
tests and compiler semantic assertions also pass. See the
[validation record](tests/performance/validation.json). CI runs the new semantic
tests; performance comparisons use both revisions on the same machine.

```bash
# Rebuild ./valtan from the changed compiler sources first (see installation).
cd tests
npm run build:performance
npm run test:performance
node performance/run.mjs dist-performance/performance.js after.json
node performance/compare.mjs performance/baseline.json after.json
```

For a comparison on another machine, build both revisions there with identical
`performance.system`, `performance/workloads.lisp`, `performance/semantics.lisp`
and `vite.performance.config.js` files. Save the original bundle before rebuilding,
then run `performance/run.mjs ORIGINAL-BUNDLE before.json` and compare that report
with `after.json`. The comparison requires matching Node/CPU and workload identity;
the default gate is a geometric mean of at least 10x. Timing excludes startup,
compilation and semantic regression assertions. Each workload checks its result,
warms up for at least 500 ms and ten calls, then reports seven sample timings and
their median. Run measurements without other CPU-intensive jobs.

## License
MIT
