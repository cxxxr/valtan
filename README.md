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

**Upstream ANSI test success: 91.45%** (20,062/21,937 cases, all 24 categories
complete, no unexecuted tests; 2026-11-04, eval mode).
[Recorded full result](tests/ansi/full-results.json). The independent coverage
and 90% gate passes; 1,875 test failures remain.

**Recorded SACLA baseline: 98.07%** (9,824/10,017 cases, all 27 files complete; 2026-11-04).
[Recorded results](tests/conformance-results.json).

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
ANSI specification. The recorded SACLA run passes 9,824 of 10,017 cases (98.07%), with all
27 files complete. It includes the radix-reader, WITH-SLOTS, WITH-ACCESSORS,
CHANGE-CLASS and default slot allocation fixes, along with the preceding runtime improvements; see
[the recorded results](tests/conformance-results.json).

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
registered test inventory and runtime bundle, and requires at least 90% success.
The runner itself reports a nonzero exit status whenever any test fails.

## License
MIT
