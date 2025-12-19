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

**Overall Test Pass Rate: 98.5%** (4346/4411) - [View detailed results](https://github.com/cxxxr/valtan/actions/workflows/test.yml)

Valtan [aims](https://github.com/cxxxr/valtan/issues/18) to achieve
compatibility with [ANSI Common Lisp](https://www.cliki.net/CLHS). At present,
the primary test suite being used is the
[`ansi-test`](https://gitlab.common-lisp.net/ansi-test/ansi-test). However,
valtan is not yet sufficiently developed to fully support this test suite.
More details and discussions can be found in [Issue
18](https://github.com/cxxxr/valtan/issues/18).

As an alternative, valtan utilizes the [sacla common lisp test
suite](https://minejima.jp/lisp/sacla/index-en.html).

### Run tests
```bash
$ cd valtan/tests
$ npm install
$ npm start                           # Run all tests
$ node dist/tests.js --category cons  # Run specific category
$ node dist/tests.js --quick          # Quick smoke test
$ node dist/tests.js --json           # Output JSON for CI
```

## License
MIT
