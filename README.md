# valtan
Common Lisp to JavaScript compiler

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

## Demo

```
$ cd example/react-tic-tac-toe
$ npm install
$ npm run build
$ open index.html
```

## Create a project

```
$ valtan init project-name
$ cd project-name
$ npm install

$ npm start # run
```

## Status

Valtan is still in its alpha stage.

Valtan [aims](https://github.com/cxxxr/valtan/issues/18) to achieve
compatibility with [ANSI Common Lisp](https://www.cliki.net/CLHS). At present,
the primary test suite being used is the
[`ansi-test`](https://gitlab.common-lisp.net/ansi-test/ansi-test). However,
valtan is not yet sufficiently developed to fully support this test suite.
More details and discussions can be found in [Issue
18](https://github.com/cxxxr/valtan/issues/18).

As an alternative, valtan utilizes the [sacla common lisp test
suite](https://minejima.jp/lisp/sacla/index-en.html). Below is a table showing
the results of valtan when run against this suite:

|Test                                     |Success|Failure|
|:----------------------------------------|:-----|:-----|
|sacla-tests/desirable-printer.lisp       | 14   | 22   |
|sacla-tests/must-array.lisp              | 162  | 171  |
|sacla-tests/must-character.lisp          | 313  | 0    |
|sacla-tests/must-condition.lisp          | 142  | 5    |
|sacla-tests/must-cons.lisp               | 728  | 0    |
|sacla-tests/must-data-and-control.lisp   | 170  | 144  |
|sacla-tests/must-do.lisp                 | 75   | 0    |
|sacla-tests/must-eval.lisp               | 9    | 0    |
|sacla-tests/must-hash-table.lisp         | 67   | 29   |
|sacla-tests/must-loop.lisp               | 778  | 72   |
|sacla-tests/must-reader.lisp             | 891  | 696  |
|sacla-tests/must-sequence.lisp           | 6    | 3853 |
|sacla-tests/must-string.lisp             | 0    | 414  |
|sacla-tests/must-symbol.lisp             | 5    | 191  |
|sacla-tests/should-array.lisp            | 17   | 20   |
|sacla-tests/should-character.lisp        | 46   | 3    |
|sacla-tests/should-cons.lisp             | 141  | 0    |
|sacla-tests/should-data-and-control.lisp | 1    | 3    |
|sacla-tests/should-eval.lisp             | 0    | 2    |
|sacla-tests/should-hash-table.lisp       | 3    | 0    |
|sacla-tests/should-package.lisp          | 3    | 3    |
|sacla-tests/should-sequence.lisp         | 48   | 33   |
|sacla-tests/should-string.lisp           | 0    | 0    |
|sacla-tests/should-symbol.lisp           | 44   | 0    |

### Run tests
```
$ cd valtan/tests
$ npm i
$ npm start
```

## License
MIT
