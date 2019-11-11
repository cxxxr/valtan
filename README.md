# valtan
Common Lisp to JavaScript compiler

## Installation

```
$ ros install cxxxr/valtan
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
$ valtan-init project-name
$ cd project-name
$ npm install

$ npm start # run
```

## Status
This project is still alpha quality.

[sacla common lisp](https://minejima.jp/lisp/sacla/index-en.html) depends on Ansi test.
The followings are list of Ansi test and the "Success or failure".

- [ ] desirable-printer.lisp
- [ ] must-array.lisp
- [X] must-character.lisp
- [X] must-condition.lisp
- [X] must-cons.lisp
- [ ] must-data-and-control.lisp
- [ ] must-do.lisp
- [ ] must-eval.lisp
- [ ] must-hash-table.lisp
- [ ] must-loop.lisp
- [ ] must-package.lisp
- [ ] must-printer.lisp
- [ ] must-reader.lisp
- [X] must-sequence.lisp
- [X] must-string.lisp
- [X] must-symbol.lisp
- [ ] should-array.lisp
- [X] should-character.lisp
- [X] should-cons.lisp
- [ ] should-data-and-control.lisp
- [ ] should-eval.lisp
- [ ] should-hash-table.lisp
- [ ] should-package.lisp
- [ ] should-sequence.lisp
- [X] should-string.lisp
- [X] should-symbol.lisp
- [ ] x-sequence.lisp

## License
MIT
