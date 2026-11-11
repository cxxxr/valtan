import test from 'node:test';
import assert from 'node:assert/strict';
import {readForms} from './staged-artifact.mjs';

const header = '; Valtan compiled JavaScript, version 1\n';
const lispString = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';

test('compiled JavaScript preserves Lisp string escapes and immediate/called forms', () => {
  const code = 'return "quote: \\";\n// λ and \\n remain literal';
  const forms = readForms(header + `(FFI:JS-EVAL ${lispString(code)})\n` +
    `(FUNCALL (FFI:JS-EVAL ${lispString('return function () { return 42; };')}))\n`);
  assert.deepEqual(forms, [{code, call: false}, {code: 'return function () { return 42; };', call: true}]);
  assert.equal(new Function(forms[1].code)()(), 42);
});

test('reject incomplete, unsupported and extra Lisp forms instead of replaying a prefix', () => {
  for (const body of ['(FFI:JS-EVAL "unfinished', '(FFI:JS-EVAL "escape\\',
    '(FUNCALL (FFI:JS-EVAL "return 1;")) (PRINT 2)',
    '(FUNCALL (FFI:JS-EVAL "return 1;")', '(FFI:JS-EVAL "return 1;"))'])
    assert.throws(() => readForms(header + body), undefined, body);
  assert.throws(() => readForms(header.replace('version 1', 'version 2')));
});
