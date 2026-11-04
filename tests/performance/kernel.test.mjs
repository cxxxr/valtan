import test from 'node:test';
import assert from 'node:assert/strict';
import * as lisp from '../../kernel/lisp.js';

test('single and multiple values yield stable snapshots across nested cleanup', () => {
  lisp.values1(1);
  const one = lisp.currentValues();
  lisp.values1(2);
  assert.deepEqual(one, [1]);
  lisp.restoreValues(one);
  assert.deepEqual(lisp.currentValues(), [1]);
  lisp.values(3, 4);
  const many = lisp.currentValues();
  lisp.values1(5);
  assert.deepEqual(many, [3, 4]);
  lisp.restoreValues(many);
  assert.deepEqual(lisp.currentValues(), [3, 4]);
  assert.equal(lisp.values(), lisp.S_nil);
  assert.deepEqual(lisp.currentValues(), []);
  lisp.values1(6);
  assert.deepEqual(lisp.CL_multipleValueCall((...xs) => xs, 0, 6), [0, 6]);
});

test('lazy backtrace frames preserve function/argument order and unwind on exceptions', () => {
  const printed = [];
  lisp.setLispFormatFunction((control, frame) => {
    printed.push(lisp.listToJsArray(frame));
    return 'frame';
  });
  const marker = new Error('marker');
  const inner = () => { lisp.getBacktrace(); throw marker; };
  const outer = (x) => lisp.CL_apply(inner, [x, 2]);
  assert.throws(() => lisp.CL_apply(outer, [1]), e => e === marker);
  assert.deepEqual(printed, [[inner, 1, 2], [outer, 1]]);
  assert.equal(lisp.getBacktrace(), 'Backtrace:\n');
  printed.length = 0;
  const symbol = lisp.intern('PERF-BACKTRACE', lisp.cl_user_package);
  symbol.func = inner;
  assert.throws(() => lisp.callFunctionWithCallStack(symbol, 7, 8), e => e === marker);
  assert.deepEqual(printed, [[symbol, 7, 8]]);
  assert.equal(lisp.getBacktrace(), 'Backtrace:\n');
});
