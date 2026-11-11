import test from 'node:test';
import assert from 'node:assert/strict';
import * as lisp from '../../kernel/lisp.js';

test('suspended dynamic bindings isolate local writes and retain shared global writes', () => {
  const a = lisp.createBindingContext(), b = lisp.createBindingContext();
  const x = lisp.makeSymbol('DYNAMIC-X');
  x.value = 'global';
  function* run() {
    const outer = lisp.bindSpecial(x);
    x.value = 'outer';
    try {
      yield x.value;
      const inner = lisp.bindSpecial(x);
      x.value = 'inner';
      try { yield x.value; }
      finally { lisp.unbindSpecial(inner); }
      yield x.value;
    } finally { lisp.unbindSpecial(outer); }
    return x.value;
  }
  const iterator = run();
  const step = () => lisp.withBindingContext(a, () => iterator.next());
  assert.deepEqual(step(), { value: 'outer', done: false });
  assert.equal(x.value, 'global');
  lisp.withBindingContext(b, () => { x.value = 'new-global'; });
  assert.deepEqual(step(), { value: 'inner', done: false });
  assert.equal(x.value, 'new-global');
  lisp.withBindingContext(b, () => { x.value = 'latest-global'; });
  assert.deepEqual(step(), { value: 'outer', done: false });
  assert.deepEqual(step(), { value: 'latest-global', done: true });
  assert.equal(x.value, 'latest-global');
});

test('interleaved binding stacks preserve unbound values, mutation and exceptional cleanup', () => {
  const a = lisp.createBindingContext(), b = lisp.createBindingContext();
  const x = lisp.makeSymbol('UNBOUND-X');
  function* run(value) {
    const frame = lisp.bindSpecial(x);
    x.value = value;
    try {
      yield x.value;
      x.value += '!';
      yield x.value;
    } finally { lisp.unbindSpecial(frame); }
  }
  const one = run('a'), two = run('b');
  const step = (context, iterator) => lisp.withBindingContext(context, () => iterator.next());
  assert.equal(step(a, one).value, 'a');
  assert.equal(x.isBound(), false);
  assert.equal(step(b, two).value, 'b');
  assert.equal(step(a, one).value, 'a!');
  assert.equal(step(b, two).value, 'b!');
  const error = new Error('cancel');
  assert.throws(() => lisp.withBindingContext(a, () => one.throw(error)), e => e === error);
  assert.equal(x.isBound(), false);
  assert.equal(step(b, two).done, true);
  assert.equal(x.isBound(), false);
});

test('binding contexts restore the caller on errors and reject foreign unwind tokens', () => {
  const context = lisp.createBindingContext(), x = lisp.makeSymbol('CALLER-X');
  x.value = 1;
  const frame = lisp.bindSpecial(x);
  x.value = 2;
  try {
    assert.throws(() => lisp.withBindingContext(context, () => {
      assert.equal(x.value, 1);
      x.value = 3;
      lisp.unbindSpecial(frame);
    }), /Dynamic binding stack mismatch/);
    assert.equal(x.value, 2);
    lisp.withBindingContext(context, () => {
      assert.equal(x.value, 3);
      lisp.withBindingContext(context, () => { x.value = 4; });
      assert.equal(x.value, 4);
    });
    assert.equal(x.value, 2);
  } finally { lisp.unbindSpecial(frame); }
  assert.equal(x.value, 4);
});

test('CLOS cache keys preserve class order, EQL matches and invalidation', () => {
  const gf = {}, other = {}, a = {}, b = {}, eql = {};
  const list = (...items) => lisp.jsArrayToList(items);
  const key = (classes, matches = []) => lisp.internMethodCacheKey(gf, list(...classes), list(...matches));
  const ordinary = key([a, b]);
  assert.equal(key([a, b]), ordinary);
  assert.notEqual(key([b, a]), ordinary);
  assert.notEqual(key([a]), ordinary);
  assert.notEqual(key([a, b], [eql]), ordinary);
  assert.equal(key([a, b], [eql]), key([a, b], [eql]));
  assert.notEqual(key([a], [b]), ordinary);
  assert.notEqual(key([]), key([a]));
  assert.notEqual(lisp.internMethodCacheKey(other, list(a, b), list()), ordinary);
  lisp.resetMethodCacheKeys(gf);
  assert.notEqual(key([a, b]), ordinary);
  assert.equal(key([a, b]), key([a, b]));
});

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

test('VALUES-LIST retains a snapshot independent of later list mutation', () => {
  const list = lisp.jsArrayToList([1, 2, 3]);
  assert.equal(lisp.CL_valuesList(list), 1);
  const snapshot = lisp.currentValues();
  lisp.rplaca(list, 99);
  lisp.rplacd(list, lisp.S_nil);
  assert.deepEqual(snapshot, [1, 2, 3]);
  assert.equal(lisp.CL_valuesList(lisp.S_nil), lisp.S_nil);
  assert.deepEqual(lisp.currentValues(), []);
  assert.deepEqual(snapshot, [1, 2, 3]);
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

test('symbol calls observe redefinition and preserve multiple values with profiling', () => {
  const symbol = lisp.intern('PERF-CALL-VALUES', lisp.cl_user_package);
  symbol.func = (...args) => lisp.values(...args);
  assert.equal(lisp.callFunctionWithCallStack(symbol, 7, 8), 7);
  assert.deepEqual(lisp.currentValues(), [7, 8]);
  symbol.func = () => lisp.values();
  assert.equal(lisp.callFunctionWithCallStack(symbol, 7, 8), lisp.S_nil);
  assert.deepEqual(lisp.currentValues(), []);
  symbol.func = x => lisp.values(x, x + 1);
  const lines = [], log = console.log;
  console.log = (...args) => lines.push(args.map(String).join(' '));
  lisp.startProfile();
  try {
    assert.equal(lisp.callFunctionWithCallStack(symbol, 9), 9);
    assert.deepEqual(lisp.currentValues(), [9, 10]);
  } finally {
    try { lisp.finishProfile(); } finally { console.log = log; }
  }
  assert.match(lines.join('\n'), /PERF-CALL-VALUES\s+1\s/);
  assert.equal(lisp.getBacktrace(), 'Backtrace:\n');
});
