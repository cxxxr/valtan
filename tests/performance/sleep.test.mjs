import test from 'node:test';
import assert from 'node:assert/strict';
import * as lisp from '../../kernel/lisp.js';

const operation = lisp.intern('ENVIRONMENT-OP', 'SYSTEM');
const sleep = lisp.intern('SLEEP', 'KEYWORD');

test('resumable sleep allows other tasks and host callbacks to run before its deadline', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const order = [];
  const sleeper = scheduler.spawn(lisp.makeResumable(function*() {
    order.push('start');
    const started = performance.now();
    yield* lisp.callResumable(operation, sleep, new lisp.Float(0.03));
    assert.ok(performance.now() - started >= 30);
    assert.deepEqual(lisp.currentValues(), [lisp.S_nil]);
    order.push('finish');
    return lisp.values('slept', 2);
  }));
  const other = scheduler.spawn(() => { order.push('other'); return lisp.values(); });
  const host = new Promise(resolve => setTimeout(() => { order.push('host'); resolve(); }, 0));
  assert.deepEqual(await sleeper.completion, ['slept', 2]);
  await other.completion; await host;
  assert.ok(order.indexOf('other') < order.indexOf('finish'));
  assert.ok(order.indexOf('host') < order.indexOf('finish'));
});

test('sleep cancellation clears a long host timer and unwinds its caller', () => {
  const originalSet = globalThis.setTimeout, originalClear = globalThis.clearTimeout;
  const pending = new Map();
  let next = 0, cleaned = false;
  globalThis.setTimeout = (fn, delay) => { pending.set(++next, { fn, delay }); return next; };
  globalThis.clearTimeout = timer => pending.delete(timer);
  try {
    const task = new lisp.ResumableTask(lisp.makeResumable(function*() {
      try { yield* lisp.callResumable(operation, sleep, 100000000000); }
      finally { cleaned = true; }
    }));
    assert.equal(task.resume().value.kind, 'await');
    assert.equal(pending.size, 1);
    assert.equal(pending.values().next().value.delay, 2147483647);
    const error = new Error('cancel sleep');
    assert.throws(() => task.interrupt(error), e => e === error);
    assert.equal(cleaned, true);
    assert.equal(pending.size, 0);
  } finally { globalThis.setTimeout = originalSet; globalThis.clearTimeout = originalClear; }
});

test('zero sleep yields and unrelated environment operations retain multiple values', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const order = [];
  const sleeper = scheduler.spawn(lisp.makeResumable(function*() {
    yield* lisp.callResumable(operation, sleep, 0);
    order.push('zero');
    return lisp.values1(lisp.S_nil);
  }));
  await scheduler.spawn(() => { order.push('other'); return lisp.values(); }).completion;
  assert.deepEqual(await sleeper.completion, [lisp.S_nil]);
  assert.deepEqual(order, ['other', 'zero']);
  const decode = lisp.intern('DECODE', 'KEYWORD');
  operation.getFunction()(decode, 0, 0);
  const expected = lisp.currentValues();
  assert.equal(expected.length, 9);
  assert.deepEqual(await scheduler.spawn(operation, [decode, 0, 0]).completion, expected);
  assert.equal(operation.getFunction()(sleep, 0), lisp.S_nil);
});
