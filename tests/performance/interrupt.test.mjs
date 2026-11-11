import test from 'node:test';
import assert from 'node:assert/strict';
import * as lisp from '../../kernel/lisp.js';

const gate = () => { let release; const promise = new Promise(resolve => { release = resolve; }); return { promise, release }; };
const waitUntil = async predicate => {
  const deadline = performance.now() + 5000;
  while (!predicate()) {
    if (performance.now() > deadline) throw new Error('Interrupt test did not reach its expected state');
    await new Promise(resolve => setTimeout(resolve, 1));
  }
};
const start = (scheduler, generator) => scheduler.spawn(lisp.makeResumable(generator));

test('interrupt callbacks use target bindings and preserve a wait fulfilled during the callback', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const x = lisp.makeSymbol('INTERRUPTED-X'); x.value = 'global';
  const original = gate(), callback = gate(), entered = gate();
  let resumed = false;
  const target = start(scheduler, function*() {
    const binding = lisp.bindSpecial(x); x.value = 'target';
    try {
      lisp.values('original', 2);
      const value = yield { kind: 'await', promise: original.promise };
      assert.deepEqual(lisp.currentValues(), ['original', 2]);
      resumed = true;
      return lisp.values(value, x.value);
    } finally { lisp.unbindSpecial(binding); }
  });
  await waitUntil(() => target.state === 'waiting');
  assert.equal(scheduler.interrupt(target, lisp.makeResumable(function*() {
    assert.equal(lisp.currentScheduledTask(), target);
    assert.equal(x.value, 'target');
    x.value = 'modified';
    const binding = lisp.bindSpecial(x); x.value = 'callback';
    try {
      entered.release();
      yield { kind: 'await', promise: callback.promise };
      assert.equal(x.value, 'callback');
      return lisp.values('discarded', 99);
    } finally { lisp.unbindSpecial(binding); }
  })), true);
  await entered.promise;
  original.release(42);
  await new Promise(resolve => setTimeout(resolve, 0));
  assert.equal(resumed, false);
  assert.equal(x.value, 'global');
  callback.release();
  assert.deepEqual(await target.completion, [42, 'modified']);
  assert.equal(x.value, 'global');
  assert.equal(target.frames.length, 0);
  assert.equal(scheduler.interrupt(target, () => {}), false);
});

test('a completed interrupt leaves the original unresolved wait parked', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const original = gate();
  let called = 0, resumed = false;
  const target = start(scheduler, function*() {
    const value = yield { kind: 'await', promise: original.promise };
    resumed = true;
    return lisp.values(value);
  });
  await waitUntil(() => target.state === 'waiting');
  scheduler.interrupt(target, () => { called++; return lisp.values('ignored'); });
  await waitUntil(() => called === 1 && target.frames.length === 1);
  assert.equal(target.state, 'waiting');
  assert.equal(resumed, false);
  original.release(73);
  assert.deepEqual(await target.completion, [73]);
});

test('nonlocal interrupt exits retain their transferred multiple values', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const original = gate(), token = {};
  const target = start(scheduler, function*() {
    lisp.values('old');
    try { yield { kind: 'await', promise: original.promise }; }
    catch (error) {
      assert.ok(error instanceof lisp.BlockValue && error.name === token);
      const saved = lisp.currentValues();
      yield { kind: 'await', promise: Promise.resolve() };
      lisp.values('cleanup');
      lisp.restoreValues(saved);
      return error.value;
    }
  });
  await waitUntil(() => target.state === 'waiting');
  scheduler.interrupt(target, () => { throw new lisp.BlockValue(token, lisp.values('escaped', 17)); });
  assert.deepEqual(await target.completion, ['escaped', 17]);
});

test('nested interrupt failure unwinds only to its handler in the interrupted callback', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const original = gate(), obsolete = gate();
  const x = lisp.makeSymbol('NESTED-INTERRUPT-X'); x.value = 'global';
  const error = new Error('nested callback error');
  let caught = false;
  const target = start(scheduler, function*() {
    yield { kind: 'await', promise: original.promise };
    assert.equal(x.value, 'global');
    return lisp.values('root');
  });
  await waitUntil(() => target.state === 'waiting');
  scheduler.interrupt(target, lisp.makeResumable(function*() {
    const binding = lisp.bindSpecial(x); x.value = 'outer callback';
    try {
      try { yield { kind: 'await', promise: obsolete.promise }; }
      catch (e) { assert.equal(e, error); assert.equal(x.value, 'outer callback'); caught = true; }
    } finally { lisp.unbindSpecial(binding); }
  }));
  await waitUntil(() => target.frames.length === 2 && target.state === 'waiting');
  scheduler.interrupt(target, lisp.makeResumable(function*() {
    assert.equal(x.value, 'outer callback');
    yield { kind: 'yield' };
    throw error;
  }));
  await waitUntil(() => caught && target.frames.length === 1);
  obsolete.release('ignored');
  await new Promise(resolve => setTimeout(resolve, 0));
  assert.equal(target.state, 'waiting');
  original.release();
  assert.deepEqual(await target.completion, ['root']);
  assert.equal(x.value, 'global');
});

test('interrupt failures enter the original handler and discard the abandoned wait', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const original = gate(), cleanup = gate();
  const error = new Error('interrupt failure');
  const target = start(scheduler, function*() {
    try { yield { kind: 'await', promise: original.promise }; }
    catch (e) { assert.equal(e, error); return lisp.values('handled', 7); }
    finally { yield { kind: 'await', promise: cleanup.promise }; }
  });
  await waitUntil(() => target.state === 'waiting');
  scheduler.interrupt(target, () => { throw error; });
  await waitUntil(() => target.frames.length === 1 && target.state === 'waiting' && target.frames[0].waitVersion >= 3);
  original.release('obsolete');
  await new Promise(resolve => setTimeout(resolve, 0));
  assert.equal(target.state, 'waiting');
  cleanup.release();
  assert.deepEqual(await target.completion, ['handled', 7]);
});

test('a rejected original wait is retained while an interrupt callback awaits', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const callback = gate();
  let reject;
  const promise = new Promise((_, no) => { reject = no; });
  const error = new Error('original rejection');
  let caught = false;
  const target = start(scheduler, function*() {
    try { yield { kind: 'await', promise }; }
    catch (e) { assert.equal(e, error); caught = true; }
    return lisp.values('caught');
  });
  await waitUntil(() => target.state === 'waiting');
  scheduler.interrupt(target, lisp.makeResumable(function*() { yield { kind: 'await', promise: callback.promise }; }));
  await waitUntil(() => target.frames.length === 2 && target.state === 'waiting');
  reject(error);
  await new Promise(resolve => setTimeout(resolve, 0));
  assert.equal(caught, false);
  callback.release();
  assert.deepEqual(await target.completion, ['caught']);
});

test('cancellation unwinds interrupt callbacks before the original task, including asynchronous cleanup', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const original = gate(), callback = gate(), callbackCleanup = gate(), rootCleanup = gate();
  const order = [];
  const target = start(scheduler, function*() {
    try { yield { kind: 'await', promise: original.promise }; }
    finally {
      order.push('root cleanup');
      yield { kind: 'await', promise: rootCleanup.promise };
      order.push('root done');
    }
  });
  await waitUntil(() => target.state === 'waiting');
  scheduler.interrupt(target, lisp.makeResumable(function*() {
    try { yield { kind: 'await', promise: callback.promise }; }
    finally {
      order.push('callback cleanup');
      yield { kind: 'await', promise: callbackCleanup.promise };
      order.push('callback done');
    }
  }));
  await waitUntil(() => target.frames.length === 2 && target.state === 'waiting');
  scheduler.interrupt(target, () => assert.fail('Queued interrupt survived cancellation'));
  const error = new Error('cancel interrupted task');
  scheduler.cancel(target, error);
  await waitUntil(() => order.length === 1);
  original.release(); callback.release();
  await new Promise(resolve => setTimeout(resolve, 0));
  assert.deepEqual(order, ['callback cleanup']);
  callbackCleanup.release();
  await waitUntil(() => order.length === 3);
  assert.deepEqual(order, ['callback cleanup', 'callback done', 'root cleanup']);
  rootCleanup.release();
  await assert.rejects(target.completion, e => e === error);
  assert.equal(target.frames.length, 0);
  assert.equal(scheduler.tasks.size, 0);
});

test('self-interrupt runs at the next suspension with the same recursive lock owner', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const lock = new lisp.CooperativeLock('interrupt lock', true);
  const original = gate();
  let called = false;
  const target = start(scheduler, function*() {
    yield* lock.acquire();
    try {
      scheduler.interrupt(lisp.currentScheduledTask(), lisp.makeResumable(function*() {
        yield* lock.acquire();
        try { assert.equal(lock.depth, 2); called = true; original.release(5); }
        finally { lock.release(); }
      }));
      const value = yield { kind: 'await', promise: original.promise };
      assert.equal(lock.depth, 1);
      return lisp.values(value);
    } finally { lock.release(); }
  });
  assert.deepEqual(await target.completion, [5]);
  assert.equal(called, true);
  assert.equal(lock.owner, null);
  assert.throws(() => new lisp.CooperativeScheduler().interrupt(target, () => {}), /another scheduler/);
});

test('callbacks can interrupt checkpoint-only computation without losing its position', async () => {
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 1, slicesPerTurn: 1 });
  const order = [];
  const target = start(scheduler, function*() {
    for (let i = 0; i < 10; i++) { order.push(i); yield lisp.resumableCheckpoint; }
    return lisp.values('finished');
  });
  await waitUntil(() => order.length >= 2);
  scheduler.interrupt(target, () => { order.push('interrupt'); return lisp.values(); });
  assert.deepEqual(await target.completion, ['finished']);
  assert.deepEqual(order.filter(x => typeof x === 'number'), Array.from({ length: 10 }, (_, i) => i));
  assert.ok(order.indexOf('interrupt') > 0 && order.indexOf('interrupt') < order.indexOf(9));
});
