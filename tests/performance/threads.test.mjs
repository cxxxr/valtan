import test from 'node:test';
import assert from 'node:assert/strict';
import * as lisp from '../../kernel/lisp.js';
const op = lisp.intern('%COOPERATIVE-THREAD-OP', 'SYSTEM');
const keyword = name => lisp.intern(name, 'KEYWORD');
const call = (name, ...args) => op.getFunction()(keyword(name), ...args);
const gate = () => { let release; const promise = new Promise(resolve => { release = resolve; }); return { promise, release }; };
const waitUntil = async predicate => {
  const deadline = performance.now() + 5000;
  while (!predicate()) {
    if (performance.now() > deadline) throw new Error('Thread test timed out');
    await new Promise(resolve => setTimeout(resolve, 1));
  }
};

test('native thread identity, all/live registry and multiple-value results survive joining', async () => {
  const main = call('CURRENT'), wait = gate();
  let started = false;
  const target = call('MAKE', lisp.makeResumable(function*() {
    started = true;
    assert.equal(call('CURRENT'), target);
    assert.equal(call('P', target), lisp.S_t);
    yield { kind: 'await', promise: wait.promise };
    return lisp.values(7, 9);
  }), 'named worker');
  assert.equal(started, false);
  assert.equal(call('CURRENT'), main);
  assert.equal(call('NAME', target), 'named worker');
  assert.equal(call('ALIVE', target), lisp.S_t);
  assert.ok(call('ALL').includes(main) && call('ALL').includes(target));
  const scheduler = new lisp.CooperativeScheduler();
  const joiner = scheduler.spawn(op, [keyword('JOIN'), target]);
  await waitUntil(() => joiner.state === 'waiting');
  assert.throws(() => call('JOIN', target), /resumable Lisp call/);
  wait.release();
  assert.deepEqual(await joiner.completion, [target]);
  assert.deepEqual(call('RESULT', target), [7, 9]);
  assert.equal(call('ALIVE', target), lisp.S_nil);
  assert.equal(call('P', target), lisp.S_t);
  assert.equal(call('P', {}), lisp.S_nil);
  assert.equal(call('ALL').includes(target), false);
  assert.equal(call('JOIN', target), target);
  assert.equal(lisp.allScheduledTasks().length, 0);
});

test('native join distinguishes target failure and cancellation of the joining task', async () => {
  const error = new Error('target failure');
  const failed = call('MAKE', () => { throw error; }, 'failure');
  const scheduler = new lisp.CooperativeScheduler();
  assert.deepEqual(await scheduler.spawn(op, [keyword('JOIN'), failed]).completion, [failed]);
  assert.equal(call('FAILED', failed), lisp.S_t);
  assert.equal(call('ERROR', failed), error);
  const wait = gate();
  const target = call('MAKE', lisp.makeResumable(function*() {
    yield { kind: 'await', promise: wait.promise }; return lisp.values(42);
  }), 'waiting');
  const joiner = scheduler.spawn(op, [keyword('JOIN'), target]);
  await waitUntil(() => joiner.state === 'waiting');
  scheduler.cancel(joiner, error);
  await assert.rejects(joiner.completion, value => value === error);
  assert.equal(call('ALIVE', target), lisp.S_t);
  wait.release();
  assert.deepEqual(await target.completion, [42]);
});

test('self interrupt completes before the caller continues, including suspension and tail calls', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const order = [];
  const target = scheduler.spawn(lisp.makeResumable(function*() {
    const current = call('CURRENT');
    const returned = yield* lisp.callResumable(op, keyword('INTERRUPT'), current,
      lisp.makeResumable(function*() {
        order.push('callback start');
        yield { kind: 'await', promise: Promise.resolve() };
        order.push('callback end');
      }));
    assert.equal(returned, current);
    order.push('caller');
    return returned;
  }));
  assert.deepEqual(await target.completion, [target]);
  assert.deepEqual(order, ['callback start', 'callback end', 'caller']);
});

test('destroy marks a thread dead but join still waits for its cleanup', async () => {
  const original = gate(), cleanup = gate();
  let cleaning = false;
  const target = call('MAKE', lisp.makeResumable(function*() {
    try { yield { kind: 'await', promise: original.promise }; }
    finally { cleaning = true; yield { kind: 'await', promise: cleanup.promise }; }
  }), 'destroy');
  await waitUntil(() => target.state === 'waiting');
  assert.equal(call('DESTROY', target), lisp.S_t);
  assert.equal(call('ALIVE', target), lisp.S_nil);
  const scheduler = new lisp.CooperativeScheduler();
  const joiner = scheduler.spawn(op, [keyword('JOIN'), target]);
  await waitUntil(() => cleaning && joiner.state === 'waiting');
  assert.equal(target.task.done, false);
  cleanup.release();
  assert.deepEqual(await joiner.completion, [target]);
  assert.ok(call('ERROR', target) instanceof lisp.ThreadTermination);
  assert.equal(call('DESTROY', target), lisp.S_nil);
  assert.throws(() => call('DESTROY', call('CURRENT')), /current thread/);
});

test('native atomic words compare, exchange and wrap at 64 bits without lost updates', async () => {
  const atomic = (name, ...args) => lisp.CL_cooperativeAtomicOperation(keyword(name), ...args);
  const word = atomic('MAKE', 0);
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 1 });
  const tasks = Array.from({ length: 64 }, () => scheduler.spawn(lisp.makeResumable(function*() {
    for (let i = 0; i < 10; i++) { atomic('INC', word, 1); yield lisp.resumableCheckpoint; }
    return lisp.values();
  })));
  await Promise.all(tasks.map(task => task.completion));
  assert.equal(atomic('VALUE', word), 640);
  assert.equal(atomic('CAS', word, 1, 9), lisp.S_nil);
  assert.equal(atomic('CAS', word, 640, 9007199254740993n), lisp.S_t);
  assert.equal(atomic('INC', word, 1), 9007199254740994n);
  atomic('SET', word, 18446744073709551615n);
  assert.equal(atomic('INC', word, 1), 0);
  assert.equal(atomic('DEC', word, 1), 18446744073709551615n);
});
