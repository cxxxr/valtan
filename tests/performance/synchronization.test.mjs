import test from 'node:test';
import assert from 'node:assert/strict';
import * as lisp from '../../kernel/lisp.js';

const waitUntil = async predicate => {
  const deadline = performance.now() + 5000;
  while (!predicate()) {
    if (performance.now() > deadline) throw new Error('Synchronization test did not reach its expected state');
    await new Promise(resolve => setTimeout(resolve, 1));
  }
};
const start = (scheduler, generator) => scheduler.spawn(lisp.makeResumable(generator));
const lockNow = lock => assert.deepEqual(lock.acquire().next(), { value: true, done: true });

test('locks protect interleaved shared read-modify-write operations in 64 tasks', async () => {
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 1, slicesPerTurn: 8 });
  const lock = new lisp.CooperativeLock();
  let count = 0, inside = 0;
  const order = [];
  const tasks = Array.from({ length: 64 }, (_, id) => start(scheduler, function*() {
    for (let round = 0; round < 3; round++) {
      assert.equal(yield* lock.acquire(), true);
      try {
        assert.equal(++inside, 1);
        const before = count;
        order.push([round, id]);
        yield { kind: 'yield' };
        count = before + 1;
        assert.equal(inside--, 1);
      } finally { lock.release(); }
    }
    return lisp.values(id);
  }));
  await Promise.all(tasks.map(task => task.completion));
  assert.equal(count, 192);
  assert.deepEqual(order, Array.from({ length: 192 }, (_, i) => [Math.floor(i / 64), i % 64]));
  assert.equal(lock.owner, null);
  assert.equal(lock.waiters.entries.size, 0);
});

test('recursive ownership, nonblocking acquisition and timed-out waiters', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const recursive = new lisp.CooperativeLock('recursive', true);
  lockNow(recursive); lockNow(recursive);
  assert.equal(recursive.depth, 2);
  const other = start(scheduler, function*() {
    assert.throws(() => recursive.release(), /Only the lock owner/);
    assert.equal(yield* recursive.acquire(false), false);
    assert.equal(yield* recursive.acquire(true, 0), false);
    assert.equal(yield* recursive.acquire(true, 5), false);
    assert.equal(recursive.waiters.entries.size, 0);
    return lisp.values();
  });
  await other.completion;
  recursive.release(); assert.equal(recursive.depth, 1);
  recursive.release(); assert.equal(recursive.owner, null);
  const lock = new lisp.CooperativeLock();
  lockNow(lock);
  assert.equal(lock.acquire(false).next().value, false);
  assert.throws(() => lock.acquire().next(), /non-recursive/);
  lock.release();
  for (const timeout of [-1, NaN, Infinity, '5']) {
    assert.throws(() => lock.acquire(true, timeout).next(), /Timeout/);
    assert.equal(lock.owner, null);
  }
});

test('cancellation removes queued waiters and relinquishes an unclaimed grant', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const lock = new lisp.CooperativeLock();
  lockNow(lock);
  const acquire = function*() {
    yield* lock.acquire(true, 100000000000);
    try { return lisp.values('acquired'); }
    finally { lock.release(); }
  };
  const cancelled = start(scheduler, acquire), granted = start(scheduler, acquire), next = start(scheduler, acquire);
  await waitUntil(() => lock.waiters.entries.size === 3);
  const firstEntry = lock.waiters.first();
  const error = new Error('cancel queued');
  scheduler.cancel(cancelled, error);
  await assert.rejects(cancelled.completion, e => e === error);
  assert.equal(firstEntry.timer, null);
  assert.equal(lock.waiters.entries.size, 2);
  lock.release(); // Grants to the second task before its promise continuation.
  assert.equal(lock.owner, granted);
  scheduler.cancel(granted, error);
  await assert.rejects(granted.completion, e => e === error);
  assert.deepEqual(await next.completion, ['acquired']);
  assert.equal(lock.owner, null);
  assert.equal(lock.waiters.entries.size, 0);
});

test('condition notification wakes one waiter and broadcast wakes the remaining 63', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const lock = new lisp.CooperativeLock(), condition = new lisp.CooperativeConditionVariable();
  let completed = 0;
  const tasks = Array.from({ length: 64 }, () => start(scheduler, function*() {
    yield* lock.acquire();
    try {
      assert.equal(yield* condition.wait(lock), true);
      assert.equal(lock.owner, lisp.currentScheduledTask());
      completed++;
      return lisp.values();
    } finally { lock.release(); }
  }));
  await waitUntil(() => condition.waiters.entries.size === 64);
  lockNow(lock); condition.notify(); lock.release();
  await waitUntil(() => completed === 1);
  assert.equal(condition.waiters.entries.size, 63);
  lockNow(lock); condition.notify(true); lock.release();
  await Promise.all(tasks.map(task => task.completion));
  assert.equal(completed, 64);
  assert.equal(lock.owner, null);
  assert.equal(condition.waiters.entries.size, 0);
});

test('condition timeout waits to reacquire the lock before returning false', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const lock = new lisp.CooperativeLock(), condition = new lisp.CooperativeConditionVariable();
  let returned = false;
  const task = start(scheduler, function*() {
    yield* lock.acquire();
    try {
      assert.equal(yield* condition.wait(lock, 5), false);
      assert.equal(lock.owner, lisp.currentScheduledTask());
      returned = true;
      return lisp.values();
    } finally { lock.release(); }
  });
  await waitUntil(() => condition.waiters.entries.size === 1);
  lockNow(lock);
  await waitUntil(() => lock.waiters.entries.size === 1);
  assert.equal(condition.waiters.entries.size, 0);
  assert.equal(returned, false);
  lock.release();
  await task.completion;
  assert.equal(returned, true);
});

test('repeated cancellation reacquires recursive depth before unwinding a condition wait', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const lock = new lisp.CooperativeLock('recursive', true), condition = new lisp.CooperativeConditionVariable();
  let cleaned = false;
  const task = start(scheduler, function*() {
    yield* lock.acquire(); yield* lock.acquire();
    try { yield* condition.wait(lock); }
    finally {
      assert.equal(lock.owner, lisp.currentScheduledTask());
      assert.equal(lock.depth, 2);
      lock.release(); lock.release(); cleaned = true;
    }
  });
  await waitUntil(() => condition.waiters.entries.size === 1);
  lockNow(lock);
  const first = new Error('first cancellation'), second = new Error('second cancellation');
  scheduler.cancel(task, first);
  await waitUntil(() => lock.waiters.entries.size === 1);
  const entry = lock.waiters.first();
  scheduler.cancel(task, second);
  await waitUntil(() => lock.waiters.first() !== entry);
  assert.equal(cleaned, false);
  assert.equal(condition.waiters.entries.size, 0);
  lock.release();
  await assert.rejects(task.completion, error => error === first);
  assert.equal(cleaned, true);
  assert.equal(lock.owner, null);
  assert.equal(lock.waiters.entries.size, 0);
});

test('ordinary condition calls and manually stepped lock tasks fail before changing ownership', async () => {
  const lock = new lisp.CooperativeLock(), condition = new lisp.CooperativeConditionVariable();
  lockNow(lock);
  const owner = lock.owner;
  const fn = lisp.intern('%COOPERATIVE-CONDITION-WAIT', 'SYSTEM').getFunction();
  assert.throws(() => fn(condition, lock, lisp.S_nil), /resumable Lisp call/);
  assert.equal(lock.owner, owner);
  assert.equal(condition.waiters.entries.size, 0);
  const manual = new lisp.ResumableTask(lisp.makeResumable(function*() { yield* lock.acquire(); }));
  assert.throws(() => manual.resume(), /scheduled task/);
  const scheduler = new lisp.CooperativeScheduler();
  await assert.rejects(start(scheduler, function*() { yield* condition.wait(lock); }).completion, /ownership/);
  assert.equal(lock.owner, owner);
  lock.release();
});
