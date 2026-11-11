import test from 'node:test';
import assert from 'node:assert/strict';
import * as lisp from '../../kernel/lisp.js';
import './synchronization.test.mjs';
import './sleep.test.mjs';
import './interrupt.test.mjs';

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

test('execution contexts isolate values and suspended backtraces while sharing globals', () => {
  const printed = [];
  lisp.setLispFormatFunction((control, frame) => {
    printed.push(lisp.listToJsArray(frame)[0]);
    return 'frame';
  });
  const x = lisp.makeSymbol('TASK-X'); x.value = 'global';
  const a = lisp.makeResumable(function*() {
    const binding = lisp.bindSpecial(x); x.value = 'a';
    try {
      lisp.values('a', 1);
      yield { kind: 'yield', value: 'a' };
      assert.deepEqual(lisp.currentValues(), ['a', 1]);
      assert.equal(x.value, 'a');
      lisp.getBacktrace();
      return lisp.values('a', 2);
    } finally { lisp.unbindSpecial(binding); }
  });
  const b = lisp.makeResumable(function*() {
    assert.equal(x.value, 'global'); x.value = 'changed';
    lisp.values('b', 3);
    yield { kind: 'yield', value: 'b' };
    assert.deepEqual(lisp.currentValues(), ['b', 3]);
    lisp.getBacktrace();
    return lisp.values();
  });
  const one = new lisp.ResumableTask(a), two = new lisp.ResumableTask(b);
  lisp.values('caller', 9);
  assert.equal(one.resume().value.value, 'a');
  assert.equal(two.resume().value.value, 'b');
  assert.deepEqual(lisp.currentValues(), ['caller', 9]);
  assert.equal(lisp.getBacktrace(), 'Backtrace:\n');
  assert.equal(one.resume().done, true);
  assert.equal(two.resume().done, true);
  assert.deepEqual(one.values, ['a', 2]);
  assert.deepEqual(two.values, []);
  assert.deepEqual(printed, [a, b]);
  assert.equal(x.value, 'changed');
  assert.deepEqual(lisp.currentValues(), ['caller', 9]);
  assert.throws(() => one.resume(), /completed task/);
});

test('task checkpoints bound work and interrupts run suspended cleanup', () => {
  let steps = 0, cleaned = 0;
  const fn = lisp.makeResumable(function*() {
    try {
      for (;;) { steps++; yield lisp.resumableCheckpoint; }
    } finally {
      yield { kind: 'yield', value: 'cleanup' };
      cleaned++;
    }
  });
  const task = new lisp.ResumableTask(fn);
  assert.equal(task.resume(undefined, 3).value, lisp.resumableCheckpoint);
  assert.equal(steps, 3);
  const error = new Error('interrupt');
  assert.equal(task.interrupt(error).value.value, 'cleanup');
  assert.equal(task.done, false);
  assert.throws(() => task.resume(), e => e === error);
  assert.equal(task.done, true);
  assert.equal(cleaned, 1);
  assert.equal(lisp.getBacktrace(), 'Backtrace:\n');
});

test('synchronous resumable callbacks ignore checkpoints but reject suspension with cleanup', () => {
  const plain = lisp.makeResumable(function*() {
    yield lisp.resumableCheckpoint;
    return lisp.values(1, 2);
  });
  assert.equal(plain(), 1);
  assert.deepEqual(lisp.currentValues(), [1, 2]);
  const x = lisp.makeSymbol('SYNC-X'); x.value = 1;
  let cleaned = false;
  const yielding = lisp.makeResumable(function*() {
    const binding = lisp.bindSpecial(x); x.value = 2;
    try { yield { kind: 'yield' }; }
    finally { lisp.unbindSpecial(binding); cleaned = true; }
  });
  assert.throws(() => yielding(), /synchronous JavaScript call boundary/);
  assert.equal(cleaned, true);
  assert.equal(x.value, 1);
});

test('native package enumeration delegates callbacks and preserves visibility', () => {
  const used = lisp.CL_makePackage('TASK-ENUM-USED', [], []);
  const alsoUsed = lisp.CL_makePackage('TASK-ENUM-ALSO-USED', [], []);
  const pkg = lisp.CL_makePackage('TASK-ENUM-PACKAGE', [], []);
  const inherited = lisp.intern('INHERITED', used);
  lisp.intern('SHADOWED', used);
  lisp.intern('PRIVATE', used);
  used.exportSymbol('INHERITED'); used.exportSymbol('SHADOWED');
  alsoUsed.symbolTable.INHERITED = inherited; alsoUsed.exportSymbol('INHERITED');
  lisp.intern('SHADOWED', pkg); pkg.shadowing.add('SHADOWED');
  lisp.intern('EXPORTED', pkg); pkg.exportSymbol('EXPORTED');
  pkg.useList = [used, alsoUsed];
  try {
    for (const [name, expected] of [
      ['MAP-PACKAGE-SYMBOLS', ['SHADOWED', 'EXPORTED']],
      ['MAP-PACKAGE-EXTERNAL-SYMBOLS', ['EXPORTED']],
      ['MAP-PACKAGE-ALL-SYMBOLS', ['SHADOWED', 'EXPORTED', 'INHERITED']],
    ]) {
      const symbol = lisp.intern(name, 'SYSTEM'), ordinary = [];
      symbol.getFunction()(pkg, s => ordinary.push(s.name));
      assert.deepEqual(ordinary, expected);
      const visited = [], callback = lisp.makeResumable(function*(s) {
        yield { kind: 'yield', value: s.name };
        visited.push(s.name);
        return lisp.values('ignored', 'secondary');
      });
      const task = new lisp.ResumableTask(symbol, [pkg, callback]);
      for (const [i, expectedName] of expected.entries()) {
        assert.equal(task.resume().value.value, expectedName);
        assert.equal(visited.length, i);
      }
      assert.equal(task.resume().done, true);
      assert.deepEqual(visited, expected);
      assert.deepEqual(task.values, [lisp.S_nil]);
    }
  } finally {
    lisp.CL_deletePackage(pkg); lisp.CL_deletePackage(alsoUsed); lisp.CL_deletePackage(used);
  }
});

test('native enumeration bounds private-symbol scans and unwinds interrupted callbacks', () => {
  const pkg = lisp.CL_makePackage('TASK-ENUM-BOUNDS', [], []);
  for (let i = 0; i < 20; i++) lisp.intern(`PRIVATE-${i}`, pkg);
  const external = new lisp.ResumableTask(lisp.intern('MAP-PACKAGE-EXTERNAL-SYMBOLS', 'SYSTEM'),
    [pkg, () => assert.fail('private symbol was exposed')]);
  try {
    assert.equal(external.resume(undefined, 3).value, lisp.resumableCheckpoint);
    assert.equal(external.done, false);
    assert.equal(external.resume().done, true);
    let visited = 0, cleaned = false;
    const callback = lisp.makeResumable(function*() {
      visited++;
      try { yield { kind: 'yield', value: 'callback' }; }
      finally { cleaned = true; }
    });
    const task = new lisp.ResumableTask(lisp.intern('MAP-PACKAGE-SYMBOLS', 'SYSTEM'), [pkg, callback]);
    assert.equal(task.resume().value.value, 'callback');
    const error = new Error('cancel enumeration');
    assert.throws(() => task.interrupt(error), e => e === error);
    assert.equal(visited, 1);
    assert.equal(cleaned, true);
    assert.equal(lisp.getBacktrace(), 'Backtrace:\n');
  } finally { lisp.CL_deletePackage(pkg); }
});

test('scheduler interleaves 64 same-heap tasks and gives the host event loop time', async () => {
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 1, slicesPerTurn: 4 });
  const x = lisp.makeSymbol('SCHEDULED-X'); x.value = 'global';
  const log = [], shared = { count: 0 };
  const run = lisp.makeResumable(function*(id) {
    const frame = lisp.bindSpecial(x); x.value = id;
    try {
      for (let round = 0; round < 3; round++) {
        assert.equal(x.value, id);
        assert.equal(lisp.currentScheduledTask().task.context.bindings !== null, true);
        log.push([round, id]); shared.count++;
        yield lisp.resumableCheckpoint;
      }
      return lisp.values(id, shared.count);
    } finally { lisp.unbindSpecial(frame); }
  });
  const handles = Array.from({ length: 64 }, (_, id) => scheduler.spawn(run, [id]));
  assert.equal(log.length, 0, 'spawn must not execute the closure synchronously');
  let hostObserved;
  const host = new Promise(resolve => setTimeout(() => { hostObserved = shared.count; resolve(); }, 0));
  const values = await Promise.all(handles.map(h => h.completion));
  await host;
  assert.ok(hostObserved > 0 && hostObserved < 192);
  assert.deepEqual(log, Array.from({ length: 192 }, (_, i) => [Math.floor(i / 64), i % 64]));
  assert.deepEqual(values, Array.from({ length: 64 }, (_, id) => [id, 192]));
  assert.equal(x.value, 'global');
  assert.equal(lisp.currentScheduledTask(), null);
  assert.equal(scheduler.tasks.size, 0);
});

test('scheduler parks on promises, resumes values and throws rejected waits through cleanup', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  let release;
  const gate = new Promise(resolve => { release = resolve; });
  const error = new Error('rejected wait');
  let cleaned = false;
  const waiting = scheduler.spawn(lisp.makeResumable(function*() {
    const value = yield { kind: 'await', promise: gate };
    try {
      yield { kind: 'await', promise: Promise.reject(error) };
    } catch (caught) {
      assert.equal(caught, error);
      return lisp.values(value, 'caught');
    } finally { cleaned = true; }
  }));
  const other = scheduler.spawn(lisp.makeResumable(function*() {
    yield { kind: 'yield' };
    assert.equal(waiting.state, 'waiting');
    release(42);
    return lisp.values();
  }));
  assert.deepEqual(await waiting.completion, [42, 'caught']);
  assert.deepEqual(await other.completion, []);
  assert.equal(cleaned, true);
});

test('cancelled waits cannot resume a newer cleanup wait', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  let oldRelease, cleanupRelease, entered;
  const oldGate = new Promise(resolve => { oldRelease = resolve; });
  const cleanupGate = new Promise(resolve => { cleanupRelease = resolve; });
  const cleanupEntered = new Promise(resolve => { entered = resolve; });
  const error = new Error('cancel waiting task');
  let cleaned = false;
  const handle = scheduler.spawn(lisp.makeResumable(function*() {
    try { yield { kind: 'await', promise: oldGate }; }
    finally {
      entered();
      assert.equal(yield { kind: 'await', promise: cleanupGate }, 'cleanup');
      cleaned = true;
    }
  }));
  // A second queued task observes the first task parked and cancels it.
  const canceller = scheduler.spawn(lisp.makeResumable(function*() {
    assert.equal(handle.state, 'waiting');
    assert.equal(scheduler.cancel(handle, error), true);
    return lisp.values();
  }));
  await canceller.completion;
  await cleanupEntered;
  oldRelease('obsolete');
  await new Promise(resolve => setTimeout(resolve, 0));
  assert.equal(handle.state, 'waiting');
  assert.equal(cleaned, false);
  cleanupRelease('cleanup');
  await assert.rejects(handle.completion, e => e === error);
  assert.equal(cleaned, true);
  assert.equal(scheduler.cancel(handle), false);
});

test('scheduler preserves self-cancellation and delivers unsupported requests through unwind', async () => {
  const scheduler = new lisp.CooperativeScheduler();
  const error = new Error('self cancel');
  let cleaned = 0;
  const handle = scheduler.spawn(lisp.makeResumable(function*() {
    try {
      scheduler.cancel(lisp.currentScheduledTask(), error);
      yield { kind: 'await', promise: new Promise(() => {}) };
    } finally { cleaned++; }
  }));
  await assert.rejects(handle.completion, e => e === error);
  const invalid = scheduler.spawn(lisp.makeResumable(function*() {
    try { yield { kind: 'unsupported' }; }
    finally { cleaned++; }
  }));
  await assert.rejects(invalid.completion, /Unknown scheduler suspension request/);
  const malformed = scheduler.spawn(lisp.makeResumable(function*() {
    try { yield { kind: 'await', get promise() { throw error; } }; }
    finally { cleaned++; }
  }));
  await assert.rejects(malformed.completion, e => e === error);
  assert.equal(cleaned, 3);
  assert.equal(scheduler.tasks.size, 0);
  assert.equal(lisp.getBacktrace(), 'Backtrace:\n');
});
