import assert from 'node:assert/strict';

// Compare task-visible slices across old, counted and mixed generator code.
export function verifyCheckpoints(lisp) {
  const checkpoint = lisp.resumableCheckpoint;
  assert.equal(lisp.resumableCheckpointDue(), true);
  let checks = 1;
  function run(mode, depth) {
    const trace = [], slices = [];
    const fn = lisp.makeResumable(function* visit(level) {
      for (let i = 0; i < 7; i++) {
        if (mode === 'legacy' || (mode === 'mixed' && i % 2 === 0) || lisp.resumableCheckpointDue())
          yield checkpoint;
        trace.push([level, i]);
        if (level > 0 && i === 3) yield* visit(level - 1);
        if (i === 5) yield {kind: 'yield', value: level};
      }
      return lisp.values1(73);
    });
    const task = new lisp.ResumableTask(fn);
    for (let i = 0; i < 200; i++) {
      const step = task.resume(undefined, 1 + i % 5);
      assert.equal(task.context.checkpointBudget, null, 'Budget must be scoped to one advance');
      slices.push({done: step.done, kind: step.done ? 'done' : step.value.kind, trace: trace.slice()});
      if (step.done) {assert.deepEqual(task.values, [73]); return slices;}
    }
    throw new Error('Checkpoint fixture did not finish');
  }
  for (const depth of [0, 5]) {
    const legacy = run('legacy', depth);
    for (const mode of ['counted', 'mixed']) {
      assert.deepEqual(run(mode, depth), legacy, `${mode} checkpoints preserve slices at depth ${depth}`);
      checks++;
    }
  }

  // Nested task advancement must restore the outer task's remaining budget.
  const inner = new lisp.ResumableTask(lisp.makeResumable(function*() {
    for (;;) {if (lisp.resumableCheckpointDue()) yield checkpoint;}
  }));
  const outer = new lisp.ResumableTask(lisp.makeResumable(function*() {
    assert.equal(lisp.resumableCheckpointDue(), false);
    assert.equal(inner.resume(undefined, 1).value, checkpoint);
    assert.equal(lisp.resumableCheckpointDue(), false);
    if (lisp.resumableCheckpointDue()) yield checkpoint;
    return lisp.values1(42);
  }));
  assert.equal(outer.resume(undefined, 3).value, checkpoint);
  assert.equal(outer.resume(undefined, 1).done, true);
  assert.deepEqual(outer.values, [42]); checks++;

  // Exceptional exits also relinquish the budget, even after a checkpoint.
  const failure = new Error('checkpoint interruption');
  const interrupted = new lisp.ResumableTask(lisp.makeResumable(function*() {
    if (lisp.resumableCheckpointDue()) yield checkpoint;
    return lisp.values1(0);
  }));
  assert.equal(interrupted.resume(undefined, 1).value, checkpoint);
  assert.throws(() => interrupted.interrupt(failure, 4), error => error === failure);
  assert.equal(interrupted.context.checkpointBudget, null);
  assert.equal(lisp.resumableCheckpointDue(), true); checks++;

  const observations = [];
  const synchronous = lisp.makeResumable(function*() {
    const due = lisp.resumableCheckpointDue(); observations.push(due);
    if (due) yield checkpoint;
    return lisp.values1(19);
  });
  assert.equal(synchronous(), 19);
  assert.deepEqual(observations, [false], 'Synchronous facade has no observable checkpoint'); checks++;
  const direct = lisp.getResumableImplementation(synchronous)();
  assert.equal(direct.next().value, checkpoint, 'Direct iterator consumer still sees checkpoints');
  assert.deepEqual(direct.next(), {done: true, value: 19}); checks++;
  const throwsSynchronously = lisp.makeResumable(function*() {throw failure;});
  assert.throws(() => throwsSynchronously(), error => error === failure);
  assert.equal(lisp.synchronousResumptionActive(), false);
  assert.equal(lisp.resumableCheckpointDue(), true); checks++;
  const nestedTrace = [];
  const nestedTask = new lisp.ResumableTask(lisp.makeResumable(function*() {
    for (let i = 0; i < 4; i++) {
      if (lisp.resumableCheckpointDue()) yield checkpoint;
      nestedTrace.push(i);
    }
    return lisp.values1(4);
  }));
  lisp.makeResumable(function*() {
    assert.equal(nestedTask.resume(undefined, 1).value, checkpoint);
    assert.deepEqual(nestedTrace, [], 'Explicit nested task still respects its budget inside a synchronous facade');
    assert.equal(lisp.synchronousResumptionActive(), true);
    return lisp.values1(0);
  })();
  assert.equal(nestedTask.resume(undefined, 10).done, true);
  assert.deepEqual(nestedTask.values, [4]); checks++;
  return {checks, legacyCompatibility: true, identicalSlices: true, nestedContexts: true,
    exceptionalCleanup: true, synchronousFacades: true, directIterators: true};
}
