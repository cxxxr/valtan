// Portable verifier shared by Node and the fresh browser worker.
export async function verifyScheduledLisp(lisp, fixture) {
  const check = (value, message) => { if (!value) throw new Error(message); };
  const equal = (actual, expected, message) => check(JSON.stringify(actual) === JSON.stringify(expected), message);
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 100000, slicesPerTurn: 8 });
  const log = [], record = (id, round, local) => log.push([id, round, local]);
  const handles = Array.from({ length: 64 }, (_, id) => scheduler.spawn(fixture.run, [id, record]));
  check(log.length === 0, 'Spawn must defer execution');
  const values = await Promise.all(handles.map(handle => handle.completion));
  equal(values, Array.from({ length: 64 }, (_, id) => [id, 3]), 'Task multiple values');
  equal(log, Array.from({ length: 192 }, (_, i) => [i % 64, Math.floor(i / 64), i % 64]),
    '64 Lisp tasks must interleave and preserve their special binding');
  equal(Array.from(fixture.snapshot()), Array(64).fill(3), 'Shared Lisp array');
  check(fixture.global().name === 'GLOBAL', 'Caller binding restored');
  const cpuScheduler = new lisp.CooperativeScheduler({ checkpointBudget: 1 });
  const cpuLog = [];
  const cpuRecord = (id, round, local) => cpuLog.push([id, round, local]);
  const cpuTasks = [0, 1].map(id => cpuScheduler.spawn(fixture.cpu, [id, cpuRecord]));
  equal(await Promise.all(cpuTasks.map(task => task.completion)), [[0, 11], [1, 11]], 'CPU loop results');
  for (const id of [0, 1]) equal(cpuLog.filter(entry => entry[0] === id),
    Array.from({ length: 8 }, (_, round) => [id, round, id]), 'CPU loop local state');
  check(cpuLog.findIndex(entry => entry[0] === 1) < cpuLog.findIndex(entry => entry[0] === 0 && entry[1] === 7),
    'Checkpoint scheduling must interleave loops without explicit yield');
  check(cpuScheduler.tasks.size === 0, 'CPU tasks retired');
  const awaited = scheduler.spawn(fixture.await, [Promise.resolve(42)]);
  const awaitedValues = await awaited.completion;
  check(awaitedValues[0] === 42 && awaitedValues[1].name === 'AWAITED', 'Await values survive cleanup');
  check(fixture.cleaned() === 1, 'Asynchronous Lisp cleanup completed');
  check(fixture.global().name === 'GLOBAL', 'Await binding restored');
  check(scheduler.tasks.size === 0, 'All tasks retired');
  return { tasks: 64, sharedArrayWrites: 192, checkpointOnlyTasks: 2, asynchronousCleanup: true };
}
