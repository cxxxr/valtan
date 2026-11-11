export async function verifyNativeErrors(lisp, fixture) {
  const check = (value, message) => { if (!value) throw new Error(message); };
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 10000 });
  const cases = [
    ['symbolValue', 'UNBOUND-VARIABLE', fixture.missing],
    ['globalValue', 'UNBOUND-VARIABLE', fixture.globalMissing],
    ['undefinedFunction', 'UNDEFINED-FUNCTION', fixture.missing],
    ['symbolFunction', 'UNDEFINED-FUNCTION', fixture.missing],
    ['arity', 'PROGRAM-ERROR', lisp.S_nil],
    ['symbolValueType', 'TYPE-ERROR', 17],
    ['symbolFunctionType', 'TYPE-ERROR', 17],
    ['functionType', 'TYPE-ERROR', 17],
  ];
  globalThis.valtanNativeInvalidFunction = 17;
  let release;
  const gate = new Promise(resolve => { release = resolve; });
  const tasks = cases.map(([name, type, detail], index) => ({
    name, marker: lisp.intern(`NATIVE-ERROR-${index}`, 'KEYWORD'),
    task: scheduler.spawn(fixture.run, [fixture[name], gate,
      lisp.intern(`NATIVE-ERROR-${index}`, 'KEYWORD'), lisp.intern(type, 'COMMON-LISP'), detail]),
  }));
  // Keep rejections observed while checking that all handlers reach the wait.
  const completion = Promise.all(tasks.map(({ task }) => task.completion));
  completion.catch(() => {});
  const deadline = performance.now() + 120000;
  while (!tasks.every(({ task }) => task.state === 'waiting')) {
    const failed = tasks.find(({ task }) => task.state === 'failed' || task.state === 'completed');
    if (failed) { await failed.task.completion; throw new Error(`${failed.name} did not suspend in its handler`); }
    check(performance.now() < deadline, 'Native error handlers did not reach their waits');
    await new Promise(resolve => setTimeout(resolve, 1));
  }
  check(fixture.cleanups() === 0, 'No protected form unwinds before its handler resumes');
  check(fixture.binding().name === 'GLOBAL', 'Suspended handlers restore the host dynamic context');
  check(fixture.ordinary().name === 'CAUGHT', 'Ordinary native error delivery remains synchronous');
  release();
  const values = await completion;
  values.forEach((result, index) => {
    check(result.length === 2 && result[0].name === 'CAUGHT' && result[1] === tasks[index].marker,
      `${tasks[index].name}: handler resumes in its own bindings and invokes the active restart`);
  });
  check(fixture.cleanups() === cases.length, 'Every resumed condition runs asynchronous cleanup exactly once');
  check(scheduler.tasks.size === 0 && fixture.binding().name === 'GLOBAL', 'Tasks retire and restore bindings');
  delete globalThis.valtanNativeInvalidFunction;
  return { cases: cases.length, asynchronousHandlers: true, restarts: true, asynchronousCleanup: true, ordinaryHandler: true };
}
