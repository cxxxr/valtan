export async function verifyResumableEval(lisp, fixture, { fileCompilation = true } = {}) {
  const check = (value, message) => { if (!value) throw new Error(message); };
  const equal = (actual, expected, message) => check(JSON.stringify(actual) === JSON.stringify(expected), message);
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 10000 });
  const gate = () => { let release; const promise = new Promise(resolve => { release = resolve; }); return { promise, release }; };
  const waitUntil = async predicate => {
    const deadline = performance.now() + 120000;
    while (!predicate()) {
      if (performance.now() > deadline) throw new Error('EVAL test did not reach its wait');
      await new Promise(resolve => setTimeout(resolve, 1));
    }
  };
  const run = async (function_, ...args) => scheduler.spawn(function_, args).completion;
  const awaited = await run(fixture.await, Promise.resolve(42));
  check(awaited[0] === 42 && awaited[1].name === 'LOCAL', 'EVAL await multiple values');
  check(fixture.cleaned() === 1 && fixture.global().name === 'GLOBAL', 'EVAL cleanup and binding restoration');
  const macroGate = gate(), macro = scheduler.spawn(fixture.macrolet, [macroGate.promise]);
  await waitUntil(() => macro.state === 'waiting');
  check(fixture.order() === 5, 'Definition before wait must execute first');
  fixture.setOrder(71); macroGate.release();
  equal(await macro.completion, [71, 71], 'Later macro expansion must observe the resumed definition order');
  const symbolGate = gate(), symbol = scheduler.spawn(fixture.symbolMacro, [symbolGate.promise]);
  await waitUntil(() => symbol.state === 'waiting');
  check(fixture.order() === 9, 'SYMBOL-MACROLET assignment');
  fixture.setOrder(81); symbolGate.release();
  equal(await symbol.completion, [81, 81], 'Local macro/declaration environment survives wait');
  const when = await run(fixture.evalWhen, Promise.resolve());
  check(when[0].name === 'EXECUTED' && when[1] === 17, 'EVAL-WHEN situations');
  equal(await run(fixture.empty), [], 'EVAL zero values');
  check((await run(fixture.nullEnvironment))[0] === lisp.S_nil, 'EVAL must not inherit lexical bindings');
  const caught = await run(fixture.throw, Promise.resolve());
  check(caught[0].name === 'CAUGHT' && caught[1] === 23, 'Dynamic THROW across EVAL');
  const sharedGate = gate();
  const one = scheduler.spawn(fixture.shared, [sharedGate.promise, 1]);
  const two = scheduler.spawn(fixture.shared, [sharedGate.promise, 2]);
  await waitUntil(() => one.state === 'waiting' && two.state === 'waiting');
  sharedGate.release(99);
  equal(await one.completion, [1, 99, 1, 1], 'First invocation temporary isolation');
  equal(await two.completion, [2, 99, 2, 1], 'Second invocation temporary isolation');
  check(fixture.ltvCount() === 1, 'Shared compiled top-level load-time value runs once');
  const loaded = await run(fixture.load, Promise.resolve());
  check(loaded[0] === 32 && loaded[1].name === 'LOADED', 'Source LOAD resumes in order');
  const entry = await scheduler.spawn(lisp.intern('EVAL', 'COMMON-LISP'), [fixture.entry]).completion;
  check(entry[0].name === 'ENTRY' && entry[1] === 55, 'Direct scheduled EVAL entry');
  const stream = await run(fixture.compiledStream, Promise.resolve());
  check(stream[0] === 52 && stream[1].name === 'COMPILED-STREAM', 'Compiled stream LOAD can await');
  if (fileCompilation) {
    const compiled = await run(fixture.compiledFile, Promise.resolve());
    check(compiled[0] === 42 && compiled[1].name === 'COMPILED', 'COMPILE-FILE artifact LOAD can await');
  }
  check(scheduler.tasks.size === 0 && fixture.global().name === 'GLOBAL', 'Evaluation tasks retire and restore context');
  return { checks: fileCompilation ? 18 : 17, sharedTopLevelInvocations: 2, sourceLoad: true, compiledStreamLoad: true, compileFileLoad: fileCompilation };
}
