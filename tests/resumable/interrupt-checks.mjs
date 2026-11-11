export async function verifyInterrupts(lisp, fixture) {
  const check = (value, message) => { if (!value) throw new Error(message); };
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 100000 });
  const gate = () => { let release; const promise = new Promise(resolve => { release = resolve; }); return { promise, release }; };
  const waitUntil = async predicate => {
    const deadline = performance.now() + 120000;
    while (!predicate()) {
      if (performance.now() > deadline) throw new Error('Lisp interrupt did not reach its expected state');
      await new Promise(resolve => setTimeout(resolve, 1));
    }
  };
  const parked = async function_ => {
    const wait = gate(), target = scheduler.spawn(function_, [wait.promise]);
    await waitUntil(() => target.state === 'waiting');
    return { target, wait };
  };
  const { target, wait } = await parked(fixture.root);
  const callback = gate();
  scheduler.interrupt(target, fixture.callback, [callback.promise]);
  await waitUntil(() => target.frames.length === 2 && target.state === 'waiting');
  wait.release(42);
  await new Promise(resolve => setTimeout(resolve, 0));
  check(!target.task.done && target.frames.length === 2, 'Root resumed before the interrupt completed');
  check(fixture.global().name === 'GLOBAL', 'Interrupted dynamic bindings escaped to caller');
  callback.release();
  const values = await target.completion;
  check(values[0].name === 'ORIGINAL' && values[1] === 7 && values[2].name === 'MODIFIED', 'Callback values or bindings corrupted the interrupted result');

  const handled = await parked(fixture.handler);
  scheduler.interrupt(handled.target, fixture.error);
  const handlerValues = await handled.target.completion;
  check(handlerValues[0].name === 'HANDLED' && handlerValues[1].name === 'HANDLED-TARGET', 'Interrupt error did not reach the target Lisp handler');
  check(fixture.cleaned() === 1, 'Interrupt handler transfer skipped unwind cleanup');

  const caught = await parked(fixture.catch);
  scheduler.interrupt(caught.target, fixture.throw);
  const catchValues = await caught.target.completion;
  check(catchValues[0].name === 'CAUGHT' && catchValues[1] === 17, 'THROW did not reach the target CATCH');
  check(fixture.cleaned() === 2, 'Nonlocal interrupt exit skipped asynchronous cleanup');

  const signalled = await parked(fixture.signalRoot);
  scheduler.interrupt(signalled.target, fixture.signal);
  await waitUntil(() => fixture.seen() === 1 && signalled.target.frames.length === 1);
  check(signalled.target.state === 'waiting', 'Returning handler resumed an unresolved original wait');
  signalled.wait.release();
  check((await signalled.target.completion)[0] === 1, 'Resumable target handler did not run');

  const jumped = await parked(fixture.goRoot);
  scheduler.interrupt(jumped.target, fixture.jump());
  const jumpValues = await jumped.target.completion;
  check(jumpValues[0].name === 'JUMPED' && jumpValues[1] === 23, 'GO did not reach the active target TAGBODY');

  const cancelled = await parked(fixture.cancelRoot), cancelGate = gate();
  scheduler.interrupt(cancelled.target, fixture.cancelCallback, [cancelGate.promise]);
  await waitUntil(() => cancelled.target.frames.length === 2 && cancelled.target.state === 'waiting');
  const error = new Error('Cancel target and callback');
  scheduler.cancel(cancelled.target, error);
  let received;
  try { await cancelled.target.completion; } catch (value) { received = value; }
  check(received === error, 'Interrupted task cancellation changed the error');
  check(fixture.order().map(value => value.name).join(',') === 'CALLBACK,ROOT', 'Lisp interrupt cleanup order or dynamic bindings');
  check(scheduler.tasks.size === 0 && fixture.global().name === 'GLOBAL', 'Interrupt tasks or bindings leaked');
  return { checks: 13, targetDynamicBindings: true, multipleValues: true,
    handlerCase: true, resumableHandlerBind: true, catchThrow: true, nonlocalGo: true,
    asynchronousCancellationCleanup: true };
}
