export async function verifySleep(lisp, fixture) {
  const check = (value, message) => { if (!value) throw new Error(message); };
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 100000 });
  const started = performance.now();
  const sleeper = scheduler.spawn(fixture.sleep);
  const other = scheduler.spawn(fixture.other);
  let hostRanWhileSleeping = false;
  const host = new Promise(resolve => setTimeout(() => {
    hostRanWhileSleeping = fixture.finished() === lisp.S_nil; resolve();
  }, 0));
  const values = await sleeper.completion;
  check(values[0] === lisp.S_nil && values[1].name === 'SLEPT', 'SLEEP result or multiple values');
  check(performance.now() - started >= 20, 'Rational sleep returned before its duration');
  check((await other.completion)[0] === 42, 'Other task did not finish');
  check(fixture.interleaved() !== lisp.S_nil, 'SLEEP blocked another Lisp task');
  await host;
  check(hostRanWhileSleeping, 'SLEEP blocked the host event loop');
  check((await scheduler.spawn(fixture.float).completion)[0] === lisp.S_nil, 'Float sleep result');
  check((await scheduler.spawn(fixture.zero).completion)[0] === lisp.S_nil, 'Zero sleep result');
  check((await scheduler.spawn(fixture.invalid).completion)[0].name === 'REJECTED', 'Invalid duration did not signal TYPE-ERROR');
  const cancelled = scheduler.spawn(fixture.cancel);
  const deadline = performance.now() + 120000;
  while (fixture.cancelStarted() === lisp.S_nil || cancelled.state !== 'waiting') {
    if (performance.now() > deadline) throw new Error('Cancellation sleep did not start');
    await new Promise(resolve => setTimeout(resolve, 1));
  }
  const error = new Error('Cancel sleeping Lisp');
  scheduler.cancel(cancelled, error);
  let caught;
  try { await cancelled.completion; } catch (value) { caught = value; }
  check(caught === error && fixture.cleaned() === 1, 'Sleep cancellation did not run asynchronous cleanup');
  check(scheduler.tasks.size === 0 && fixture.global().name === 'GLOBAL', 'Sleep tasks or special bindings leaked');
  return { checks: 10, rational: true, float: true, zero: true,
    taskInterleaving: true, hostProgress: true, asynchronousCancellationCleanup: true };
}
