export async function verifySynchronization(lisp, fixture) {
  const check = (value, message) => { if (!value) throw new Error(message); };
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 100 });
  const waitUntil = async predicate => {
    const deadline = performance.now() + 120000;
    while (!predicate()) {
      if (performance.now() > deadline) throw new Error('Synchronization did not reach its expected state');
      await new Promise(resolve => setTimeout(resolve, 1));
    }
  };
  const spawn = (function_, ...args) => scheduler.spawn(function_, args);
  const counters = Array.from({ length: 64 }, (_, id) => spawn(fixture.counter, id));
  const counterValues = await Promise.all(counters.map(task => task.completion));
  check(fixture.count() === 192, 'Shared counter lost updates across suspension');
  check(counterValues.every((values, id) => values[0] === id && values[1].name === 'COUNTED'), 'Counter task results');
  check(fixture.global().name === 'GLOBAL', 'Lock tasks leaked dynamic bindings');

  const consumers = Array.from({ length: 4 }, (_, id) => spawn(fixture.consumer, id));
  await waitUntil(() => fixture.available.waiters.entries.size === 4);
  const producers = Array.from({ length: 4 }, (_, id) => spawn(fixture.producer, id));
  const consumed = await Promise.all(consumers.map(task => task.completion));
  await Promise.all(producers.map(task => task.completion));
  check(consumed.every((values, id) => values[1] === id && values[0].length === 16), 'Consumer results');
  const items = consumed.flatMap(values => values[0]).sort((a, b) => a - b);
  check(items.length === 64 && items.every((value, index) => value === index), 'Queue lost or duplicated an item');
  check(fixture.available.waiters.entries.size === 0, 'Queue retained a waiting consumer');

  const waiters = Array.from({ length: 4 }, () => spawn(fixture.eventWait));
  await waitUntil(() => fixture.event.waiters.entries.size === 4);
  await spawn(fixture.signal).completion;
  check((await Promise.all(waiters.map(task => task.completion))).every(values => values[0].name === 'NOTIFIED'), 'Broadcast did not wake all waiters');
  const timed = await spawn(fixture.timeout).completion;
  check(timed[0] === lisp.S_nil && timed[1].name === 'TIMED-OUT', 'Condition timeout result or multiple values');
  await Promise.all(Array.from({ length: 8 }, () => spawn(fixture.recursive).completion));
  check(fixture.recursiveCount() === 8, 'Recursive lock released too early');

  const cancelled = spawn(fixture.cancel);
  await waitUntil(() => fixture.cancelEvent.waiters.entries.size === 1);
  const error = new Error('Cancel condition waiter');
  scheduler.cancel(cancelled, error);
  let caught;
  try { await cancelled.completion; } catch (value) { caught = value; }
  check(caught === error && fixture.cleaned() === 1, 'Cancellation did not unwind the Lisp waiter');
  check(fixture.cancelLock.owner === null && fixture.cancelEvent.waiters.entries.size === 0, 'Cancellation leaked lock ownership or a waiter');
  check(scheduler.tasks.size === 0 && fixture.global().name === 'GLOBAL', 'Synchronization tasks or bindings leaked');
  return { checks: 12, counterTasks: 64, counterUpdates: 192, producers: 4, consumers: 4,
    transferredItems: 64, broadcastWaiters: 4, recursiveTasks: 8, cancelledWaiters: 1 };
}
