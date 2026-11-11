export async function verifyBordeaux(lisp, fixture) {
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 10000 });
  const passed = [];
  for (const name of Object.keys(fixture)) {
    console.log('BORDEAUX-CHECK-START ' + name);
    const values = await scheduler.spawn(fixture[name]).completion;
    if (values.length !== 1 || values[0].name !== 'PASSED') throw new Error('Bordeaux group failed: ' + name);
    passed.push(name);
    console.log('BORDEAUX-CHECK-PASS ' + name);
  }
  if (scheduler.tasks.size !== 0) throw new Error('Bordeaux test left active threads');
  return { groups: passed };
}
