import fs from 'node:fs';
import { createHash } from 'node:crypto';
const output = process.argv[2] ?? '/tmp/valtan-bordeaux-upstream.json';
const bundle = new URL('../dist-bordeaux/bordeaux-upstream.js', import.meta.url);
const report = { status: 'running', complete: false, startedAt: new Date().toISOString(),
  scope: 'Both upstream Bordeaux Threads 0.9.4 suites, with unchanged atomic test bodies explicitly enabled for Valtan',
  bundleSha256: createHash('sha256').update(fs.readFileSync(bundle)).digest('hex') };
const save = () => fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
save();
const errors = [], originalError = console.error;
console.error = (...args) => { errors.push(args.map(String).join(' ')); originalError(...args); };
try {
  await import(bundle);
  if (errors.length || !globalThis.valtanBordeauxUpstream) throw new Error('Upstream suite failed to load');
  report.stage = 'tests'; save();
  const lisp = globalThis.valtanBordeauxKernel;
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 10000 });
  const [results] = await scheduler.spawn(globalThis.valtanBordeauxUpstream).completion;
  report.results = results;
  // Some upstream tests intentionally leave sleeping threads unjoined. Record
  // and clean these only after the suites have returned their results.
  const remaining = lisp.allScheduledTasks();
  report.teardown = remaining.map(task => ({ name: task.name, state: task.state }));
  for (const task of remaining) task.scheduler.cancel(task, new Error('Upstream suite teardown'));
  await Promise.allSettled(remaining.map(task => task.completion));
  report.complete = true;
  if (errors.length || !results.registered || !results.results || results.failed || results.skipped || results.unexecuted.length)
    throw new Error('Upstream suite has failed, skipped or unexecuted cases');
  report.status = 'passed';
} catch (error) { report.status = 'failed'; report.error = String(error); process.exitCode = 1; }
finally { console.error = originalError; report.consoleErrors = errors; report.finishedAt = new Date().toISOString(); save(); }
