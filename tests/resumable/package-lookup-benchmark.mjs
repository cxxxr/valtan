// Measure actual FIND-PACKAGE calls with many packages and global nicknames.
import fs from 'node:fs';
import path from 'node:path';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {pathToFileURL} from 'node:url';
const [bundle, output] = process.argv.slice(2);
assert.ok(bundle && output, 'Usage: package-lookup-benchmark.mjs BUNDLE OUTPUT');
const sha = bytes => createHash('sha256').update(bytes).digest('hex');
const report = {status: 'running', startedAt: new Date().toISOString(),
  scope: 'Ordinary and scheduled FIND-PACKAGE with 128 additional packages; two timed samples after warmup. Not an end-to-end McCLIM measurement.',
  bundleSha256: sha(fs.readFileSync(bundle)), packages: 128, iterations: 200, results: []};
const save = () => fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
save();
try {
  await import(pathToFileURL(path.resolve(bundle)));
  const lisp = globalThis.valtanSchedulerKernel;
  const s = (name, pkg = 'COMMON-LISP') => lisp.intern(name, pkg);
  const call = (name, ...args) => s(name).getFunction()(...args);
  const string = value => call('SIMPLE-MAKE-STRING', value);
  const scheduler = new lisp.CooperativeScheduler({checkpointBudget: 10000});
  let target;
  for (let i = 0; i < report.packages; i++) {
    target = call('MAKE-PACKAGE', string(`LOOKUP-BENCH-${i}`), s('USE', 'KEYWORD'), lisp.S_nil,
      s('NICKNAMES', 'KEYWORD'), lisp.jsArrayToList([string(`LOOKUP-NICK-${i}`)]));
  }
  const source = '(lambda (name iterations) (let ((found nil)) (dotimes (i iterations found) (setf found (find-package name)))))';
  const fn = call('COMPILE', lisp.S_nil, call('READ-FROM-STRING', string(source)));
  report.sourceSha256 = sha(source);
  for (const mode of ['ordinary', 'scheduled']) {
    const run = async (name, n) => mode === 'ordinary' ? call('FUNCALL', fn, name, n) :
      (await scheduler.spawn(fn, [name, n]).completion)[0];
    for (const name of [`LOOKUP-BENCH-${report.packages - 1}`, `LOOKUP-NICK-${report.packages - 1}`]) {
      const value = string(name);
      assert.equal(await run(value, 5), target);
      const samples = [];
      for (let i = 0; i < 2; i++) {
        const start = performance.now();
        assert.equal(await run(value, report.iterations), target);
        samples.push(performance.now() - start);
      }
      report.results.push({mode, name, milliseconds: samples}); save();
      console.log('PACKAGE-LOOKUP-BENCH', mode, name, JSON.stringify(samples));
    }
  }
  assert.equal(scheduler.tasks.size, 0);
  report.status = 'passed';
} catch (error) {
  report.status = 'failed'; report.error = String(error?.stack ?? error); process.exitCode = 1;
} finally {report.finishedAt = new Date().toISOString(); save();}
