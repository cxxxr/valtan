// Compile real large Lisp bodies; verify both entries and record generated code.
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import path from 'node:path';
import {pathToFileURL} from 'node:url';

const [bundle, output] = process.argv.slice(2);
assert.ok(bundle && output, 'Usage: node compile-benchmark.mjs BUNDLE OUTPUT');
const sha = value => createHash('sha256').update(value).digest('hex');
const report = {
  status: 'running',
  startedAt: new Date().toISOString(),
  bundleSha256: sha(fs.readFileSync(bundle)),
  scope: 'Scheduled COMPILE of actual Lisp assignment bodies; two timed samples per size, after one small warmup. Not an end-to-end McCLIM benchmark.',
  results: [],
};
const save = () => fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
save();

try {
  await import(pathToFileURL(path.resolve(bundle)));
  const lisp = globalThis.valtanSchedulerKernel;
  const s = name => lisp.intern(name, 'COMMON-LISP');
  const string = value => s('SIMPLE-MAKE-STRING').getFunction()(value);
  const scheduler = new lisp.CooperativeScheduler({checkpointBudget: 10000});
  for (const size of [16, 64, 256]) {
    const source = `(lambda () (let ((value 0)) ${'(setq value (+ value 1)) '.repeat(size)} value))`;
    const form = s('READ-FROM-STRING').getFunction()(string(source));
    const samples = [];
    for (let trial = 0; trial < (size === 16 ? 1 : 2); trial++) {
      const started = performance.now();
      const [fn, warnings, failure] = await scheduler.spawn(s('COMPILE'), [lisp.S_nil, form]).completion;
      const milliseconds = performance.now() - started;
      assert.equal(warnings, lisp.S_nil);
      assert.equal(failure, lisp.S_nil);
      assert.equal(s('FUNCALL').getFunction()(fn), size);
      assert.deepEqual(await scheduler.spawn(fn).completion, [size]);
      samples.push({
        milliseconds,
        ordinaryCodeSha256: sha(fn.toString()),
        resumableCodeSha256: sha(lisp.getResumableImplementation(fn).toString()),
      });
      console.log('COMPILE-BENCH', size, trial, milliseconds);
    }
    if (size !== 16) report.results.push({size, sourceSha256: sha(source), samples});
    save();
  }
  report.status = 'passed';
} catch (error) {
  report.status = 'failed';
  report.error = String(error?.stack ?? error);
  process.exitCode = 1;
} finally {
  report.finishedAt = new Date().toISOString();
  save();
}
