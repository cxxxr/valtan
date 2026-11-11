// Measure scheduled APPLY calls and checkpoint propagation through native calls.
// Usage: node tests/performance/native-apply.mjs /absolute/bundle.mjs /tmp/report.json
import fs from 'node:fs';
import assert from 'node:assert/strict';
import {pathToFileURL} from 'node:url';
import {createHash} from 'node:crypto';
const [bundle, output] = process.argv.slice(2);
assert.ok(bundle && output, 'Provide a dual runtime bundle and report path');
await import(pathToFileURL(bundle));
const lisp = globalThis.valtanSchedulerKernel;
const symbol = (name, pkg = 'COMMON-LISP') => lisp.intern(name, pkg);
const string = value => symbol('SIMPLE-MAKE-STRING').getFunction()(value);
const source = `(progn
  (defun cl-user::native-apply-bench (n)
    (let ((value '(73)))
      (dotimes (i n n) (assert (= (apply #'car (list value)) 73)))))
  (defun cl-user::nested-apply-bench (depth n)
    (if (zerop depth) (dotimes (i n n) (identity i))
        (apply #'cl-user::nested-apply-bench (list (1- depth) n)))))`;
symbol('EVAL').getFunction()(symbol('READ-FROM-STRING').getFunction()(string(source)));
const scheduler = new lisp.CooperativeScheduler({checkpointBudget: 100000});
const results = [];
for (const [name, args] of [['NATIVE-APPLY-BENCH', [10000]], ['NESTED-APPLY-BENCH', [20, 10000]]]) {
  const fn = symbol(name, 'COMMON-LISP-USER');
  const warmup = [...args]; warmup[warmup.length - 1] = 1000;
  assert.deepEqual(await scheduler.spawn(fn, warmup).completion, [1000]);
  const samples = [];
  for (let i = 0; i < 7; i++) {
    const start = performance.now();
    const values = await scheduler.spawn(fn, args).completion;
    samples.push(performance.now() - start);
    assert.deepEqual(values, [10000]);
  }
  results.push({name, args, samples, medianMs: [...samples].sort((a, b) => a - b)[3]});
}
const report = {date: new Date().toISOString(), node: process.version,
  scope: 'Scheduled synthetic APPLY workloads; excludes startup and is not a whole McCLIM benchmark',
  bundleSha256: createHash('sha256').update(fs.readFileSync(bundle)).digest('hex'), results};
fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify(report));
