// Exercise metadata through EVAL and through a source-deleted compiled artifact.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {fileURLToPath} from 'node:url';

const [phase, source] = process.argv.slice(2);
if (!phase) {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-introspection-'));
  try {
    const source = path.join(directory, 'checks.lisp');
    fs.copyFileSync(new URL('./introspection.lisp', import.meta.url), source);
    for (const phase of ['eval', 'produce', 'consume']) {
      const terminal = await new Promise((resolve, reject) => {
        const child = spawn(process.execPath, [fileURLToPath(import.meta.url), phase, source],
          {stdio: 'inherit'});
        child.on('error', reject);
        child.on('exit', (code, signal) => resolve({code, signal}));
      });
      assert.deepEqual(terminal, {code: 0, signal: null}, phase);
    }
  } finally { fs.rmSync(directory, {recursive: true, force: true}); }
} else {
  const errors = [], originalError = console.error;
  console.error = (...args) => { errors.push(args); originalError(...args); };
  await import('../dist-resumable/resumable.js');
  const lisp = globalThis.valtanSchedulerKernel;
  const symbol = name => lisp.intern(name, 'COMMON-LISP');
  const string = value => symbol('SIMPLE-MAKE-STRING').getFunction()(value);
  const artifact = source.replace(/\.lisp$/, '.fasl');
  if (phase === 'produce') {
    symbol('COMPILE-FILE').getFunction()(string(source));
    assert.equal(lisp.currentValues()[2], lisp.S_nil);
    assert.notEqual(symbol('FIND-PACKAGE').getFunction()(string('VALTAN-INTROSPECTION-TESTS')), lisp.S_nil);
    symbol('FIND-SYMBOL').getFunction()(string('ORIGINAL'), string('VALTAN-INTROSPECTION-TESTS'));
    assert.equal(lisp.currentValues()[1].name, 'EXTERNAL');
    assert.equal(lisp.intern('ORIGINAL', 'VALTAN-INTROSPECTION-TESTS').isFBound(), false);
    assert.ok(fs.existsSync(artifact));
    fs.unlinkSync(source);
  } else {
    assert.equal(fs.existsSync(source), phase === 'eval');
    symbol('LOAD').getFunction()(string(phase === 'eval' ? source : artifact));
    const run = lisp.intern('RUN-CHECKS', 'VALTAN-INTROSPECTION-TESTS');
    const check = values => { assert.equal(values[0].name, 'PASSED'); assert.equal(values[1], 38); };
    run.getFunction()(); check(lisp.currentValues());
    const scheduler = new lisp.CooperativeScheduler({checkpointBudget: 10000});
    check(await scheduler.spawn(run, []).completion);
    console.log('38 ordinary and 38 scheduled introspection assertions passed');
  }
  console.error = originalError;
  assert.equal(errors.length, 0, 'No Lisp toplevel errors');
  console.log(`INTROSPECTION-${phase.toUpperCase()}-PASS`);
}
