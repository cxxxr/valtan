import fs from 'node:fs';
import assert from 'node:assert/strict';
const [phase, source] = process.argv.slice(2);
const errors = [], originalError = console.error;
console.error = (...args) => { errors.push(args); originalError(...args); };
try { await import('../dist-resumable/resumable.js'); }
finally { console.error = originalError; }
assert.equal(errors.length, 0);
const lisp = globalThis.valtanSchedulerKernel;
const symbol = name => lisp.intern(name, 'COMMON-LISP');
const userSymbol = name => lisp.intern(name, 'COMMON-LISP-USER');
const string = value => symbol('SIMPLE-MAKE-STRING').getFunction()(value);
const artifact = source.replace(/\.lisp$/, '.fasl');
assert.equal(userSymbol('RESUMED-ARTIFACT-FUNCTION').isFBound(), false);
if (phase === 'produce') {
  fs.writeFileSync(source, `(in-package :cl-user)
(defvar *resumable-artifact-promise* nil)
(defparameter *resumable-artifact-result* (system::%resumable-await *resumable-artifact-promise*))
(defun resumed-artifact-function () (values *resumable-artifact-result* :restored))
`);
  symbol('COMPILE-FILE').getFunction()(string(source));
  assert.equal(lisp.currentValues()[2], lisp.S_nil);
  assert.equal(userSymbol('RESUMED-ARTIFACT-FUNCTION').isFBound(), false);
  assert.ok(fs.existsSync(artifact));
  fs.unlinkSync(source);
} else if (phase === 'consume') {
  assert.equal(fs.existsSync(source), false);
  let release;
  userSymbol('*RESUMABLE-ARTIFACT-PROMISE*').value = new Promise(resolve => { release = resolve; });
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 10000 });
  const task = scheduler.spawn(symbol('LOAD'), [string(artifact)]);
  const deadline = performance.now() + 120000;
  while (task.state !== 'waiting') {
    if (task.state === 'failed') throw task.error;
    if (task.state === 'completed' || performance.now() > deadline) throw new Error('Artifact did not suspend');
    await new Promise(resolve => setTimeout(resolve, 1));
  }
  assert.equal(userSymbol('RESUMED-ARTIFACT-FUNCTION').isFBound(), false);
  release(73);
  assert.deepEqual(await task.completion, [lisp.S_t]);
  assert.equal(userSymbol('RESUMED-ARTIFACT-FUNCTION').getFunction()(), 73);
  assert.equal(lisp.currentValues()[1].name, 'RESTORED');
} else throw new Error('Unknown artifact phase');
console.log(`RESUMABLE-ARTIFACT-${phase.toUpperCase()}-PASS`);
