import fs from 'node:fs';
import path from 'node:path';
import assert from 'node:assert/strict';

const [phase, directory, mode] = process.argv.slice(2);
await import('../dist-resumable/resumable.js');
const lisp = globalThis.valtanSchedulerKernel;
const symbol = (name, pkg = 'COMMON-LISP') => lisp.intern(name, pkg);
const user = name => symbol(name, 'COMMON-LISP-USER');
const string = value => symbol('SIMPLE-MAKE-STRING').getFunction()(value);
const call = (name, ...args) => symbol(name).getFunction()(...args);
symbol('*PACKAGE*').value = call('FIND-PACKAGE', string('COMMON-LISP-USER'));
const evaluate = text => call('EVAL', call('READ-FROM-STRING', string(text)));
const source = path.join(directory, 'literals.lisp');
const artifact = path.join(directory, 'literals.fasl');
const childSource = path.join(directory, 'child.lisp');
const childArtifact = path.join(directory, 'child.fasl');
const largeSource = path.join(directory, 'large.lisp');
const largeArtifact = path.join(directory, 'large.fasl');
if (phase === 'produce') {
  if (mode === 'ordinary') symbol('*RESUMABLE-FUNCTIONS*', 'COMPILER').value = lisp.S_nil;
  fs.copyFileSync(new URL('./file-literals.lisp', import.meta.url), source);
  fs.writeFileSync(childSource, '(in-package :cl-user) (defun file-literal-child () \'("child" #:child))');
  call('COMPILE-FILE', string(childSource));
  call('COMPILE-FILE', string(source));
  assert.equal(lisp.currentValues()[2], lisp.S_nil);
  assert.equal(user('FILE-LITERAL-ROOT-BEFORE').isFBound(), false);
  // Nine user objects, each externalized once despite two functions, dual
  // function bodies, shared slots and circular initialization references.
  assert.equal(evaluate('(length *literal-method-calls*)'), 9);
  assert.equal(evaluate('(boundp \'+file-literal-value+)'), lisp.S_nil,
    'A DEFCONSTANT in a load form must not execute at compilation time');
  assert.equal(evaluate('(null compiler::*p2-file-literal-dependencies*)'), lisp.S_t,
    'Compilation must not retain its literal graph in the compiler dependency collector');
  assert.equal(evaluate('(length (remove-duplicates *literal-method-calls* :test #\'eq))'), 9);
  assert.equal(evaluate('(eq *literal-root* (eval (list \'quote *literal-root*)))'), lisp.S_t);
  assert.equal(evaluate('(length *literal-method-calls*)'), 9);
  // The list is first encountered inside a load form, so this also exercises
  // aggregate dependency traversal before emitting its 8192 cons cells.
  fs.writeFileSync(largeSource, `(in-package :cl-user)
    (eval-when (:compile-toplevel)
      (defparameter *large-file-source* (loop for i below 8192 collect i)))
    (defun large-file-node () '#.(make-instance 'file-literal-node :value *large-file-source* :next nil))
    (defun large-file-list () '#.*large-file-source*)`);
  call('COMPILE-FILE', string(largeSource));
  // A creation form quoting itself, even through a list, cannot be ordered.
  const badSource = path.join(directory, 'cycle.lisp');
  fs.writeFileSync(badSource, `(in-package :cl-user)
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (defclass bad-file-literal () ())
      (defmethod make-load-form ((x bad-file-literal) &optional environment)
        (declare (ignore environment))
        \`(car ',(list x))))
    (defun bad-file-literal () '#.(make-instance 'bad-file-literal))`);
  assert.throws(() => call('COMPILE-FILE', string(badSource)), /Circular MAKE-LOAD-FORM creation dependency/);
  fs.unlinkSync(source); fs.unlinkSync(childSource); fs.unlinkSync(largeSource); fs.unlinkSync(badSource);
} else if (phase === 'consume') {
  assert.equal(fs.existsSync(source), false);
  assert.equal(fs.existsSync(childSource), false);
  assert.equal(fs.existsSync(largeSource), false);
  user('*LITERAL-NESTED-ARTIFACT*').value = string(childArtifact);
  const scheduler = new lisp.CooperativeScheduler({ checkpointBudget: 10000 });
  async function load() {
    if (mode === 'ordinary') { call('LOAD', string(artifact)); return; }
    let create, initialize;
    user('*LITERAL-CREATION-PROMISE*').value = new Promise(resolve => { create = resolve; });
    user('*LITERAL-INIT-PROMISE*').value = new Promise(resolve => { initialize = resolve; });
    const task = scheduler.spawn(symbol('LOAD'), [string(artifact)]);
    async function waiting() {
      const deadline = performance.now() + 120000;
      while (task.state !== 'waiting') {
        if (task.state === 'failed') throw task.error;
        assert.notEqual(task.state, 'completed', 'LOAD must suspend in the load form');
        assert.ok(performance.now() < deadline, 'Timed out waiting for load form');
        await new Promise(resolve => setTimeout(resolve, 1));
      }
    }
    await waiting(); create(37);
    // Let the scheduler leave the first waiting state before observing the next.
    await new Promise(resolve => setTimeout(resolve, 10));
    await waiting(); initialize(42);
    assert.deepEqual(await task.completion, [lisp.S_t]);
  }
  await load();
  assert.equal(user('CHECK-FILE-LITERALS').getFunction()(), 25);
  // Large graph traversal is a compiler regression. Read this larger artifact
  // synchronously; the main fixture separately tests scheduled LOAD and waits.
  call('LOAD', string(largeArtifact));
  assert.equal(evaluate('(length (large-file-list))'), 8192);
  assert.equal(evaluate('(car (last (large-file-list)))'), 8191);
  assert.equal(evaluate('(eq (large-file-list) (slot-value (large-file-node) \'value))'), lisp.S_t);
  const oldFunction = user('FILE-LITERAL-ROOT-BEFORE').getFunction();
  const oldRoot = oldFunction();
  if (mode !== 'ordinary') {
    const result = await scheduler.spawn(user('FILE-LITERAL-ROOT-AFTER'), []).completion;
    assert.equal(result[0], oldRoot, 'Ordinary and scheduled entries share literals');
  }
  await load();
  assert.equal(user('CHECK-FILE-LITERALS').getFunction()(), 25);
  assert.equal(oldFunction(), oldRoot, 'Old closures retain their own LOAD pool');
  assert.notEqual(user('FILE-LITERAL-ROOT-BEFORE').getFunction()(), oldRoot);
  assert.equal(symbol('*COMPILED-FILE-LITERALS*', 'SYSTEM').value, lisp.S_nil);
} else throw new Error('Unknown phase');
console.log(`FILE-LITERALS-${mode}-${phase}-PASS`);
