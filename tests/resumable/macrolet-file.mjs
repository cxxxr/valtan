import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
await import('../dist-resumable/resumable.js');
const lisp = globalThis.valtanSchedulerKernel;
const symbol = name => lisp.intern(name, 'COMMON-LISP');
const call = (name, ...args) => symbol(name).getFunction()(...args);
const string = value => call('SIMPLE-MAKE-STRING', value);
symbol('*PACKAGE*').value = call('FIND-PACKAGE', string('COMMON-LISP-USER'));
const read = source => call('READ-FROM-STRING', string(source));
const scheduler = new lisp.CooperativeScheduler({checkpointBudget: 10000});
const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-macrolet-file-'));
let checks = 0;
const equal = (actual, expected) => {assert.equal(actual, expected); checks++;};
try {
  for (const mode of ['ordinary', 'scheduled']) {
    const run = async source => mode === 'ordinary' ? call('EVAL', read(source)) :
      (await scheduler.spawn(symbol('EVAL'), [read(source)]).completion)[0];
    await run('(defvar *macrolet-file-events* nil)');
    await run('(setf *macrolet-file-events* nil)');
    await run("(fmakunbound 'macrolet-file-answer)");
    await run("(fmakunbound 'macrolet-file-nested)");
    const source = path.join(directory, mode + '.lisp');
    fs.writeFileSync(source, `(in-package :cl-user)
      (macrolet ((install () '(progn (eval-when (:compile-toplevel)
                                       (push :installed *macrolet-file-events*))))
                 (use-installed ()
                   (assert (member :installed *macrolet-file-events*))
                   42))
        (install)
        (defun macrolet-file-answer () (use-installed)))
      (macrolet ((helper () :local-macro))
        (symbol-macrolet ((marker :local-symbol-macro))
          (eval-when (:compile-toplevel)
            (push (list :compile (helper) marker) *macrolet-file-events*))
          (eval-when (:load-toplevel)
            (push (list :load (helper) marker) *macrolet-file-events*))))
      (defun macrolet-file-nested ()
        (macrolet ((helper () :nested))
          (eval-when (:compile-toplevel) (push :wrong-compile *macrolet-file-events*))
          (eval-when (:load-toplevel) (push :wrong-load *macrolet-file-events*))
          (eval-when (:execute) (push (helper) *macrolet-file-events*))))`);
    let artifact, warnings, failure;
    if (mode === 'ordinary') {
      artifact = call('COMPILE-FILE', string(source));
      [, warnings, failure] = lisp.currentValues();
    } else [artifact, warnings, failure] = await scheduler.spawn(symbol('COMPILE-FILE'), [string(source)]).completion;
    equal(warnings, lisp.S_nil); equal(failure, lisp.S_nil);
    equal(await run("(fboundp 'macrolet-file-answer)"), lisp.S_nil);
    equal(await run("(fboundp 'macrolet-file-nested)"), lisp.S_nil);
    equal(await run("(equal *macrolet-file-events* '((:compile :local-macro :local-symbol-macro) :installed))"), lisp.S_t);
    await run('(setf *macrolet-file-events* nil)');
    fs.unlinkSync(source);
    if (mode === 'ordinary') call('LOAD', artifact);
    else await scheduler.spawn(symbol('LOAD'), [artifact]).completion;
    equal(await run('(macrolet-file-answer)'), 42);
    equal(await run("(equal *macrolet-file-events* '((:load :local-macro :local-symbol-macro)))"), lisp.S_t);
    await run('(macrolet-file-nested)');
    equal(await run("(equal *macrolet-file-events* '(:nested (:load :local-macro :local-symbol-macro)))"), lisp.S_t);
    await run(`(macrolet ((helper () :evaluated))
      (eval-when (:compile-toplevel :load-toplevel) (push :wrong-eval *macrolet-file-events*))
      (eval-when (:execute) (push (helper) *macrolet-file-events*)))`);
    equal(await run('(first *macrolet-file-events*)'), lisp.intern('EVALUATED', 'KEYWORD'));
    equal(await run('(length *macrolet-file-events*)'), 3);
  }
  equal(scheduler.tasks.size, 0);
} finally {fs.rmSync(directory, {recursive: true, force: true});}
console.log('MACROLET-FILE-CHECKS-PASS ' + JSON.stringify({checks, modes: ['ordinary', 'scheduled'],
  topLevelExpansion: true, localCompileAndLoadEnvironment: true, sourceDeleted: true}));
