import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
await import('../dist-resumable/resumable.js');
const lisp = globalThis.valtanSchedulerKernel;
const symbol = (name, pkg = 'COMMON-LISP') => lisp.intern(name, pkg);
const call = (name, ...args) => symbol(name).getFunction()(...args);
const string = value => call('SIMPLE-MAKE-STRING', value);
symbol('*PACKAGE*').value = call('FIND-PACKAGE', string('COMMON-LISP-USER'));
const read = source => call('READ-FROM-STRING', string(source));
const scheduler = new lisp.CooperativeScheduler({checkpointBudget: 10000});
let checks = 0;
const equal = (actual, expected) => {assert.equal(actual, expected); checks++;};
const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-deftype-'));
try {
  for (const mode of ['ordinary', 'scheduled']) {
    const run = async source => mode === 'ordinary' ? call('EVAL', read(source)) :
      (await scheduler.spawn(symbol('EVAL'), [read(source)]).completion)[0];
    const nested = `${mode}-nested-type`, local = `${mode}-local-type`;
    await run(`(let ((limit 3)) (deftype ${local} () (list 'integer 0 limit)))`);
    equal(await run(`(typep 3 '${local})`), lisp.S_t);
    equal(await run(`(typep 4 '${local})`), lisp.S_nil);
    const fn = await run(`(compile nil '(lambda (limit)
      (deftype ${nested} () (list 'integer 0 limit))))`);
    equal(await run(`(cl::deftype-expander '${nested})`), lisp.S_nil);
    if (mode === 'ordinary') call('FUNCALL', fn, 7);
    else await scheduler.spawn(fn, [7]).completion;
    equal(await run(`(typep 7 '${nested})`), lisp.S_t);
    equal(await run(`(typep 8 '${nested})`), lisp.S_nil);

    const type = `${mode}-tracking-clause`, bounded = `${mode}-bounded-type`;
    const macro = `${mode}-check-clause`, accessor = `${mode}-compiled-clause`;
    const loaded = `*${mode}-type-file-loaded*`;
    const source = path.join(directory, mode + '.lisp');
    fs.writeFileSync(source, `(in-package :cl-user)
      (deftype ${type} () '(member :pointer-motion :keyboard))
      (deftype ${bounded} (&optional high) (list 'integer 0 high))
      (defmacro ${macro} (name)
        (assert (typep name '${type}))
        (assert (typep 100 '${bounded}))
        (assert (typep 2 '(${bounded} 3)))
        (assert (not (typep 4 '(${bounded} 3))))
        (list 'quote name))
      (defparameter ${loaded} :loaded)
      (defun ${accessor} () (${macro} :pointer-motion))`);
    let artifact, warnings, failure;
    if (mode === 'ordinary') {
      artifact = call('COMPILE-FILE', string(source));
      [, warnings, failure] = lisp.currentValues();
    } else {
      [artifact, warnings, failure] = await scheduler.spawn(symbol('COMPILE-FILE'), [string(source)]).completion;
    }
    equal(warnings, lisp.S_nil);
    equal(failure, lisp.S_nil);
    equal(await run(`(boundp '${loaded})`), lisp.S_nil);
    equal(await run(`(fboundp '${accessor})`), lisp.S_nil);
    equal(await run(`(typep :pointer-motion '${type})`), lisp.S_t);
    // Remove compile-time definitions to prove LOAD installs both expanders.
    await run(`(setf (cl::deftype-expander '${type}) nil
                    (cl::deftype-expander '${bounded}) nil)`);
    fs.unlinkSync(source);
    if (mode === 'ordinary') call('LOAD', artifact);
    else await scheduler.spawn(symbol('LOAD'), [artifact]).completion;
    equal(await run(loaded), symbol('LOADED', 'KEYWORD'));
    equal(await run(`(${accessor})`), symbol('POINTER-MOTION', 'KEYWORD'));
    equal(await run(`(typep :keyboard '${type})`), lisp.S_t);
    equal(await run(`(typep :other '${type})`), lisp.S_nil);
    equal(await run(`(typep 100 '${bounded})`), lisp.S_t);
    equal(await run(`(typep 4 '(${bounded} 3))`), lisp.S_nil);
  }
  equal(scheduler.tasks.size, 0);
} finally {
  fs.rmSync(directory, {recursive: true, force: true});
}
console.log('DEFTYPE-CHECKS-PASS ' + JSON.stringify({checks, compileTimeExpansion: true,
  nestedLexicalEnvironment: true, noNestedCompileTimeEffects: true,
  optionalDefaultWildcard: true, sourceDeletedBeforeLoad: true}));
