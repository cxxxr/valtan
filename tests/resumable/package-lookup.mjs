import assert from 'node:assert/strict';
await import('../dist-resumable/resumable.js');
const lisp = globalThis.valtanSchedulerKernel;
const symbol = name => lisp.intern(name, 'COMMON-LISP');
const call = (name, ...args) => symbol(name).getFunction()(...args);
const string = text => call('SIMPLE-MAKE-STRING', text);
symbol('*PACKAGE*').value = call('FIND-PACKAGE', string('COMMON-LISP-USER'));
const read = text => call('READ-FROM-STRING', string(text));
const scheduler = new lisp.CooperativeScheduler({checkpointBudget: 10000});
const source = `(lambda ()
  (let* ((target (make-package "LOOKUP-TARGET" :nicknames '("Q") :use nil))
         (owner (make-package "LOOKUP-OWNER" :use nil))
         (cl (find-package "COMMON-LISP")) (checks 0))
    (flet ((check (value) (assert value) (incf checks)))
      (unwind-protect
          (progn
            (check (eq target (find-package "LOOKUP-TARGET")))
            (check (eq target (find-package :lookup-target)))
            (check (eq target (find-package #\\Q)))
            (check (eq target (find-package target)))
            (check (equal (multiple-value-list (find-package "Q")) (list target)))
            (check (null (find-package "NO-SUCH-LOOKUP-PACKAGE")))
            (let ((name (make-array 5 :element-type 'character :initial-contents "Qjunk" :fill-pointer 1)))
              (check (eq target (find-package name))))
            (let ((name (make-array 1 :element-type 'character :displaced-to "xQy" :displaced-index-offset 1)))
              (check (eq target (find-package name))))
            (handler-bind ((package-error (lambda (condition)
                                           (declare (ignore condition))
                                           (invoke-restart 'continue))))
              (system:add-package-local-nickname "CL" target owner))
            (let ((*package* owner))
              (check (eq target (find-package "CL")))
              (check (eq cl (find-package cl))))
            (check (eq cl (find-package "CL")))
            (rename-package target "LOOKUP-RENAMED" '("NEW-Q"))
            (check (eq target (find-package "NEW-Q")))
            (check (null (find-package "Q")))
            (check (null (find-package "LOOKUP-TARGET")))
            (let ((*package* owner)) (check (eq target (find-package "CL"))))
            (delete-package target)
            (check (null (find-package "LOOKUP-RENAMED")))
            (check (null (find-package "NEW-Q")))
            (check (eq target (find-package target)))
            (let ((*package* owner)) (check (eq cl (find-package "CL")))))
        (when (package-name target) (delete-package target))
        (delete-package owner)))
    checks))`;
let checks = 0;
for (const mode of ['ordinary', 'scheduled']) {
  const fn = mode === 'ordinary' ? call('COMPILE', lisp.S_nil, read(source)) :
    (await scheduler.spawn(symbol('COMPILE'), [lisp.S_nil, read(source)]).completion)[0];
  const actual = mode === 'ordinary' ? call('FUNCALL', fn) : (await scheduler.spawn(fn).completion)[0];
  assert.equal(actual, 19, mode); checks += actual;
}
assert.equal(scheduler.tasks.size, 0);
console.log('PACKAGE-LOOKUP-CHECKS-PASS ' + JSON.stringify({checks, modes: ['ordinary', 'scheduled']}));
