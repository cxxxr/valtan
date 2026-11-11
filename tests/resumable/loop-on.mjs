import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';

const errors = [], originalError = console.error;
console.error = (...args) => {errors.push(args); originalError(...args);};
await import('../dist-resumable/resumable.js');
const lisp = globalThis.valtanSchedulerKernel;
const symbol = name => lisp.intern(name, 'COMMON-LISP');
const string = value => symbol('SIMPLE-MAKE-STRING').getFunction()(value);
symbol('LOAD').getFunction()(string(fileURLToPath(new URL('./loop-on.lisp', import.meta.url))));
const run = lisp.intern('RUN-CHECKS', 'VALTAN-LOOP-ON-TESTS');
const check = values => {
  assert.equal(values[0].name, 'PASSED');
  assert.equal(values[1], 13);
};
run.getFunction()(); check(lisp.currentValues());
const scheduler = new lisp.CooperativeScheduler({checkpointBudget: 10000});
check(await scheduler.spawn(run, [lisp.S_t]).completion);
console.error = originalError;
assert.equal(errors.length, 0, 'No Lisp toplevel errors');
console.log('13 ordinary and 13 scheduled LOOP ON assertions passed');
