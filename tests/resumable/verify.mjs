import assert from 'node:assert/strict';
const errors = [], originalError = console.error;
console.error = (...args) => { errors.push(args); originalError(...args); };
try { await import('../dist-resumable/resumable.js'); }
finally { console.error = originalError; }
assert.equal(errors.length, 0, 'No Lisp toplevel may report an error');
assert.equal(globalThis.valtanResumableChecksPassed, 43);
assert.equal(globalThis.valtanDualChecksPassed, 51);
assert.equal(globalThis.valtanCallbackChecksPassed, 23);
console.log('43 resumable, 51 dual core/library and 23 native callback checks passed');

const { verifyScheduledLisp } = await import('./scheduler-checks.mjs');
await verifyScheduledLisp(globalThis.valtanSchedulerKernel, globalThis.valtanScheduledTestSetup());
console.log('64 scheduled Lisp tasks, 2 checkpoint-only loops, shared array, local bindings, await and asynchronous cleanup passed');

const { verifyResumableEval } = await import('./eval-checks.mjs');
const evalResult = await verifyResumableEval(globalThis.valtanSchedulerKernel, globalThis.valtanEvalTestSetup());
console.log('RESUMABLE-EVAL-CHECKS-PASS ' + JSON.stringify(evalResult));

const { verifySynchronization } = await import('./synchronization-checks.mjs');
const synchronization = await verifySynchronization(globalThis.valtanSchedulerKernel, globalThis.valtanSynchronizationTestSetup());
console.log('SYNCHRONIZATION-CHECKS-PASS ' + JSON.stringify(synchronization));

const { verifySleep } = await import('./sleep-checks.mjs');
const sleep = await verifySleep(globalThis.valtanSchedulerKernel, globalThis.valtanSleepTestSetup());
console.log('SLEEP-CHECKS-PASS ' + JSON.stringify(sleep));

const { verifyInterrupts } = await import('./interrupt-checks.mjs');
const interrupts = await verifyInterrupts(globalThis.valtanSchedulerKernel, globalThis.valtanInterruptTestSetup());
console.log('INTERRUPT-CHECKS-PASS ' + JSON.stringify(interrupts));
