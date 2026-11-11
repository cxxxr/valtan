import assert from 'node:assert/strict';
const errors = [], originalError = console.error;
console.error = (...args) => { errors.push(args); originalError(...args); };
try { await import('../dist-resumable/resumable.js'); }
finally { console.error = originalError; }
assert.equal(errors.length, 0, 'No Lisp toplevel may report an error');
assert.equal(globalThis.valtanResumableChecksPassed, 43);
assert.equal(globalThis.valtanDualChecksPassed, 51);
console.log('43 resumable and 51 dual core/library checks passed');
