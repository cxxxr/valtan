import assert from 'node:assert/strict';
const workloads = [];
globalThis.valtanBenchmark = (name, fn, input, expected) => workloads.push({ name, fn, input, expected });
// Loading the bundle also executes the compiler semantic regression assertions.
// Generated toplevels report Lisp errors and continue loading. Treat those as
// failures even if the later workload registrations still execute.
const errors = [], originalError = console.error;
console.error = (...args) => { errors.push(args); originalError(...args); };
try { await import('../dist-performance/performance.js'); }
finally { console.error = originalError; }
assert.equal(errors.length, 0, 'Compiler semantic regression errors were reported');
assert.equal(workloads.length, 8);
for (const { name, fn, input, expected } of workloads) assert.equal(fn(input), expected, name);
console.log('Compiler semantic regressions and all 8 workload results passed');
