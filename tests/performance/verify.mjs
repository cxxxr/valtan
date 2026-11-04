import assert from 'node:assert/strict';
const workloads = [];
globalThis.valtanBenchmark = (name, fn, input, expected) => workloads.push({ name, fn, input, expected });
// Loading the bundle also executes the compiler semantic regression assertions.
await import('../dist-performance/performance.js');
assert.equal(workloads.length, 8);
for (const { name, fn, input, expected } of workloads) assert.equal(fn(input), expected, name);
console.log('Compiler semantic regressions and all 8 workload results passed');
