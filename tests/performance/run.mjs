import fs from 'node:fs';
import os from 'node:os';
import { pathToFileURL } from 'node:url';
import { performance } from 'node:perf_hooks';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';

const workloads = [];
globalThis.valtanBenchmark = (name, fn, input, expected) => workloads.push({ name, fn, input, expected });
const bundle = process.argv[2] ? pathToFileURL(process.argv[2]) : new URL('../dist-performance/performance.js', import.meta.url);
await import(bundle.href);
assert.equal(workloads.length, 8, 'Missing benchmark workloads');
const sources = globalThis.valtanBenchmarkSources;
assert.ok(sources?.workloadsSha256 && sources?.semanticsSha256, 'Bundle lacks build-time source identities; rebuild it');
const results = [];
for (const { name, fn, input, expected } of workloads) {
  const run = () => assert.equal(fn(input), expected, name);
  // Warm up each workload for at least 500 ms and ten calls before sampling.
  let start = performance.now(), iterations = 0;
  do { run(); iterations++; } while (performance.now() - start < 500 || iterations < 10);
  const batch = Math.max(1, Math.ceil(iterations * 100 / (performance.now() - start)));
  const samples = [];
  for (let sample = 0; sample < 7; sample++) {
    start = performance.now();
    for (let i = 0; i < batch; i++) run();
    samples.push((performance.now() - start) / batch);
  }
  const medianMs = [...samples].sort((a, b) => a - b)[3];
  results.push({ name, input, expected, batch, medianMs, samples });
  console.error(`${name}: ${medianMs.toFixed(3)} ms`);
}
const sha256 = data => createHash('sha256').update(data).digest('hex');
const report = { node: process.version, cpu: os.cpus()[0].model, date: new Date().toISOString(),
  bundleSha256: sha256(fs.readFileSync(bundle)),
  ...sources, results };
const output = JSON.stringify(report, null, 2) + '\n';
if (process.argv[3]) fs.writeFileSync(process.argv[3], output);
else process.stdout.write(output);
