// Run the unchanged upstream dynamic variable/slot tests in a scheduled worker.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import assert from 'node:assert/strict';
import {spawn, spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {hash} from './bundle.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
if (args.length > 1 || args.some(arg => arg.startsWith('--')))
  throw new Error('Usage: dynamic-vars.mjs [report.json]');
const output = path.resolve(args[0] ?? '/tmp/valtan-mcclim-dynamic-vars.json');
const work = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-dynamic-vars-'));
const probeOutput = path.join(work, 'probe.json');
const report = {status: 'running', complete: false, startedAt: new Date().toISOString(), work,
  scope: 'All unchanged upstream Tests/dynamic-vars.lisp cases, including 64-thread metaclass and 4096-variable stress workloads, in a fresh Chrome worker; not GUI coverage',
  targetFeatures: ['valtan-resumable', 'clim-mp']};
const save = () => fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
save();
try {
  const manifest = path.join(work, 'source-plan.json');
  const planned = spawnSync('sbcl', ['--script', path.join(here, 'upstream-plan.lisp'), manifest, 'mcclim/test'],
    {encoding: 'utf8', timeout: 120000, maxBuffer: 16 * 1024 * 1024});
  fs.writeFileSync(path.join(work, 'plan.log'), (planned.stdout ?? '') + (planned.stderr ?? ''));
  assert.equal(planned.status, 0, planned.stderr ?? String(planned.error));
  const plan = JSON.parse(fs.readFileSync(manifest));
  const files = plan.files.filter(file => path.relative(plan.root, file) === 'Tests/dynamic-vars.lisp');
  assert.equal(files.length, 1, 'Dynamic variable tests must belong to the upstream test plan');
  const source = fs.readFileSync(files[0], 'utf8');
  const fixture = fs.readFileSync(path.join(here, 'dynamic-vars-package.lisp'), 'utf8') + source +
    fs.readFileSync(path.join(here, 'dynamic-vars-summary.lisp'), 'utf8');
  const checkFile = path.join(work, 'checks.lisp');
  const afterLoad = path.join(work, 'after-load.mjs');
  fs.writeFileSync(checkFile, fixture);
  fs.writeFileSync(path.join(work, 'upstream-source.lisp'), source);
  fs.copyFileSync(path.join(here, 'dynamic-vars-after.mjs'), afterLoad);
  Object.assign(report, {asdfVersion: plan.asdfVersion,
    upstreamDefinition: {file: plan.definition, sha256: hash(fs.readFileSync(plan.definition))},
    source: {file: files[0], sha256: hash(source)}, checksSha256: hash(fixture),
    harnessHashes: Object.fromEntries(['dynamic-vars.mjs', 'dynamic-vars-package.lisp',
      'dynamic-vars-summary.lisp', 'dynamic-vars-after.mjs', 'probe.mjs'].map(name =>
      [name, hash(fs.readFileSync(path.join(here, name)))]))});
  save();
  const terminal = await new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [path.join(here, 'probe.mjs'), 'clim-core/system',
      probeOutput, checkFile, '--browser', ...report.targetFeatures, '--depends-on=fiveam',
      `--after-load=${afterLoad}`, '--runtime-timeout=10800000'], {stdio: 'inherit'});
    child.on('error', reject);
    child.on('exit', (code, signal) => resolve({code, signal}));
  });
  report.probe = JSON.parse(fs.readFileSync(probeOutput));
  const log = fs.readFileSync(probeOutput.replace(/\.json$/, '') + '-logs/run.log', 'utf8');
  const line = log.split('\n').find(line => line.startsWith('DYNAMIC-VARS-RESULT '));
  if (line) report.checks = JSON.parse(line.slice('DYNAMIC-VARS-RESULT '.length));
  assert.deepEqual(terminal, {code: 0, signal: null});
  assert.equal(report.probe.status, 'passed');
  assert.equal(report.probe.complete, true);
  assert.ok(report.checks?.results > 0, 'No test results');
  assert.equal(report.checks.registered, 13);
  assert.equal(report.checks.failed, 0);
  assert.equal(report.checks.skipped, 0);
  assert.deepEqual(report.checks.unexecuted, []);
  assert.equal(hash(fs.readFileSync(files[0])), report.source.sha256, 'Upstream source changed during execution');
  report.complete = true; report.status = 'passed';
} catch (error) {
  report.status = 'failed'; report.error = String(error); process.exitCode = 1;
} finally {
  if (fs.existsSync(probeOutput)) report.probe = JSON.parse(fs.readFileSync(probeOutput));
  report.finishedAt = new Date().toISOString(); save();
}
console.log(`McCLIM dynamic variables: ${report.status}; report: ${output}`);
