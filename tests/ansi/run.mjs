import fs from 'node:fs';
import path from 'node:path';
import os from 'node:os';
import { createHash } from 'node:crypto';
import { spawn, execFileSync } from 'node:child_process';
import { createInterface } from 'node:readline';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { auditRegistrations } from './registration-audit.mjs';

const here = path.dirname(fileURLToPath(import.meta.url));
const upstream = path.resolve(here, '../../submodules/ansi-test');
export const revision = 'ebf9c806f383d5dc921c421e0d083e925c64a6cd';
export const manifest = JSON.parse(fs.readFileSync(path.join(here, 'manifest.json')));
const referenceInventory = JSON.parse(fs.readFileSync(path.join(here, 'reference-inventory.json')));
if (referenceInventory.revision !== revision) throw new Error('Reference inventory revision mismatch');

export function runWorker(name, files, { timeout = 120000, mode = 'eval', fixture = false, onProgress, expectedRegistered = null, bundlePath } = {}) {
  return new Promise((resolve) => {
    const result = { name, files, mode, status: 'running', pass: 0, fail: 0, registered: null,
      expectedRegistered, completedFiles: [], errors: [], failures: [], complete: false, timedOut: false,
      unexecuted: null, diagnostics: '' };
    let pending = null;
    const observedNames = new Set();
    let lastPublished = 0;
    let progressTimer;
    const publish = (force = false) => {
      result.pendingTest = pending;
      if (!onProgress) return;
      if (force || Date.now() - lastPublished >= 1000) {
        clearTimeout(progressTimer);
        progressTimer = undefined;
        lastPublished = Date.now();
        onProgress(result);
      } else if (!progressTimer) {
        // Flush the last event even if the worker stops emitting events while
        // loading or running a long test.
        progressTimer = setTimeout(() => publish(true), 1000 - (Date.now() - lastPublished));
      }
    };
    publish(true);
    const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-ansi-work-'));
    if (!fixture) {
      fs.cpSync(upstream, scratch, { recursive: true, filter: source => path.basename(source) !== '.git' });
      // The preceding FILES category creates this directory in an upstream full
      // run. Supply that prerequisite when STREAMS runs in its own process.
      if (files.some(file => file.startsWith('streams/'))) fs.mkdirSync(path.join(scratch, 'sandbox/scratch'), { recursive: true });
    }
    const child = spawn(process.execPath, [path.join(here, 'worker.mjs'), ...files], {
      cwd: scratch,
      env: { ...process.env, VALTAN_ANSI_MODE: mode, VALTAN_ANSI_FIXTURE: fixture ? '1' : '',
        VALTAN_ANSI_BUNDLE: bundlePath ? pathToFileURL(bundlePath).href : '' },
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    const timer = setTimeout(() => { result.timedOut = true; child.kill('SIGKILL'); }, timeout);
    const diagnostic = (text) => { result.diagnostics = (result.diagnostics + text).slice(-16000); };
    child.stderr.on('data', chunk => diagnostic(chunk.toString()));
    createInterface({ input: child.stdout }).on('line', line => {
      if (!line.startsWith('@@ANSI ')) { diagnostic(line + '\n'); return; }
      try {
        const event = JSON.parse(line.slice(7));
        if (event.kind === 'registered') result.registered = Number(event.detail);
        else if (event.kind === 'file-start') result.currentFile = event.name;
        else if (event.kind === 'file-complete') result.completedFiles.push(event.name);
        else if (event.kind === 'test-start') { pending = event.name; observedNames.add(event.name); }
        else if (event.kind === 'pass') { result.pass++; pending = null; }
        else if (event.kind === 'fail') {
          result.fail++; result.failures.push({ test: event.name, reason: event.detail }); pending = null;
        } else if (event.kind === 'complete') result.complete = true;
        else if (event.kind.endsWith('-error')) result.errors.push(event);
        publish();
      } catch (error) { result.errors.push({ kind: 'protocol-error', detail: error.message }); }
    });
    child.on('error', error => result.errors.push({ kind: 'spawn-error', detail: error.message }));
    child.on('close', (code, signal) => {
      clearTimeout(timer);
      clearTimeout(progressTimer);
      fs.rmSync(scratch, { recursive: true, force: true });
      result.exitCode = code; result.signal = signal; result.pendingTest = pending;
      if (result.registered !== null) result.unexecuted = Math.max(0, result.registered - result.pass - result.fail);
      result.unreadFiles = files.filter(file => !result.completedFiles.includes(file));
      result.unregisteredTestsUnknown = result.unreadFiles.length > 0 || result.errors.length > 0 || result.registered === null;
      result.fatalError = /Valtan error:/.test(result.diagnostics);
      result.complete = result.complete && code === 0 && !signal && !result.timedOut &&
        result.errors.length === 0 && result.unreadFiles.length === 0 &&
        !result.fatalError && result.registered === result.pass + result.fail && result.unexecuted === 0;
      result.referenceRegistrationMismatch = expectedRegistered !== null && result.registered !== expectedRegistered;
      if (!fixture && expectedRegistered !== null && referenceInventory.testNames) {
        Object.assign(result, auditRegistrations(files, observedNames, referenceInventory, expectedRegistered));
        if (result.registrationNameMismatch || result.referenceInventoryMismatch || observedNames.size !== result.registered) {
          result.complete = false;
        }
      }
      result.registrationMismatch = expectedRegistered !== null &&
        result.registered !== (result.effectiveExpectedRegistered ?? expectedRegistered);
      if (result.registered === null || result.registered === 0 || result.registrationMismatch) result.complete = false;
      result.status = 'finished';
      publish(true);
      resolve(result);
    });
  });
}

async function main() {
  const args = process.argv.slice(2);
  const categories = [], selectedFiles = [];
  let output = 'ansi-results.json', timeout = 120000, mode = 'eval', concurrency = 1;
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg === '--help') {
      console.log('Usage: node ansi/run.mjs [--category NAME | --file PATH]... [--mode eval|compiled] [--timeout SECONDS] [--jobs COUNT] [--output PATH]\nWithout selectors all 24 upstream categories run. --list lists categories.');
      return;
    }
    if (arg === '--list') { console.log(Object.keys(manifest).join('\n')); return; }
    if (!['--category', '--file', '--mode', '--timeout', '--output', '--jobs'].includes(arg) || !args[i + 1]) throw new Error(`Invalid option: ${arg}`);
    const value = args[++i];
    if (arg === '--category') categories.push(value);
    if (arg === '--file') selectedFiles.push(value);
    if (arg === '--mode') mode = value;
    if (arg === '--output') output = value;
    if (arg === '--timeout') timeout = Number(value) * 1000;
    if (arg === '--jobs') concurrency = Number(value);
  }
  if (!['eval', 'compiled'].includes(mode)) throw new Error('Mode must be eval or compiled');
  if (!Number.isFinite(timeout) || timeout <= 0) throw new Error('Timeout must be positive');
  if (!Number.isSafeInteger(concurrency) || concurrency < 1) throw new Error('Jobs must be a positive integer');
  if (categories.length && selectedFiles.length) throw new Error('Use categories or files, not both');
  const actualRevision = execFileSync('git', ['-C', upstream, 'rev-parse', 'HEAD'], { encoding: 'utf8' }).trim();
  if (actualRevision !== revision) throw new Error(`Expected upstream ${revision}, got ${actualRevision}`);
  if (execFileSync('git', ['-C', upstream, 'status', '--porcelain'], { encoding: 'utf8' }).trim()) throw new Error('Upstream checkout is modified');
  const jobs = selectedFiles.length ? selectedFiles.map(file => {
    const category = Object.keys(manifest).find(key => manifest[key].includes(file));
    if (!category || file.startsWith('auxiliary/')) throw new Error(`Unknown test file: ${file}`);
    // Category auxiliary definitions are loaded in upstream order before the file.
    return [file, [...new Set(manifest[category].slice(0, manifest[category].indexOf(file)).filter(p => p.startsWith('auxiliary/'))), file]];
  }) : (categories.length ? [...new Set(categories)] : Object.keys(manifest)).map(category => {
    if (!manifest[category]) throw new Error(`Unknown category: ${category}`);
    return [category, manifest[category]];
  });
  // A rebuild during a long run must not change the runtime of later workers.
  const bundle = fs.readFileSync(path.resolve(here, '../dist-ansi/ansi.js'));
  const snapshot = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-ansi-'));
  const bundlePath = path.join(snapshot, 'ansi.mjs');
  fs.writeFileSync(bundlePath, bundle);
  const report = { suite: 'ansi-test', revision, mode,
    bundleSha256: createHash('sha256').update(bundle).digest('hex'), startedAt: new Date().toISOString(),
    selection: selectedFiles.length || categories.length ? 'selected' : 'all',
    harness: 'Valtan adapter; upstream DEFTEST bodies and equalp-with-case comparator', results: [] };
  const save = () => {
    const temporary = `${output}.${process.pid}.tmp`;
    fs.writeFileSync(temporary, JSON.stringify(report, null, 2) + '\n');
    fs.renameSync(temporary, output);
  };
  save();
  let nextJob = 0;
  async function work() {
    while (nextJob < jobs.length) {
      const index = nextJob++;
      const [name, files] = jobs[index];
      console.log(`Running ${name} (${files.length} load steps, ${timeout / 1000}s limit)`);
      const expectedRegistered = referenceInventory.categories[name] ?? referenceInventory.files[name] ?? null;
      const result = await runWorker(name, files, { timeout, mode, expectedRegistered, bundlePath, onProgress: current => {
        report.results[index] = current;
        save();
      } });
      save();
      console.log(`${name}: pass=${result.pass}, fail=${result.fail}, registered=${result.registered ?? 'unknown'}, ${result.complete ? 'complete' : 'INCOMPLETE'}`);
    }
  }
  try {
    await Promise.all(Array.from({ length: Math.min(concurrency, jobs.length) }, work));
  } finally {
    fs.rmSync(snapshot, { recursive: true, force: true });
  }
  report.pass = report.results.reduce((n, r) => n + r.pass, 0);
  report.fail = report.results.reduce((n, r) => n + r.fail, 0);
  report.complete = report.results.every(r => r.complete);
  report.passRate = report.complete ? 100 * report.pass / (report.pass + report.fail) : null;
  report.finishedAt = new Date().toISOString(); save();
  console.log(`Report: ${path.resolve(output)}; pass=${report.pass}, fail=${report.fail}, complete=${report.complete}`);
  if (!report.complete || report.fail) process.exitCode = 1;
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  main().catch(error => { console.error(error.message); process.exitCode = 2; });
}
