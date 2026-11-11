// Build each upstream library in a fresh compiler process and execute real calls.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { createHash } from 'node:crypto';
import { build } from 'esbuild';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
const args = process.argv.slice(2);
const jobsIndex = args.indexOf('--jobs');
const jobs = jobsIndex < 0 ? 2 : Number(args.splice(jobsIndex, 2)[1]);
if (!Number.isInteger(jobs) || jobs < 1 || jobs > 16) throw new Error('--jobs must be between 1 and 16');
const timeoutIndex = args.indexOf('--timeout');
const runtimeTimeoutSeconds = timeoutIndex < 0 ? 600 : Number(args.splice(timeoutIndex, 2)[1]);
if (!Number.isFinite(runtimeTimeoutSeconds) || runtimeTimeoutSeconds <= 0) throw new Error('--timeout must be positive seconds');
const aheadOfTime = args.includes('--ahead-of-time');
if (aheadOfTime) args.splice(args.indexOf('--ahead-of-time'), 1);
const outputIndex = args.indexOf('--output');
const output = path.resolve(outputIndex < 0 ? path.join(here, 'results.json') : args.splice(outputIndex, 2)[1]);
const cases = JSON.parse(fs.readFileSync(path.join(here, 'cases.json')));
for (const name of args) {
  if (!cases.some(c => c.system === name)) throw new Error(`Unknown test system: ${name}`);
}
const selected = args.length ? cases.filter(c => args.includes(c.system)) : cases;
const sha256 = data => createHash('sha256').update(data).digest('hex');
function sourceReleases(cache) {
  const releases = new Map();
  const walk = directory => {
    for (const entry of fs.readdirSync(directory, { withFileTypes: true })) {
      const file = path.join(directory, entry.name);
      if (entry.isDirectory()) { walk(file); continue; }
      if (!file.endsWith('.js')) continue;
      const original = path.sep + path.relative(cache, file).slice(0, -3);
      const match = original.match(/\/software\/([^/]+)\/(.*)/);
      if (!match || !fs.existsSync(original)) continue;
      if (!releases.has(match[1])) releases.set(match[1], new Map());
      releases.get(match[1]).set(match[2], fs.readFileSync(original));
    }
  };
  walk(cache);
  return [...releases].sort(([a], [b]) => a.localeCompare(b)).map(([release, files]) => {
    const hash = createHash('sha256');
    for (const [name, data] of [...files].sort(([a], [b]) => a.localeCompare(b))) {
      hash.update(name).update('\0').update(data).update('\0');
    }
    return { release, files: files.size, sha256: hash.digest('hex') };
  });
}
const report = {
  schemaVersion: 1, runtimeTimeoutSeconds, mode: aheadOfTime ? 'ahead-of-time' : 'runtime', timestamp: new Date().toISOString(), node: process.version,
  compilerSha256: sha256(fs.readFileSync(path.join(root, 'valtan'))),
  casesSha256: sha256(fs.readFileSync(path.join(here, 'cases.json'))),
  scope: 'Selected API smoke tests, not full upstream test suites or all of Quicklisp',
  complete: false, selected: selected.map(c => c.system),
  results: [],
};
const logs = output.replace(/\.json$/, '') + '-logs';
fs.mkdirSync(logs, { recursive: true });
const diagnostic = text => {
  const start = text.search(/(?:Unhandled |debugger invoked|While evaluating|(?:Valtan error: )?Error:)/);
  return text.slice(start < 0 ? 0 : start, (start < 0 ? 0 : start) + 2000);
};
const work = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-quicklisp-'));
const execFileAsync = promisify(execFile);
const run = async (command, argv, cwd, timeout = 120000) => {
  try {
    const execution = execFileAsync(command, argv,
      { cwd, encoding: 'utf8', timeout, maxBuffer: 16 * 1024 * 1024 });
    execution.child.stdin.end();
    const result = await execution;
    return { ok: true, output: result.stdout + result.stderr };
  } catch (error) {
    const timedOut = !!error.killed && error.signal === 'SIGTERM';
    return { ok: false, timedOut, signal: error.signal, output: `${error.stdout ?? ''}${error.stderr ?? ''}\n${timedOut ? `Error: Timed out after ${timeout} ms\n` : ''}${String(error)}` };
  }
};
async function runCase(test) {
    const dir = path.join(work, test.system.replaceAll('/', '-'));
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(path.join(dir, 'smoke.system'),
      `(defsystem "smoke" :depends-on (${[...(test.dependsOn ?? []), test.system].map(s => JSON.stringify(s)).join(' ')}) :components ((:file "main")) :target :node :compile-mode :${aheadOfTime ? 'ahead-of-time' : 'runtime'})\n`);
    fs.writeFileSync(path.join(dir, 'main.lisp'), `${test.form}\n(write-line "QUICKLISP-SMOKE-PASS")\n`);
    const result = { system: test.system, status: 'failed', stage: 'compile', testSha256: sha256(test.form) };
    const compiled = await run(path.join(root, 'valtan'), ['build', 'smoke.system'], dir);
    let log = compiled.output;
    if (!compiled.ok) {
      result.diagnostic = diagnostic(compiled.output);
    } else {
      result.sourceReleases = sourceReleases(path.join(dir, '.valtan-cache'));
      result.stage = 'bundle';
      try {
        const bundle = path.join(dir, 'smoke.mjs');
        await build({ entryPoints: [path.join(dir, '.valtan-cache/smoke.js')], outfile: bundle,
          bundle: true, platform: 'node', format: 'esm', target: 'node18', logLevel: 'silent',
          alias: { lisp: path.join(root, 'kernel/lisp.js') } });
        result.bundleSha256 = sha256(fs.readFileSync(bundle));
        result.stage = 'run';
        const executed = await run(process.execPath, [bundle], dir, runtimeTimeoutSeconds * 1000);
        log += '\n' + executed.output;
        result.runtimeTimeoutSeconds = runtimeTimeoutSeconds;
        if (executed.timedOut) result.timedOut = true;
        if (executed.ok && !executed.output.includes('Valtan error:') && executed.output.split(/\r?\n/).includes('QUICKLISP-SMOKE-PASS')) {
          result.status = 'passed';
        } else result.diagnostic = diagnostic(executed.output);
      } catch (error) { result.diagnostic = String(error); }
    }
    report.results.push(result);
    fs.writeFileSync(path.join(logs, test.system.replaceAll('/', '-') + '.log'), log);
    fs.mkdirSync(path.dirname(output), { recursive: true });
    fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
    console.log(`${result.status.padEnd(6)} ${test.system} (${result.stage})`);
}
let next = 0;
try {
  // Each worker uses its own compiler, target process and cache directory.
  const workers = Array.from({ length: Math.min(jobs, selected.length) }, async () => {
    while (next < selected.length) await runCase(selected[next++]);
  });
  // Wait for every worker before removing the shared temporary parent.
  const outcomes = await Promise.allSettled(workers);
  const rejected = outcomes.find(result => result.status === 'rejected');
  if (rejected) throw rejected.reason;
} finally {
  fs.rmSync(work, { recursive: true, force: true });
}
const passed = report.results.filter(r => r.status === 'passed').length;
report.complete = true;
report.passed = passed;
report.failed = report.results.length - passed;
report.results.sort((a, b) => report.selected.indexOf(a.system) - report.selected.indexOf(b.system));
fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
console.log(`${passed}/${report.results.length} API smoke tests passed; report: ${output}`);
process.exitCode = passed === selected.length ? 0 : 1;
