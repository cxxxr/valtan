// Execute every upstream FLEXI-STREAMS comparison scenario and all nine suites.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';
import { build } from 'esbuild';
import { normalizeFlexiCase, summarizeFlexiWorker } from './flexi-report.mjs';
const here = path.dirname(fileURLToPath(import.meta.url)), root = path.resolve(here, '../..');
const args = process.argv.slice(2);
function option(name, fallback) {
  const i = args.indexOf(name); if (i < 0) return fallback;
  if (!args[i + 1]) throw new Error(`Missing ${name} value`);
  return args.splice(i, 2)[1];
}
const output = path.resolve(option('--output', path.join(here, 'flexi-results.json')));
const jobs = Number(option('--jobs', String(Math.min(16, os.availableParallelism()))));
if (args.length || !Number.isInteger(jobs) || jobs < 1 || jobs > 64) throw new Error('Expected --jobs 1..64 and/or --output path');
const hash = data => createHash('sha256').update(data).digest('hex');
const str = s => '"' + s.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-flexi-upstream-'));
const logs = output.replace(/\.json$/, '') + '-logs';
fs.mkdirSync(logs, { recursive: true });
const report = { schemaVersion: 1, timestamp: new Date().toISOString(), complete: false, jobs,
  scope: 'All original FLEXI-STREAMS suites; comparison scenarios partitioned without removing any direction, encoding, file or assertion',
  compilerSha256: hash(fs.readFileSync(path.join(root, 'valtan'))), node: process.version,
  harnessFiles: Object.fromEntries(['flexi.mjs', 'flexi-shard-main.lisp', 'flexi-report.mjs'].map(name => [name, hash(fs.readFileSync(path.join(here, name)))])), workers: [] };
const save = () => fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
async function run(command, argv, cwd, logName, timeout = 120000) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, argv, { cwd, stdio: ['ignore', 'pipe', 'pipe'] });
    const log = fs.createWriteStream(path.join(logs, logName));
    let result = ''; const timer = setTimeout(() => child.kill('SIGKILL'), timeout);
    for (const stream of [child.stdout, child.stderr]) stream.on('data', chunk => { result += chunk; log.write(chunk); });
    child.on('error', e => { clearTimeout(timer); log.end(); reject(e); });
    child.on('close', (code, signal) => { clearTimeout(timer); log.end();
      if (code !== 0) reject(new Error(`${command} failed (${signal ?? code}): ${result.slice(-1500)}`)); else resolve(result); });
  });
}
save();
try {
  const setup = path.join(os.homedir(), 'quicklisp/setup.lisp');
  const native = await run('sbcl', ['--noinform', '--non-interactive', '--eval', '(require :asdf)',
    ...(fs.existsSync(setup) ? ['--load', setup] : []), '--eval', '(asdf:load-system "flexi-streams-test")', '--eval',
    '(progn (format t "FLEXI-SOURCE ~A~%" (asdf:system-source-directory "flexi-streams")) (let ((i 0) (*print-pretty* nil)) (dolist (entry flexi-streams-test::*test-files*) (dolist (scenario (flexi-streams-test::create-test-combinations (first entry) (second entry))) (format t "FLEXI-CASE ~D ~S~%" i scenario) (incf i)))))'], root, 'reference.log');
  const source = native.match(/^FLEXI-SOURCE (.+)$/m)?.[1];
  const expected = [...native.matchAll(/^FLEXI-CASE (\d+) (.+)$/gm)].map(m => ({ index: Number(m[1]), scenario: normalizeFlexiCase(m[2]) }));
  if (!source || !expected.length || expected.some((c, i) => c.index !== i)) throw new Error('Incomplete host comparison plan');
  report.source = source; report.expectedCases = expected;
  report.sourceFiles = {};
  const walk = (relative = '') => { for (const e of fs.readdirSync(path.join(source, relative), { withFileTypes: true })) {
    if (e.name.startsWith('.')) continue; const name = path.join(relative, e.name);
    if (e.isDirectory()) walk(name); else if (!/\.(fasl|fsl|fas|o)$/.test(name)) report.sourceFiles[name] = hash(fs.readFileSync(path.join(source, name)));
  }}; walk();
  fs.writeFileSync(path.join(dir, 'app.system'), '(defsystem "app" :depends-on ("flexi-streams") :components ((:file "prelude") (:file "main")) :target :node :compile-mode :runtime)');
  fs.writeFileSync(path.join(dir, 'prelude.lisp'), `(defparameter *flexi-test-source* #p${str(source)})\n`);
  fs.copyFileSync(path.join(here, 'flexi-shard-main.lisp'), path.join(dir, 'main.lisp'));
  const compiled = await run(path.join(root, 'valtan'), ['build', 'app.system'], dir, 'build.log');
  if (/debugger invoked|caught ERROR|Valtan error:/.test(compiled)) throw new Error('Compilation failed');
  const outfile = path.join(dir, 'app.mjs');
  await build({ entryPoints: [path.join(dir, '.valtan-cache/app.js')], outfile, bundle: true,
    platform: 'node', format: 'esm', logLevel: 'silent', alias: { lisp: path.join(root, 'kernel/lisp.js') } });
  report.bundleSha256 = hash(fs.readFileSync(outfile)); save();
  console.log(`Running ${expected.length * 8} file comparisons across ${jobs} workers, plus eight other complete suites`);
  await Promise.all(Array.from({ length: jobs }, async (_, index) => {
    const worker = { index, complete: false, status: 'failed' };
    try {
      const result = await run(process.execPath, [outfile, String(index), String(jobs), path.join(dir, `output-${index}`) + '/'], dir, `worker-${index}.log`, 120 * 60 * 1000);
      Object.assign(worker, summarizeFlexiWorker(result, expected, index, jobs));
    } catch (error) { worker.error = String(error); }
    report.workers.push(worker); save(); console.log(`Worker ${index}: ${worker.status}, complete=${worker.complete}`);
  }));
  report.workers.sort((a, b) => a.index - b.index);
  report.complete = report.workers.length === jobs && report.workers.every(w => w.complete);
  report.comparisons = report.workers.reduce((n, w) => n + (w.cases?.length ?? 0) * 8, 0);
  report.status = report.complete && report.workers.every(w => w.status === 'passed') ? 'passed' : 'failed';
  if (report.status !== 'passed') process.exitCode = 1;
} catch (error) { report.error = String(error); report.status = 'failed'; process.exitCode = 1; }
finally { save(); fs.rmSync(dir, { recursive: true, force: true }); }
console.log(`FLEXI-STREAMS ${report.status}; report: ${output}`);
