// Exercise the installed, unmodified CL-PPCRE and its upstream test assertions.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';
import { build } from 'esbuild';

const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
const args = process.argv.slice(2);
function option(name, fallback) {
  const i = args.indexOf(name);
  if (i < 0) return fallback;
  if (!args[i + 1]) throw new Error(`Missing value for ${name}`);
  return args.splice(i, 2)[1];
}
const sourceOption = option('--source', null);
const output = path.resolve(option('--output', path.join(here, 'ppcre-results.json')));
if (args.length) throw new Error(`Unknown arguments: ${args.join(' ')}`);
const sha256 = data => createHash('sha256').update(data).digest('hex');
const lispString = text => '"' + text.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
fs.mkdirSync(path.dirname(output), { recursive: true });
const logPath = output.replace(/\.json$/, '') + '.log';
const log = fs.createWriteStream(logPath);
async function run(command, argv, cwd, timeout = 120000) {
  return await new Promise((resolve, reject) => {
    let stdout = '', stderr = '';
    const child = spawn(command, argv, { cwd, stdio: ['ignore', 'pipe', 'pipe'] });
    const timer = setTimeout(() => child.kill('SIGKILL'), timeout);
    child.stdout.on('data', data => { stdout += data; log.write(data); process.stdout.write(data); });
    child.stderr.on('data', data => { stderr += data; log.write(data); });
    child.on('error', error => { clearTimeout(timer); reject(error); });
    child.on('close', (code, signal) => {
      clearTimeout(timer);
      if (code !== 0) reject(new Error(`${command} failed (${signal ?? code})\n${stderr.slice(-4000)}`));
      else resolve(stdout + stderr);
    });
  });
}
const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-ppcre-'));
const started = Date.now();
const report = { schemaVersion: 1, timestamp: new Date().toISOString(), complete: false,
  compilerSha256: sha256(fs.readFileSync(path.join(root, 'valtan'))), node: process.version,
  adapter: 'Latin-1 input only; upstream library, assertions and skip list unchanged', suites: [] };
const save = () => fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
save();
try {
  const quicklispSetup = path.join(os.homedir(), 'quicklisp/setup.lisp');
  const discovery = sourceOption ? null : await run('sbcl', ['--noinform', '--non-interactive',
    '--eval', '(require :asdf)',
    ...(fs.existsSync(quicklispSetup) ? ['--load', quicklispSetup] : []),
    '--eval', '(format t "~&PPCRE-SOURCE ~A~%" (asdf:system-source-directory (asdf:find-system :cl-ppcre)))'], root);
  const source = path.resolve(sourceOption ?? discovery.match(/^PPCRE-SOURCE (.+)$/m)?.[1] ?? '');
  if (!fs.existsSync(path.join(source, 'cl-ppcre.asd'))) throw new Error('CL-PPCRE source directory not found');
  report.source = source;
  report.sourceFiles = Object.fromEntries([
    ...fs.readdirSync(source).filter(n => n.endsWith('.lisp') || n === 'cl-ppcre.asd'),
    ...['packages.lisp', 'tests.lisp', 'perl-tests.lisp', 'simple', 'perltestdata'].map(n => `test/${n}`),
  ].sort().map(n => [n, sha256(fs.readFileSync(path.join(source, n)))]));
  report.harnessFiles = Object.fromEntries(['ppcre.mjs', 'ppcre-streams.lisp', 'ppcre-main.lisp'].map(n =>
    [n, sha256(fs.readFileSync(path.join(here, n)))]));
  fs.writeFileSync(path.join(dir, 'ppcre-tests.asd'), `(asdf:load-asd #P${lispString(path.join(source, 'cl-ppcre.asd'))})
    (asdf:defsystem "ppcre-tests"
    :depends-on ("cl-ppcre") :serial t :components (
    (:file "streams" :pathname #P${lispString(path.join(here, 'ppcre-streams.lisp'))})
    ${['packages', 'tests', 'perl-tests'].map(n => `(:file "${n}" :pathname #P${lispString(path.join(source, 'test', n + '.lisp'))})`).join('\n')}))\n`);
  fs.copyFileSync(path.join(here, 'ppcre-main.lisp'), path.join(dir, 'main.lisp'));
  const example = fs.readFileSync(path.join(root, 'example/try-using-cl-ppcre/main.lisp'));
  report.exampleSha256 = sha256(example);
  fs.writeFileSync(path.join(dir, 'example.lisp'), example);
  fs.writeFileSync(path.join(dir, 'ppcre.system'), '(defsystem "ppcre" :depends-on ("ppcre-tests") :components ((:file "example") (:file "main")) :target :node :compile-mode :runtime)\n');
  const compilation = await run(path.join(root, 'valtan'), ['build', 'ppcre.system'], dir);
  if (/debugger invoked|caught ERROR|Valtan error:/.test(compilation)) throw new Error('Compilation failed; see log');
  const bundle = path.join(dir, 'ppcre.mjs');
  await build({ entryPoints: [path.join(dir, '.valtan-cache/ppcre.js')], outfile: bundle,
    bundle: true, platform: 'node', format: 'esm', logLevel: 'silent',
    alias: { lisp: path.join(root, 'kernel/lisp.js') } });
  report.bundleSha256 = sha256(fs.readFileSync(bundle));
  save();
  const result = await run(process.execPath, [bundle], dir, 30 * 60 * 1000);
  report.suites = [...result.matchAll(/^PPCRE-SUITE (\S+) (passed|failed) (\d+) (\d+)$/gm)].map(m =>
    ({ name: m[1], status: m[2], cases: Number(m[3]), upstreamSkipped: Number(m[4]) }));
  if (report.suites.length !== 3 || report.suites.some(s => s.status !== 'passed') ||
      result.includes('Valtan error:') || !/^PPCRE-UPSTREAM-PASS$/m.test(result)) {
    throw new Error('Upstream suites did not complete successfully');
  }
  report.complete = true;
  report.status = 'passed';
} catch (error) {
  report.status = 'failed';
  report.error = String(error);
  console.error(error);
  process.exitCode = 1;
} finally {
  report.elapsedSeconds = (Date.now() - started) / 1000;
  if (!report.complete && fs.existsSync(logPath)) {
    report.suites = [...fs.readFileSync(logPath, 'utf8').matchAll(/^PPCRE-SUITE (\S+) (passed|failed) (\d+) (\d+)$/gm)].map(m =>
      ({ name: m[1], status: m[2], cases: Number(m[3]), upstreamSkipped: Number(m[4]) }));
  }
  save();
  log.end();
  fs.rmSync(dir, { recursive: true, force: true });
}
console.log(`CL-PPCRE report: ${output}`);
