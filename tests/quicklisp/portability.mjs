// Run unmodified upstream Babel and FLEXI-STREAMS tests with target adapters.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawn } from 'node:child_process';
import { createHash } from 'node:crypto';
import { build } from 'esbuild';
import { summarizeUpstream } from './portability-report.mjs';
import { combinePortability } from './portability-combine.mjs';
const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
const args = process.argv.slice(2);
function option(name, fallback) {
  const i = args.indexOf(name);
  if (i < 0) return fallback;
  if (!args[i + 1]) throw new Error(`Missing ${name} value`);
  return args.splice(i, 2)[1];
}
const suite = option('--suite', 'all');
const jobs = option('--jobs', String(Math.min(16, os.availableParallelism())));
if (!Number.isInteger(Number(jobs)) || Number(jobs) < 1 || Number(jobs) > 64) throw new Error('Expected --jobs 1..64');
if (!['all', 'babel', 'flexi'].includes(suite)) throw new Error('Expected --suite all, babel or flexi');
const output = path.resolve(option('--output', path.join(here, 'portability-results.json')));
if (args.length) throw new Error(`Unknown arguments: ${args.join(' ')}`);
const hash = data => createHash('sha256').update(data).digest('hex');
const str = s => '"' + s.replaceAll('\\', '\\\\').replaceAll('"', '\\"') + '"';
const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-portability-upstream-'));
fs.mkdirSync(path.dirname(output), { recursive: true });
const log = fs.createWriteStream(output.replace(/\.json$/, '') + '.log');
const report = { schemaVersion: 2, suite, timestamp: new Date().toISOString(), complete: false,
  compilerSha256: hash(fs.readFileSync(path.join(root, 'valtan'))), node: process.version,
  scope: suite === 'babel' ? 'Complete default Babel upstream test selection with an assertion-framework adapter' : 'Babel and FLEXI-STREAMS upstream test orchestration',
  adapters: 'Babel: Stefil assertion/registration API and ASDF fixture-path lookup only. No library source, test assertions, character ranges or upstream feature exclusions changed.',
};
const save = () => fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
async function run(command, argv, cwd, timeout = 120000) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, argv, { cwd, stdio: ['ignore', 'pipe', 'pipe'] });
    let result = '';
    const timer = setTimeout(() => child.kill('SIGKILL'), timeout);
    for (const stream of [child.stdout, child.stderr]) stream.on('data', chunk => {
      result += chunk; log.write(chunk);
      if (stream === child.stdout) process.stdout.write(chunk);
    });
    child.on('error', e => { clearTimeout(timer); reject(e); });
    child.on('close', (code, signal) => {
      clearTimeout(timer);
      if (code !== 0) reject(new Error(`${command} failed (${signal ?? code}): ${result.slice(-3000)}`));
      else resolve(result);
    });
  });
}
function treeHashes(directory) {
  const result = {};
  function walk(relative = '') {
    for (const entry of fs.readdirSync(path.join(directory, relative), { withFileTypes: true })) {
      if (entry.name.startsWith('.')) continue;
      const name = path.join(relative, entry.name);
      if (entry.isDirectory()) walk(name);
      else if (!/\.(fasl|fsl|fas|o)$/.test(name)) result[name] = hash(fs.readFileSync(path.join(directory, name)));
    }
  }
  walk(); return result;
}
save();
if (suite !== 'babel') {
  try {
    const babelOutput = output.replace(/\.json$/, '') + '-babel.json';
    const flexiOutput = output.replace(/\.json$/, '') + '-flexi.json';
    for (const file of [babelOutput, flexiOutput]) fs.writeFileSync(file, JSON.stringify({ complete: false, status: 'not-started' }));
    const children = await Promise.allSettled([
      ...(suite === 'all' ? [run(process.execPath, [path.join(here, 'portability.mjs'), '--suite', 'babel', '--output', babelOutput], root, 130 * 60 * 1000)] : []),
      run(process.execPath, [path.join(here, 'flexi.mjs'), '--jobs', jobs, '--output', flexiOutput], root, 130 * 60 * 1000),
    ]);
    report.childErrors = children.filter(c => c.status === 'rejected').map(c => String(c.reason));
    const flexi = JSON.parse(fs.readFileSync(flexiOutput));
    Object.assign(report, suite === 'all'
      ? combinePortability(JSON.parse(fs.readFileSync(babelOutput)), flexi) : flexi);
    report.aggregationSha256 = hash(fs.readFileSync(path.join(here, 'portability-combine.mjs')));
    if (report.status !== 'passed') process.exitCode = 1;
  } catch (error) { report.status = 'failed'; report.error = String(error); process.exitCode = 1; }
  finally { save(); log.end(); fs.rmSync(dir, { recursive: true, force: true }); }
  console.log(`Portability upstream tests ${report.status}; report: ${output}`);
} else {
try {
  const setup = path.join(os.homedir(), 'quicklisp/setup.lisp');
  const discovery = await run('sbcl', ['--noinform', '--non-interactive', '--eval', '(require :asdf)',
    ...(fs.existsSync(setup) ? ['--load', setup] : []), '--eval',
    '(dolist (name (quote ("babel"))) (format t "~&PORTABILITY-SOURCE ~A ~A~%" name (asdf:system-source-directory (asdf:find-system name))))'], root);
  const sources = Object.fromEntries([...discovery.matchAll(/^PORTABILITY-SOURCE (\S+) (.+)$/gm)].map(m => [m[1], m[2]]));
  if (!sources.babel) throw new Error('Install Babel sources with Quicklisp first');
  report.sources = Object.fromEntries(Object.entries(sources).map(([name, directory]) => [name, { directory, files: treeHashes(directory) }]));
  report.harnessFiles = Object.fromEntries(['portability.mjs', 'babel-test-adapter.lisp', 'babel-test-main.lisp', 'portability-upstream-main.lisp', 'portability-report.mjs'].map(name => [name, hash(fs.readFileSync(path.join(here, name)))]));
  // Use the host reader independently to audit registration under target features.
  const reference = await run('sbcl', ['--noinform', '--non-interactive', '--eval', '(require :asdf)',
    ...(fs.existsSync(setup) ? ['--load', setup] : []), '--eval', '(asdf:load-system "babel")',
    '--load', path.join(here, 'babel-test-adapter.lisp'), '--eval',
    `(let ((*features* '(:valtan :node))) (load ${str(path.join(sources.babel, 'tests/tests.lisp'))}))`,
    '--eval', '(dolist (test (reverse hu.dwim.stefil::*tests*)) (format t "EXPECTED-BABEL-TEST ~A~%" test))'], root);
  report.expectedBabelTests = [...reference.matchAll(/^EXPECTED-BABEL-TEST (.+)$/gm)].map(m => m[1]);
  if (!report.expectedBabelTests.length) throw new Error('No reference Babel tests registered');
  fs.copyFileSync(path.join(here, 'babel-test-main.lisp'), path.join(dir, 'babel-test-main.lisp'));
  const dependencies = '"babel" "uiop"';
  fs.writeFileSync(path.join(dir, 'app.system'), `(defsystem "app" :depends-on (${dependencies}) :components ((:file "prelude") (:file "adapter") (:file "main")) :target :node :compile-mode :runtime)`);
  fs.writeFileSync(path.join(dir, 'prelude.lisp'), `(defparameter *babel-test-source* #p${str(sources.babel)})\n(defparameter *babel-test-main* #p${str(path.join(dir, 'babel-test-main.lisp'))})\n`);
  fs.copyFileSync(path.join(here, 'babel-test-adapter.lisp'), path.join(dir, 'adapter.lisp'));
  let main = fs.readFileSync(path.join(here, 'portability-upstream-main.lisp'), 'utf8');
  const split = main.indexOf('(load (merge-pathnames "test/packages.lisp"');
  main = main.slice(0, split) + '(write-line "PORTABILITY-UPSTREAM-COMPLETE") (finish-output)\n';
  fs.writeFileSync(path.join(dir, 'main.lisp'), main);
  const compiled = await run(path.join(root, 'valtan'), ['build', 'app.system'], dir);
  if (/debugger invoked|caught ERROR|Valtan error:/.test(compiled)) throw new Error('Compilation failed');
  const outfile = path.join(dir, 'app.mjs');
  await build({ entryPoints: [path.join(dir, '.valtan-cache/app.js')], outfile,
    bundle: true, platform: 'node', format: 'esm', logLevel: 'silent', alias: { lisp: path.join(root, 'kernel/lisp.js') } });
  report.bundleSha256 = hash(fs.readFileSync(outfile)); save();
  const result = await run(process.execPath, [outfile], dir, 45 * 60 * 1000);
  Object.assign(report, summarizeUpstream(result, report.expectedBabelTests, suite));
  if (report.status !== 'passed') throw new Error('Upstream tests failed; see individual outcomes and coverage');
} catch (error) {
  report.status = 'failed'; report.error = String(error); process.exitCode = 1;
} finally {
  save(); log.end(); fs.rmSync(dir, { recursive: true, force: true });
}
console.log(`Portability upstream tests ${report.status}; report: ${output}`);

}
