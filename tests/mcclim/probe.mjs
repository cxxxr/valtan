// Diagnose loading real installed McCLIM systems in the Valtan target.
// A successful load is an intermediate gate, not proof of a working GUI.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {build} from 'esbuild';
import http from 'node:http';
import {launchChrome} from './chrome.mjs';
const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
const [system = 'clim-core/system', outputName = '/tmp/valtan-mcclim-load.json', checkFile, ...features] = process.argv.slice(2);
const browserOption = features.indexOf('--browser');
const target = browserOption < 0 ? 'node' : 'browser';
if (browserOption >= 0) features.splice(browserOption, 1);
const afterLoadOptions = features.filter(feature => feature.startsWith('--after-load='));
if (afterLoadOptions.length > 1 || (afterLoadOptions.length && target !== 'browser'))
  throw new Error('--after-load=FILE is supported once, with --browser');
const afterLoad = afterLoadOptions.length ? fs.readFileSync(path.resolve(afterLoadOptions[0].slice(13)), 'utf8') : null;
for (let i = features.length - 1; i >= 0; i--) if (features[i].startsWith('--after-load=')) features.splice(i, 1);
const extraSystems = features.filter(feature => feature.startsWith('--depends-on=')).map(feature => feature.slice(13));
for (let i = features.length - 1; i >= 0; i--) if (features[i].startsWith('--depends-on=')) features.splice(i, 1);
if (!/^[a-z0-9/-]+$/.test(system)) throw new Error('Invalid system name');
if (extraSystems.some(name => !/^[a-z0-9/._-]+$/.test(name))) throw new Error('Invalid extra dependency');
if (features.some(feature => !/^[a-z0-9-]+$/.test(feature))) throw new Error('Invalid target feature');
const output = path.resolve(outputName);
const work = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-mcclim-'));
const logs = output.replace(/\.json$/, '') + '-logs';
fs.mkdirSync(logs, {recursive: true});
const hash = data => createHash('sha256').update(data).digest('hex');
const report = {system, scope: checkFile ? 'Load and supplied API assertions; GUI verification is separate' : 'Load gate only; GUI verification is separate',
  timestamp: new Date().toISOString(), complete: false, status: 'running', work,
  compilerSha256: hash(fs.readFileSync(path.join(root, 'valtan'))), target, extraSystems, targetFeatures: features};
if (afterLoad !== null) report.afterLoadSha256 = hash(afterLoad);
const save = () => fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
async function run(command, args, logName, timeout) {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {cwd: work, stdio: ['ignore', 'pipe', 'pipe']});
    const log = fs.createWriteStream(path.join(logs, logName)); let text = '';
    const timer = setTimeout(() => child.kill('SIGKILL'), timeout);
    for (const stream of [child.stdout, child.stderr]) stream.on('data', chunk => {
      text += chunk; log.write(chunk);
      if (logName === 'run.log') process.stdout.write(chunk);
    });
    child.on('error', error => {clearTimeout(timer); log.end(); reject(error);});
    child.on('close', (code, signal) => {
      clearTimeout(timer); log.end();
      if (code !== 0 || /debugger invoked|caught ERROR|Valtan error:/.test(text))
        reject(new Error(`${command}: ${signal ?? code}\n${text.slice(-5000)}`));
      else resolve(text);
    });
  });
}
save();
async function runBrowser(bundle) {
  const worker = `console.log = (...args) => postMessage({line: args.join(' ')});
    console.error = (...args) => postMessage({error: args.join(' ')});
    try {await import('/app.mjs'); ${afterLoad !== null ? "await import('/after-load.mjs');" : ''} postMessage({done: true});}
    catch(error) {postMessage({error: String(error), done: true});}`;
  const server = http.createServer((request, response) => {
    if (request.url === '/app.mjs') {response.setHeader('Content-Type', 'text/javascript'); response.end(fs.readFileSync(bundle));}
    else if (request.url === '/after-load.mjs' && afterLoad !== null) {response.setHeader('Content-Type', 'text/javascript'); response.end(afterLoad);}
    else if (request.url === '/worker.mjs') {response.setHeader('Content-Type', 'text/javascript'); response.end(worker);}
    else {response.setHeader('Content-Type', 'text/html'); response.end(`<script>
      window.probe = {lines: [], errors: [], done: false};
      const worker = new Worker('/worker.mjs', {type: 'module'});
      worker.onmessage = ({data}) => {
        if (data.line !== undefined) probe.lines.push(data.line);
        if (data.error !== undefined) probe.errors.push(data.error);
        if (data.done) probe.done = true;
      };
      worker.onerror = e => {probe.errors.push(e.message); probe.done = true;};
      </script>`);}
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let chrome;
  try {
    chrome = await launchChrome();
    const page = await chrome.page(`http://127.0.0.1:${server.address().port}/`);
    const deadline = Date.now() + 60 * 60 * 1000;
    while (!await page.evaluate('probe.done')) {
      if (Date.now() >= deadline) throw new Error('Browser load probe timed out');
      await new Promise(resolve => setTimeout(resolve, 1000));
    }
    const state = await page.evaluate('probe');
    const text = state.lines.join('\n');
    fs.writeFileSync(path.join(logs, 'run.log'), text + '\n' + state.errors.join('\n'));
    if (state.errors.length || /Valtan error:/.test(text)) throw new Error(state.errors.join('\n') || text);
    return text;
  } finally {
    await chrome?.close();
    await new Promise(resolve => server.close(resolve));
  }
}
try {
  fs.writeFileSync(path.join(work, 'mcclim-trace.system'), '(defsystem "mcclim-trace" :components ((:file "trace")))');
  fs.writeFileSync(path.join(work, 'trace.lisp'), '(setf *load-verbose* t)\n');
  fs.writeFileSync(path.join(work, 'app.system'), `(defsystem "app" :depends-on ("mcclim-trace" ${[...extraSystems, system].map(name => JSON.stringify(name)).join(' ')}) :components ((:file "main")) :target :${target} :compile-mode :runtime :target-features (${features.map(feature => ':' + feature).join(' ')}))`);
  const checks = checkFile ? fs.readFileSync(path.resolve(checkFile), 'utf8') : '';
  if (checkFile) report.checksSha256 = hash(checks);
  fs.writeFileSync(path.join(work, 'main.lisp'), checks + '\n(write-line "MCCLIM-LOAD-COMPLETE") (finish-output)\n');
  report.stage = 'build'; save();
  await run(path.join(root, 'valtan'), ['build', 'app.system'], 'build.log', 180000);
  const bundle = path.join(work, 'app.mjs');
  await build({entryPoints: [path.join(work, '.valtan-cache/app.js')], outfile: bundle,
    bundle: true, platform: target, format: 'esm', logLevel: 'silent',
    alias: {lisp: path.join(root, 'kernel/lisp.js')}});
  report.bundleSha256 = hash(fs.readFileSync(bundle)); report.stage = 'run'; save();
  const text = target === 'browser' ? await runBrowser(bundle) : await run(process.execPath, [bundle], 'run.log', 60 * 60 * 1000);
  if (!/^MCCLIM-LOAD-COMPLETE$/m.test(text)) throw new Error('Load completion marker missing');
  report.complete = true; report.status = 'passed';
} catch (error) {report.status = 'failed'; report.error = String(error); process.exitCode = 1;}
finally {report.finishedAt = new Date().toISOString(); save();}
console.log(`${system}: ${report.status}; report: ${output}`);
