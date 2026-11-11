// Compile McCLIM components using Valtan's own COMPILE-FILE, then load each
// artifact before compiling its dependants. The Node file adapter is installed
// without adding :NODE to the browser target's reader features.
import fs from 'node:fs';
import path from 'node:path';
import assert from 'node:assert/strict';
import {fileURLToPath, pathToFileURL} from 'node:url';
import {execFileSync} from 'node:child_process';
import {build} from 'esbuild';
import {hash} from './bundle.mjs';
import {readForms} from './staged-artifact.mjs';

const [phase, workArg, ...extra] = process.argv.slice(2);
assert.ok(['prepare', 'produce'].includes(phase) && workArg && extra.length === 0,
  'Usage: staged-produce.mjs prepare|produce work');
const root = fileURLToPath(new URL('../..', import.meta.url));
const work = path.resolve(workArg);
const cache = path.join(work, '.valtan-cache');
const sha = file => hash(fs.readFileSync(file));
const walk = directory => fs.readdirSync(directory, {withFileTypes: true})
  .flatMap(entry => entry.isDirectory() ? walk(path.join(directory, entry.name)) : [path.join(directory, entry.name)]);

function sourceInventory() {
  return walk(cache).filter(file => file.endsWith('.js')).flatMap(file => {
    const code = fs.readFileSync(file, 'utf8');
    if (!code.includes("lisp.intern('LOAD-EMBEDDED-SOURCE', 'SYSTEM')") ||
        !/TMP_2=G_1\.func\(lisp\.codeArrayToString\(\[/.test(code)) return [];
    const source = file.slice(cache.length, -3).replaceAll('\\', '');
    assert.ok(code.includes("lisp.intern('MCCLIM', 'KEYWORD')"), source);
    return [{source, sourceSha256: sha(source), module: file, moduleSha256: sha(file)}];
  }).sort((a, b) => a.source.localeCompare(b.source));
}

if (phase === 'prepare') {
  // Refuse to replace a running producer or previously generated evidence.
  assert.ok(!fs.existsSync(work) || fs.readdirSync(work).length === 0, 'Use a new or empty work directory');
  fs.mkdirSync(path.join(work, 'artifacts'), {recursive: true});
  fs.copyFileSync(new URL('./staged-loader.lisp', import.meta.url), path.join(work, 'stage.lisp'));
  fs.copyFileSync(new URL('./staged-checks.lisp', import.meta.url), path.join(work, 'main.lisp'));
  fs.writeFileSync(path.join(work, 'stage.system'), '(defsystem "stage" :components ((:file "stage")))\n');
  fs.writeFileSync(path.join(work, 'app.system'), `(defsystem "app"
    :depends-on ("stage" "mcclim-browser") :components ((:file "main"))
    :target :browser :compile-mode :runtime
    :target-features (:clim-without-swank :valtan-resumable :clim-mp))\n`);
  const compilerSha256 = sha(path.join(root, 'valtan'));
  const log = fs.openSync(path.join(work, 'build.log'), 'w');
  try {
    execFileSync(path.join(root, 'valtan'), ['build', 'app.system'],
      {cwd: work, stdio: ['ignore', log, log], timeout: 180000});
  } finally { fs.closeSync(log); }
  assert.doesNotMatch(fs.readFileSync(path.join(work, 'build.log'), 'utf8'),
    /debugger invoked|caught ERROR|Valtan error:/);
  const sources = sourceInventory();
  assert.ok(sources.length > 0);
  fs.writeFileSync(path.join(work, 'prepared.json'), JSON.stringify({
    status: 'prepared', compilerSha256, kernelSha256: sha(path.join(root, 'kernel/lisp.js')), sources,
  }, null, 2) + '\n');
  console.log(`Prepared ${sources.length - 1} McCLIM components plus the staging loader in ${work}`);
} else {
  assert.ok(!fs.existsSync(path.join(work, 'manifest.json')), 'Manifest already exists');
  assert.equal(fs.readdirSync(path.join(work, 'artifacts')).length, 0, 'Artifacts already exist; use a new work directory');
  const prepared = JSON.parse(fs.readFileSync(path.join(work, 'prepared.json')));
  assert.equal(prepared.status, 'prepared');
  assert.deepEqual(sourceInventory(), prepared.sources, 'Sources or generated modules changed after preparation');
  assert.equal(sha(path.join(root, 'kernel/lisp.js')), prepared.kernelSha256, 'Kernel changed after preparation');
  const bundle = path.join(work, 'producer.mjs');
  await build({entryPoints: [path.join(cache, 'app.js')], outfile: bundle,
    bundle: true, platform: 'node', format: 'esm', logLevel: 'silent',
    alias: {lisp: path.join(root, 'kernel/lisp.js')}});
  const errors = [], originalError = console.error;
  console.error = (...args) => { errors.push(args); originalError(...args); };
  globalThis.valtanStageFS = fs;
  globalThis.valtanStagePath = path;
  globalThis.valtanStageArtifacts = path.join(work, 'artifacts');
  globalThis.valtanStageLoad = (lisp, filename) => {
    for (const {code, call} of readForms(fs.readFileSync(filename, 'utf8'))) {
      const value = new Function('lisp', "'use strict';\n" + code)(lisp);
      if (call) lisp.intern('FUNCALL', 'COMMON-LISP').getFunction()(value);
    }
  };
  process.chdir(work);
  const startedAt = new Date().toISOString(), start = performance.now();
  try { await import(pathToFileURL(bundle)); }
  finally { console.error = originalError; }
  assert.equal(errors.length, 0, 'No Lisp top-level errors');
  const records = globalThis.valtanStageRecords;
  const compiledSources = prepared.sources.filter(record => record.source !== path.join(work, 'stage.lisp'));
  assert.deepEqual(records.map(record => record.source).sort(), compiledSources.map(record => record.source).sort());
  assert.deepEqual(sourceInventory(), prepared.sources, 'Sources or modules changed during compilation');
  const lisp = globalThis.valtanStageKernel;
  const targetFeatures = lisp.listToJsArray(lisp.findPackage('COMMON-LISP').findSymbol('*FEATURES*')[0].value).map(x => x.name);
  assert.ok(targetFeatures.includes('MCCLIM'));
  assert.ok(!targetFeatures.includes('NODE'));
  const report = {status: 'produced', complete: true, startedAt, finishedAt: new Date().toISOString(),
    durationMs: performance.now() - start, targetFeatures, producerBundleSha256: sha(bundle),
    preparedManifestSha256: sha(path.join(work, 'prepared.json')), kernelSha256: prepared.kernelSha256,
    compilerSha256: prepared.compilerSha256,
    records: records.map(record => ({...record, sourceSha256: sha(record.source), artifactSha256: sha(record.artifact)}))};
  fs.writeFileSync(path.join(work, 'manifest.json'), JSON.stringify(report, null, 2) + '\n');
  console.log(`Produced ${records.length} components; manifest: ${path.join(work, 'manifest.json')}`);
}
