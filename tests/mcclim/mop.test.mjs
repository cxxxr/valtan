import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
import {build} from 'esbuild';
const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
test('MOP metaclasses, custom slot storage and forward references', {timeout: 300000}, async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-mop-'));
  let passed = false;
  try {
    fs.writeFileSync(path.join(dir, 'app.system'), '(defsystem "app" :depends-on ("closer-mop") :components ((:file "checks")) :target :node :compile-mode :runtime)');
    fs.copyFileSync(path.join(here, 'mop-checks.lisp'), path.join(dir, 'checks.lisp'));
    const compiled = spawnSync(path.join(root, 'valtan'), ['build', 'app.system'], {cwd: dir, encoding: 'utf8', timeout: 180000, maxBuffer: 16*1024*1024});
    fs.writeFileSync(path.join(dir, 'build.log'), compiled.stdout + compiled.stderr);
    assert.equal(compiled.status, 0, compiled.stdout + compiled.stderr);
    const outfile = path.join(dir, 'app.mjs');
    await build({entryPoints: [path.join(dir, '.valtan-cache/app.js')], outfile, bundle: true, platform: 'node', format: 'esm', logLevel: 'silent', alias: {lisp: path.join(root, 'kernel/lisp.js')}});
    const executed = spawnSync(process.execPath, [outfile], {cwd: dir, encoding: 'utf8', timeout: 120000, maxBuffer: 8*1024*1024});
    fs.writeFileSync(path.join(dir, 'run.log'), executed.stdout + executed.stderr);
    assert.equal(executed.status, 0, (executed.stdout + executed.stderr).slice(-10000));
    assert.doesNotMatch(executed.stdout + executed.stderr, /Valtan error:/);
    assert.match(executed.stdout, /MOP-CHECKS-PASS/);
    passed = true;
  } finally {
    if (passed) fs.rmSync(dir, {recursive: true, force: true});
    else console.error('Retained MOP test artifacts:', dir);
  }
});
