import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';
import { build } from 'esbuild';
const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');

test('ASDF dependencies execute once in the target with macros, readtables and source paths', async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-asdf-fixture-'));
  try {
    fs.cpSync(path.join(here, 'fixtures'), dir, { recursive: true });
    const compiled = spawnSync(path.join(root, 'valtan'), ['build', 'compat.system'],
      { cwd: dir, encoding: 'utf8', timeout: 120000, maxBuffer: 16 * 1024 * 1024 });
    assert.equal(compiled.status, 0, compiled.stdout + compiled.stderr);
    const outfile = path.join(dir, 'compat.mjs');
    await build({ entryPoints: [path.join(dir, '.valtan-cache/compat.js')], outfile,
      bundle: true, platform: 'node', format: 'esm', logLevel: 'silent',
      alias: { lisp: path.join(root, 'kernel/lisp.js') } });
    // Prove this is self-contained: runtime needs none of the source files.
    fs.rmSync(path.join(dir, 'src'), { recursive: true });
    for (const name of ['main.lisp', 'sub.lisp', 'fixture-utils.asd']) fs.unlinkSync(path.join(dir, name));
    const executed = spawnSync(process.execPath, [outfile], { cwd: dir, encoding: 'utf8', timeout: 30000 });
    assert.equal(executed.status, 0, executed.stdout + executed.stderr);
    assert.match(executed.stdout, /^QUICKLISP-FIXTURE-PASS$/m);
    const failed = spawnSync(process.execPath, [outfile], { cwd: dir, encoding: 'utf8', timeout: 30000,
      env: { ...process.env, VALTAN_FIXTURE_FAIL: 'yes' } });
    assert.notEqual(failed.status, 0);
    assert.match(failed.stderr, /EXPECTED-FIXTURE-LOAD-FAILURE/);
    assert.doesNotMatch(failed.stdout, /QUICKLISP-FIXTURE-PASS/);
  } finally { fs.rmSync(dir, { recursive: true, force: true }); }
});

for (const mode of ['ahead-of-time', 'runtime']) {
  test(`LOAD-TIME-VALUE and fresh lexical bindings in ${mode} mode`, async () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-ltv-fixture-'));
    try {
      fs.copyFileSync(path.join(here, 'fixtures/load-time-values.lisp'), path.join(dir, 'main.lisp'));
      fs.writeFileSync(path.join(dir, 'ltv.system'), `(defsystem "ltv" :compile-mode :${mode} :target :node :components ((:file "main")))`);
      const compiled = spawnSync(path.join(root, 'valtan'), ['build', 'ltv.system'],
        { cwd: dir, encoding: 'utf8', timeout: 120000, maxBuffer: 16 * 1024 * 1024 });
      assert.equal(compiled.status, 0, compiled.stdout + compiled.stderr);
      assert.doesNotMatch(compiled.stdout + compiled.stderr, /debugger invoked|caught ERROR|Valtan error:/);
      const outfile = path.join(dir, 'ltv.mjs');
      await build({ entryPoints: [path.join(dir, '.valtan-cache/ltv.js')], outfile,
        bundle: true, platform: 'node', format: 'esm', logLevel: 'silent',
        alias: { lisp: path.join(root, 'kernel/lisp.js') } });
      const executed = spawnSync(process.execPath, [outfile], { cwd: dir, encoding: 'utf8', timeout: 30000 });
      assert.equal(executed.status, 0, executed.stdout + executed.stderr);
      assert.doesNotMatch(executed.stdout + executed.stderr, /Valtan error:/);
      assert.match(executed.stdout, /^LOAD-TIME-VALUE-PASS$/m);
    } finally { fs.rmSync(dir, { recursive: true, force: true }); }
  });
}
