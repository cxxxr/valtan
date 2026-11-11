import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
import {build} from 'esbuild';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');

test('weak table reachability, ephemeron cycles and numeric keys', async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-weak-'));
  try {
    const outfile = path.join(dir, 'test.mjs');
    await build({stdin: {resolveDir: root, contents: `
      import './kernel/lisp.js';
      import {WeakHashMap} from './kernel/weak.js';
      import {Float, Ratio, Complex} from './kernel/number.js';
      import assert from 'node:assert/strict';
      const tick = () => new Promise(resolve => setImmediate(resolve));
      async function collect() { for (let i = 0; i < 12; i++) { await tick(); global.gc(); } }
      for (const mode of ['key', 'value', 'key-and-value', 'key-or-value']) {
        const map = new WeakHashMap(mode), key = {}, value = {};
        map.set(key, value);
        await collect();
        assert.equal(map.get(key), value);
        assert.deepEqual([...map.entries()], [[key, value]]);
        assert.equal(map.size, 1);
        assert.equal(map.delete(key), true);
        assert.equal(map.size, 0);
      }
      const keyMap = new WeakHashMap('key');
      (() => { const key = {}; keyMap.set(key, {back: key}); })();
      await collect(); assert.equal(keyMap.size, 0, 'value-to-key cycle must collect');
      for (const mode of ['value', 'key-and-value']) {
        const map = new WeakHashMap(mode), key = {};
        (() => map.set(key, {}))();
        await collect(); assert.equal(map.has(key), false, mode);
        assert.equal(map.size, 0);
      }
      const either = new WeakHashMap('key-or-value');
      let value = {};
      (() => either.set({}, value))();
      await collect(); assert.equal(either.size, 1, 'live value retains key');
      value = null;
      await collect(); assert.equal(either.size, 0, 'unrooted pair collects');
      either.set({}, 42);
      await collect(); assert.equal(either.size, 1, 'immediate value retains key');
      either.clear(); assert.equal(either.size, 0);
      const eql = new WeakHashMap('key');
      eql.set(new Float(1.5), 'float');
      eql.set(new Ratio(1n, 2n), 'ratio');
      eql.set(new Complex(2, 3), 'complex');
      await collect();
      assert.equal(eql.get(new Float(1.5)), 'float');
      assert.equal(eql.get(new Ratio(1n, 2n)), 'ratio');
      assert.equal(eql.get(new Complex(2, 3)), 'complex');
      assert.equal(eql.get(1.5), undefined);
      console.log('WEAK-GC-PASS');
    `}, outfile, bundle: true, platform: 'node', format: 'esm', logLevel: 'silent'});
    const result = spawnSync(process.execPath, ['--expose-gc', outfile], {encoding: 'utf8', timeout: 60000});
    assert.equal(result.status, 0, result.stdout + result.stderr);
    assert.match(result.stdout, /WEAK-GC-PASS/);
  } finally { fs.rmSync(dir, {recursive: true, force: true}); }
});

test('trivial-garbage Lisp API and CL hash operations', {timeout: 300000}, async () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-tg-'));
  try {
    fs.writeFileSync(path.join(dir, 'app.system'), '(defsystem "app" :depends-on ("trivial-garbage") :components ((:file "checks")) :target :node :compile-mode :runtime)');
    fs.writeFileSync(path.join(dir, 'checks.lisp'), `
      (dolist (mode '(:key :value :key-and-value :key-or-value))
        (let ((table (tg:make-weak-hash-table :test 'eq :weakness mode))
              (key (list 1)) (value (list 2)))
          (assert (hash-table-p table))
          (assert (eq mode (tg:hash-table-weakness table)))
          (setf (gethash key table) value)
          (assert (= 1 (hash-table-count table)))
          (assert (eq value (gethash key table)))
          (let ((seen 0))
            (maphash (lambda (k v) (assert (eq k key)) (assert (eq v value)) (incf seen)) table)
            (assert (= seen 1)))
          (with-hash-table-iterator (next table)
            (multiple-value-bind (found k v) (next)
              (assert found) (assert (eq k key)) (assert (eq v value))))
          (assert (remhash key table))
          (assert (zerop (hash-table-count table)))
          (setf (gethash key table) nil)
          (assert (nth-value 1 (gethash key table)))
          (clrhash table) (assert (zerop (hash-table-count table)))))
      (let* ((object (list 1)) (pointer (tg:make-weak-pointer object)))
        (assert (tg:weak-pointer-p pointer))
        (assert (eq object (tg:weak-pointer-value pointer)))
        (tg:finalize object (lambda () nil))
        (tg:cancel-finalization object))
      (write-line "TG-API-PASS") (finish-output)
    `);
    const compiled = spawnSync(path.join(root, 'valtan'), ['build', 'app.system'], {cwd: dir, encoding: 'utf8', timeout: 180000, maxBuffer: 16*1024*1024});
    assert.equal(compiled.status, 0, compiled.stdout + compiled.stderr);
    const outfile = path.join(dir, 'app.mjs');
    await build({entryPoints: [path.join(dir, '.valtan-cache/app.js')], outfile, bundle: true, platform: 'node', format: 'esm', logLevel: 'silent', alias: {lisp: path.join(root, 'kernel/lisp.js')}});
    const executed = spawnSync(process.execPath, [outfile], {cwd: dir, encoding: 'utf8', timeout: 120000, maxBuffer: 4*1024*1024});
    assert.equal(executed.status, 0, executed.stdout + executed.stderr);
    assert.doesNotMatch(executed.stdout + executed.stderr, /Valtan error:/);
    assert.match(executed.stdout, /TG-API-PASS/);
  } finally { fs.rmSync(dir, {recursive: true, force: true}); }
});
