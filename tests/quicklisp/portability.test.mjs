// Offline adapter coverage; no Quicklisp distribution is needed.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import vm from 'node:vm';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';
import { build } from 'esbuild';
const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
for (const target of ['node', 'browser']) {
  test(`UIOP utilities and target features in ${target}`, { timeout: 900000 }, async () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-portability-'));
    try {
      fs.writeFileSync(path.join(dir, 'app.system'), `(defsystem "app" :depends-on ("uiop" "trivial-features") :components ((:file "checks") (:file "subprocess") (:file "features")) :target :${target} :compile-mode :runtime)`);
      fs.copyFileSync(path.join(here, 'uiop-checks.lisp'), path.join(dir, 'checks.lisp'));
      fs.copyFileSync(path.join(here, 'subprocess-checks.lisp'), path.join(dir, 'subprocess.lisp'));
      fs.writeFileSync(path.join(dir, 'features.lisp'), `
        (assert (member :${os.endianness() === 'LE' ? 'little-endian' : 'big-endian'} *features*))
        (assert (not (member :sbcl *features*)))
        ${target === 'node' ? `
          (assert (member :node *features*))
          (assert (member :${os.type() === 'Windows_NT' ? 'windows' : os.type().toLowerCase()} *features*))
          (assert (eq (not (null (member :unix *features*))) ${os.platform() === 'win32' ? 'nil' : 't'}))
        ` : `
          (assert (not (member :node *features*)))
          (assert (not (intersection '(:unix :windows :linux :darwin :x86 :x86-64 :arm :arm64 :32-bit :64-bit) *features*)))
        `}
        (write-line "PORTABILITY-FEATURES-PASS") (finish-output)
      `);
      const compiled = spawnSync(path.join(root, 'valtan'), ['build', 'app.system'],
        { cwd: dir, encoding: 'utf8', timeout: 120000, maxBuffer: 16 * 1024 * 1024 });
      assert.equal(compiled.status, 0, compiled.stdout + compiled.stderr);
      assert.doesNotMatch(compiled.stdout + compiled.stderr, /debugger invoked|caught ERROR|Valtan error:/);
      const outfile = path.join(dir, 'app.mjs');
      await build({ entryPoints: [path.join(dir, '.valtan-cache/app.js')], outfile,
        bundle: true, platform: target === 'node' ? 'node' : 'browser',
        format: target === 'node' ? 'esm' : 'iife', logLevel: 'silent',
        alias: { lisp: path.join(root, 'kernel/lisp.js') } });
      let output;
      if (target === 'node') {
        const executed = spawnSync(process.execPath, [outfile],
          { cwd: dir, encoding: 'utf8', timeout: 300000, maxBuffer: 4 * 1024 * 1024 });
        assert.equal(executed.status, 0, executed.stdout + executed.stderr);
        output = executed.stdout + executed.stderr;
      } else {
        const lines = [];
        // No process, require, filesystem or Node OS object in this realm.
        vm.runInNewContext(fs.readFileSync(outfile, 'utf8'), {
          console: { log: (...args) => lines.push(args.join(' ')), error: (...args) => lines.push(args.join(' ')) },
          TextEncoder, TextDecoder, performance,
        }, { timeout: 600000 });
        output = lines.join('\n');
      }
      assert.doesNotMatch(output, /Valtan error:/);
      assert.match(output, /^PORTABILITY-SUITE uiop passed \d+$/m);
      assert.match(output, /^PORTABILITY-FEATURES-PASS$/m);
      assert.match(output, /^PORTABILITY-SUBPROCESS-PASS$/m);
    } finally { fs.rmSync(dir, { recursive: true, force: true }); }
  });
}
