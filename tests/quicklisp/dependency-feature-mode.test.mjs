import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
import {build} from 'esbuild';
const root = fileURLToPath(new URL('../../', import.meta.url));

for (const mode of ['ahead-of-time', 'runtime']) {
  test(`a dependency's resumable feature selects dual compilation in ${mode} mode`, async () => {
    const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-dependency-feature-'));
    try {
      fs.writeFileSync(path.join(directory, 'cooperative-library.system'), `(defsystem "cooperative-library"
        :target-features (:valtan-resumable) :components ((:file "library")))`);
      fs.writeFileSync(path.join(directory, 'library.lisp'), `(in-package :cl-user)
        (defun delayed-add (value) (sleep .01) (1+ value))`);
      fs.writeFileSync(path.join(directory, 'app.system'), `(defsystem "app"
        :target :node :compile-mode :${mode} :depends-on ("cooperative-library")
        :components ((:file "main")))`);
      fs.writeFileSync(path.join(directory, 'main.lisp'), `(in-package :cl-user)
        #-valtan-resumable (error "Dependency feature missing from the source reader")
        ${mode === 'runtime' ? '(assert (member :valtan-resumable *features*))' : ''}
        (ffi:set (ffi:ref "globalThis.dependencyFeatureKernel") (ffi:ref "lisp"))
        (ffi:set (ffi:ref "globalThis.dependencyOrdinaryResult") (delayed-add 40))`);
      const compiled = spawnSync(path.join(root, 'valtan'), ['build', 'app.system'],
        {cwd: directory, encoding: 'utf8', timeout: 120000, maxBuffer: 16 * 1024 * 1024});
      assert.equal(compiled.status, 0, compiled.stdout + compiled.stderr);
      assert.doesNotMatch(compiled.stdout + compiled.stderr, /debugger invoked|caught ERROR|Valtan error:/);
      await build({entryPoints: [path.join(directory, '.valtan-cache/app.js')],
        outfile: path.join(directory, 'app.mjs'), bundle: true, platform: 'node',
        format: 'esm', logLevel: 'silent', alias: {lisp: path.join(root, 'kernel/lisp.js')}});
      for (const file of ['library.lisp', 'main.lisp', 'app.system', 'cooperative-library.system'])
        fs.unlinkSync(path.join(directory, file));
      fs.writeFileSync(path.join(directory, 'verify.mjs'), `
        import assert from 'node:assert/strict';
        await import('./app.mjs');
        const lisp=globalThis.dependencyFeatureKernel;
        const fn=lisp.intern('DELAYED-ADD','COMMON-LISP-USER').getFunction();
        const sleep=lisp.intern('SLEEP','COMMON-LISP').getFunction();
        assert.equal(typeof lisp.getResumableImplementation(fn),'function','dependency function is dual');
        assert.equal(typeof lisp.getResumableImplementation(sleep),'function','core functions are dual');
        assert.equal(globalThis.dependencyOrdinaryResult,41);
        const scheduler=new lisp.CooperativeScheduler({checkpointBudget:10000});
        assert.deepEqual(await scheduler.spawn(fn,[41]).completion,[42]);
        assert.equal(scheduler.tasks.size,0);
        console.log('DEPENDENCY-FEATURE-MODE-PASS');
      `);
      const executed = spawnSync(process.execPath, [path.join(directory, 'verify.mjs')],
        {cwd: directory, encoding: 'utf8', timeout: 30000});
      assert.equal(executed.status, 0, executed.stdout + executed.stderr);
      assert.match(executed.stdout, /DEPENDENCY-FEATURE-MODE-PASS/);
    } finally {
      fs.rmSync(directory, {recursive: true, force: true});
    }
  });
}
