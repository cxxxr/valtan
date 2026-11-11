import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import http from 'node:http';
import {fileURLToPath} from 'node:url';
import {spawnSync} from 'node:child_process';
import {build} from 'esbuild';
import {launchChrome} from '../mcclim/chrome.mjs';
const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');

for (const target of ['node', 'browser']) {
  test(`ASDF static resources survive source removal in ${target}`, {timeout: 240000}, async () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-resources-'));
    let chrome, server;
    try {
      fs.mkdirSync(path.join(dir, 'assets/nested'), {recursive: true});
      fs.writeFileSync(path.join(dir, 'assets/Data.bin'), Uint8Array.from([0, 1, 128, 255, 0, 42]));
      fs.writeFileSync(path.join(dir, 'assets/nested/日本語.txt'), '日本語😀\n');
      fs.writeFileSync(path.join(dir, 'assets/host-only.bin'), 'must not be embedded');
      fs.writeFileSync(path.join(dir, 'resource-fixture.asd'), `(asdf:defsystem "resource-fixture"
        :components ((:module "assets" :components ((:static-file "Data.bin")
          (:static-file "host-only.bin" :if-feature :sbcl)
          (:module "nested" :components ((:static-file "日本語.txt")))))))`);
      fs.writeFileSync(path.join(dir, 'app.system'), `(defsystem "app" :depends-on ("resource-fixture")
        :target :${target} :compile-mode :runtime :components ((:file "checks")))`);
      fs.copyFileSync(path.join(here, 'resource-checks.lisp'), path.join(dir, 'checks.lisp'));
      const compiled = spawnSync(path.join(root, 'valtan'), ['build', 'app.system'],
        {cwd: dir, encoding: 'utf8', timeout: 120000, maxBuffer: 16 * 1024 * 1024});
      assert.equal(compiled.status, 0, compiled.stdout + compiled.stderr);
      assert.doesNotMatch(compiled.stdout + compiled.stderr, /debugger invoked|caught ERROR/);
      const outfile = path.join(dir, 'app.mjs');
      await build({entryPoints: [path.join(dir, '.valtan-cache/app.js')], outfile,
        bundle: true, platform: target === 'node' ? 'node' : 'browser', format: 'esm', logLevel: 'silent',
        alias: {lisp: path.join(root, 'kernel/lisp.js')}});
      fs.rmSync(path.join(dir, 'assets'), {recursive: true});
      for (const name of ['resource-fixture.asd', 'checks.lisp']) fs.unlinkSync(path.join(dir, name));
      let output;
      if (target === 'node') {
        const run = spawnSync(process.execPath, [outfile], {cwd: dir, encoding: 'utf8', timeout: 60000});
        assert.equal(run.status, 0, run.stdout + run.stderr);
        output = run.stdout + run.stderr;
      } else {
        server = http.createServer((req, res) => {
          if (req.url === '/app.mjs') {res.setHeader('Content-Type', 'text/javascript'); res.end(fs.readFileSync(outfile));}
          else {res.setHeader('Content-Type', 'text/html'); res.end(`<script>
            window.lines = []; window.errors = [];
            console.log = (...args) => lines.push(args.join(' '));
            console.error = (...args) => errors.push(args.join(' '));
            window.addEventListener('error', e => errors.push(e.message));
            </script><script type="module">try {await import('/app.mjs');} catch(e) {errors.push(String(e));} window.done = true;</script>`);}
        });
        await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
        chrome = await launchChrome();
        const page = await chrome.page(`http://127.0.0.1:${server.address().port}/`);
        const deadline = Date.now() + 60000;
        while (!await page.evaluate('Boolean(window.done)')) {
          assert.ok(Date.now() < deadline, 'Browser resource test timed out');
          await new Promise(resolve => setTimeout(resolve, 250));
        }
        assert.deepEqual(await page.evaluate('window.errors'), []);
        output = (await page.evaluate('window.lines')).join('\n');
      }
      assert.doesNotMatch(output, /Valtan error:/);
      assert.match(output, /^EMBEDDED-RESOURCES-PASS$/m);
    } finally {
      await chrome?.close();
      if (server) await new Promise(resolve => server.close(resolve));
      fs.rmSync(dir, {recursive: true, force: true});
    }
  });
}
