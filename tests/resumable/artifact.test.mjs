import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { execFileSync } from 'node:child_process';

test('resumable compiled artifacts suspend after restart without their source', () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-resumable-artifact-'));
  try {
    const source = path.join(directory, 'restart.lisp');
    for (const phase of ['produce', 'consume']) {
      const output = execFileSync(process.execPath,
        [new URL('./artifact-worker.mjs', import.meta.url).pathname, phase, source],
        { encoding: 'utf8', timeout: 180000, maxBuffer: 4 * 1024 * 1024 });
      assert.ok(output.includes(`RESUMABLE-ARTIFACT-${phase.toUpperCase()}-PASS`), output);
      assert.equal(fs.existsSync(source), false);
    }
  } finally { fs.rmSync(directory, { recursive: true, force: true }); }
});
