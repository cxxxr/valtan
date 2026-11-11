import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { execFileSync } from 'node:child_process';

for (const mode of ['ordinary', 'scheduled']) {
  test(`file literals survive restart with identity and load-form ordering (${mode})`, () => {
    const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-file-literals-'));
    try {
      for (const phase of ['produce', 'consume']) {
        const output = execFileSync(process.execPath,
          [new URL('./file-literals-worker.mjs', import.meta.url).pathname, phase, directory, mode],
          { encoding: 'utf8', timeout: 300000, maxBuffer: 8 * 1024 * 1024 });
        assert.ok(output.includes(`FILE-LITERALS-${mode}-${phase}-PASS`), output);
      }
    } finally { fs.rmSync(directory, { recursive: true, force: true }); }
  });
}
