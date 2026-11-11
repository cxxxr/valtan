// Verify the production portless-queue adapter against real upstream queues.
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {spawn} from 'node:child_process';
const here = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(here, '../..');
const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-queue-checks-'));
const checks = path.join(directory, 'checks.lisp');
const sources = ['library/mcclim-browser/package.lisp', 'library/mcclim-browser/queues.lisp', 'tests/mcclim/queue-checks.lisp'];
fs.writeFileSync(checks, sources.map(file => fs.readFileSync(path.join(root, file), 'utf8')).join('\n'));
const output = path.resolve(process.argv[2] ?? path.join(here, 'queue-results.json'));
const child = spawn(process.execPath, [path.join(here, 'probe.mjs'), 'clim-core/silex', output, checks], {stdio: 'inherit'});
child.on('error', error => {console.error(error); process.exitCode = 1;});
child.on('exit', code => {process.exitCode = code ?? 1;});
