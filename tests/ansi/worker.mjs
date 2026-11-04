import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
const here = path.dirname(fileURLToPath(import.meta.url));
const upstream = path.resolve(here, '../../submodules/ansi-test');
globalThis.valtanAnsiCompile = process.env.VALTAN_ANSI_MODE || 'eval';
globalThis.valtanAnsiFixture = process.env.VALTAN_ANSI_FIXTURE || '';
globalThis.valtanAnsiFiles = process.argv.slice(2);
globalThis.valtanAnsiEvent = (kind, name, detail) => {
  // Lisp code may have just printed a partial line. Keep protocol records on
  // their own line so that such output cannot swallow a test result.
  process.stdout.write(`\n@@ANSI ${JSON.stringify({ kind, name, detail })}\n`);
};
// A native runtime exception is a failed test, never a passing condition test.
// Keep running after it so one implementation bug does not hide later results.
globalThis.valtanAnsiRunTest = (name, fn) => {
  try { fn(); }
  catch (error) {
    globalThis.valtanAnsiEvent('fail', name,
      `Uncaught JavaScript exception: ${error?.stack || String(error)}`);
  }
};
globalThis.valtanAnsiResolve = (relative, parent) => path.posix.join(path.posix.dirname(parent), relative);
globalThis.valtanAnsiSource = (relative) => {
  const base = globalThis.valtanAnsiFixture === '1' ? path.join(here, 'fixtures') : upstream;
  const file = path.resolve(base, relative);
  if (!file.startsWith(base + path.sep)) throw new Error('Path outside ansi-test');
  const source = fs.readFileSync(file, 'utf8');
  return source;
};
await import(process.env.VALTAN_ANSI_BUNDLE || '../dist-ansi/ansi.js');
