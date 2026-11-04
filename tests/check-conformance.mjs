// Expected top-level forms were counted independently with SBCL's reader and :valtan in *features*.
// Check coverage as well as the pass rate: a reader abort must never improve it.
import { readFileSync } from 'node:fs';

const expected = JSON.parse(readFileSync(new URL('./sacla-manifest.json', import.meta.url)));
const logPath = process.argv[2];
if (!logPath) throw new Error('Usage: node check-conformance.mjs <test-output.txt>');
const log = readFileSync(logPath, 'utf8');
const errors = [];
const results = new Map();
for (const match of log.matchAll(/^--- (\S+): Pass=(\d+), Fail=(\d+) \(\d+ms\) ---$/gm)) {
  const [, file, pass, fail] = match;
  if (results.has(file)) errors.push(`Duplicate file: ${file}`);
  results.set(file, { pass: Number(pass), fail: Number(fail) });
}
for (const [file, count] of Object.entries(expected)) {
  const result = results.get(file);
  if (!result) errors.push(`Missing file: ${file}`);
  else if (result.pass + result.fail !== count) {
    errors.push(`${file}: executed ${result.pass + result.fail}, expected ${count}`);
  }
}
for (const file of results.keys()) {
  if (!(file in expected)) errors.push(`Unexpected file: ${file}`);
}
if (/Valtan error:|Error loading test file/.test(log)) errors.push('Execution aborted');
if (!/^Incomplete files: 0$/m.test(log)) errors.push('Missing complete-run marker');
const pass = [...results.values()].reduce((sum, result) => sum + result.pass, 0);
const fail = [...results.values()].reduce((sum, result) => sum + result.fail, 0);
const totals = [...log.matchAll(/^TOTAL: Pass=(\d+), Fail=(\d+) \(\d+ms\)$/gm)];
if (totals.length !== 1 || Number(totals[0][1]) !== pass || Number(totals[0][2]) !== fail) {
  errors.push('Missing or inconsistent total');
}
const total = Object.values(expected).reduce((sum, count) => sum + count, 0);
if (pass * 100 < total * 90) errors.push(`Pass rate below 90%: ${pass}/${total}`);
if (errors.length) {
  console.error(errors.join('\n'));
  process.exitCode = 1;
} else {
  console.log(`SACLA: ${pass}/${total} (${(100 * pass / total).toFixed(2)}%), all ${results.size} files complete`);
}
