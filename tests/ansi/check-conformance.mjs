import fs from 'node:fs';
import path from 'node:path';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import { manifest, revision } from './run.mjs';
import { auditRegistrations } from './registration-audit.mjs';

const inventory = JSON.parse(fs.readFileSync(new URL('./reference-inventory.json', import.meta.url)));
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
const count = value => Number.isSafeInteger(value) && value >= 0;
const names = value => Array.isArray(value) && value.every(name => typeof name === 'string') &&
  new Set(value).size === value.length;

// Validate the complete upstream measurement before applying the 99% gate.
// In particular, a selected run or a smaller denominator cannot satisfy it.
export function checkConformance(report, bundleSha256) {
  const require = (condition, message) => { if (!condition) throw new Error(message); };
  require(report?.suite === 'ansi-test' && report.revision === revision, 'Wrong suite or upstream revision');
  require(report.selection === 'all', 'A full upstream run is required');
  require(['eval', 'compiled'].includes(report.mode), 'Unknown execution mode');
  require(report.complete === true, 'The full run is incomplete');
  require(Number.isFinite(Date.parse(report.startedAt)) && Number.isFinite(Date.parse(report.finishedAt)) &&
    Date.parse(report.finishedAt) >= Date.parse(report.startedAt), 'Missing or invalid run timestamps');
  require(/^[a-f0-9]{64}$/.test(report.bundleSha256), 'Missing runtime bundle identity');
  if (bundleSha256) require(report.bundleSha256 === bundleSha256, 'Runtime bundle does not match the measured build');
  require(Array.isArray(report.results) && same(report.results.map(result => result.name), Object.keys(manifest)),
    'Every upstream category must appear exactly once in loader order');

  let pass = 0, fail = 0;
  for (const result of report.results) {
    const label = result.name;
    const files = manifest[label], expected = inventory.categories[label];
    require(same(result.files, files), `${label}: file selection differs from upstream`);
    require(result.status === 'finished' && result.complete === true && result.mode === report.mode,
      `${label}: execution is incomplete`);
    require(result.exitCode === 0 && result.signal === null && result.timedOut === false &&
      result.fatalError === false && result.unregisteredTestsUnknown === false &&
      result.unexecuted === 0 && result.pendingTest === null, `${label}: execution did not finish cleanly`);
    require(Array.isArray(result.errors) && result.errors.length === 0 &&
      Array.isArray(result.unreadFiles) && result.unreadFiles.length === 0 &&
      Array.isArray(result.completedFiles) && files.every(file => result.completedFiles.includes(file)),
      `${label}: missing or failed file loads`);
    require(count(result.pass) && count(result.fail) && count(result.registered) && result.registered > 0 &&
      result.pass + result.fail === result.registered, `${label}: inconsistent execution counts`);
    require(result.expectedRegistered === expected && names(result.missingReferenceTests) && names(result.unexpectedTests),
      `${label}: missing registration audit`);

    const reference = new Set(files.flatMap(file => inventory.testNames[file] ?? []));
    const missing = new Set(result.missingReferenceTests);
    const observed = new Set([...reference].filter(name => !missing.has(name)).concat(result.unexpectedTests));
    const audit = auditRegistrations(files, observed, inventory, expected);
    require(same([...audit.missingReferenceTests].sort(), [...result.missingReferenceTests].sort()) &&
      same([...audit.unexpectedTests].sort(), [...result.unexpectedTests].sort()) &&
      !audit.referenceInventoryMismatch && !audit.registrationNameMismatch &&
      result.referenceInventoryMismatch === false && result.registrationNameMismatch === false &&
      result.registrationMismatch === false && observed.size === result.registered &&
      result.registered === audit.effectiveExpectedRegistered &&
      result.effectiveExpectedRegistered === audit.effectiveExpectedRegistered,
      `${label}: registration audit does not prove full coverage`);
    require(Array.isArray(result.failures) && result.failures.length === result.fail &&
      names(result.failures.map(failure => failure.test)) &&
      result.failures.every(failure => observed.has(failure.test)), `${label}: inconsistent failure records`);
    pass += result.pass;
    fail += result.fail;
  }
  require(report.pass === pass && report.fail === fail, 'Full-run totals do not match category totals');
  const total = pass + fail, rate = 100 * pass / total;
  require(Number.isFinite(report.passRate) && Math.abs(report.passRate - rate) < 1e-9, 'Incorrect recorded pass rate');
  require(pass * 100 >= total * 99, `ANSI success rate is below 99%: ${pass}/${total} (${rate.toFixed(2)}%)`);
  return { pass, fail, total, rate, categories: report.results.length };
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  try {
    const [reportPath, bundlePath, ...extra] = process.argv.slice(2);
    if (!reportPath || extra.length) throw new Error('Usage: node ansi/check-conformance.mjs REPORT.json [RUNTIME-BUNDLE.js]');
    const bundleSha256 = bundlePath && createHash('sha256').update(fs.readFileSync(bundlePath)).digest('hex');
    const result = checkConformance(JSON.parse(fs.readFileSync(reportPath)), bundleSha256);
    console.log(`ANSI: ${result.pass}/${result.total} (${result.rate.toFixed(2)}%), all ${result.categories} categories complete`);
  } catch (error) {
    console.error(error.message);
    process.exitCode = 1;
  }
}
