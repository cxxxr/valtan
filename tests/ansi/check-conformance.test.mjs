import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import { manifest, revision } from './run.mjs';
import { auditRegistrations } from './registration-audit.mjs';
import { checkConformance } from './check-conformance.mjs';

const inventory = JSON.parse(fs.readFileSync(new URL('./reference-inventory.json', import.meta.url)));
const sha = 'a'.repeat(64);

// Synthetic protocol records exercise the gate; they are not runtime results.
function reportWithFailures(failures = 0) {
  const results = Object.entries(manifest).map(([name, files]) => {
    const observed = [...new Set(files.flatMap(file => inventory.testNames[file] ?? []))];
    const expectedRegistered = inventory.categories[name];
    const fail = Math.min(failures, observed.length);
    failures -= fail;
    return { name, files, mode: 'eval', status: 'finished', complete: true,
      registered: observed.length, pass: observed.length - fail, fail, expectedRegistered,
      completedFiles: files, errors: [], unreadFiles: [], exitCode: 0, signal: null,
      timedOut: false, fatalError: false, unregisteredTestsUnknown: false,
      unexecuted: 0, pendingTest: null, registrationMismatch: false,
      failures: observed.slice(0, fail).map(test => ({ test, reason: 'Synthetic failure' })),
      ...auditRegistrations(files, observed, inventory, expectedRegistered) };
  });
  const pass = results.reduce((sum, result) => sum + result.pass, 0);
  const fail = results.reduce((sum, result) => sum + result.fail, 0);
  return { suite: 'ansi-test', revision, mode: 'eval', selection: 'all', complete: true,
    startedAt: '2026-11-04T00:00:00Z', finishedAt: '2026-11-04T01:00:00Z',
    bundleSha256: sha, results, pass, fail, passRate: 100 * pass / (pass + fail) };
}

test('ANSI gate applies the 99% boundary to the complete upstream denominator', () => {
  const total = inventory.registered, maximumFailures = Math.floor(total / 100);
  assert.equal(checkConformance(reportWithFailures(maximumFailures), sha).total, total);
  const below = reportWithFailures(maximumFailures + 1);
  assert.equal(below.passRate.toFixed(2), '99.00'); // Rounding must not satisfy the gate.
  assert.throws(() => checkConformance(below), /below 99%/);
});

test('ANSI gate rejects selected, missing, duplicate and truncated categories', () => {
  for (const change of [
    report => { report.selection = 'selected'; },
    report => { report.results.pop(); },
    report => { report.results[1] = report.results[0]; },
    report => { report.results[0].files = report.results[0].files.slice(1); },
    report => { report.results[0].registered--; report.results[0].pass--; },
  ]) {
    const report = reportWithFailures(); change(report);
    assert.throws(() => checkConformance(report));
  }
});

test('ANSI gate rejects incomplete execution, inconsistent totals and stale bundles', () => {
  for (const change of [
    report => { report.complete = false; },
    report => { delete report.finishedAt; },
    report => { report.results[0].timedOut = true; },
    report => { report.results[0].errors.push({ kind: 'read-error' }); },
    report => { report.results[0].completedFiles = []; },
    report => { report.pass--; },
    report => { report.passRate = 90; },
    report => { report.results[0].failures.push({ test: 'UNREPORTED' }); },
  ]) {
    const report = reportWithFailures(); change(report);
    assert.throws(() => checkConformance(report));
  }
  assert.throws(() => checkConformance(reportWithFailures(), 'b'.repeat(64)), /does not match/);
});

test('ANSI gate independently rejects equal-count substitution in registration records', () => {
  const report = reportWithFailures(), result = report.results[0];
  const original = inventory.testNames[result.files.find(file => inventory.testNames[file]?.length)][0];
  result.missingReferenceTests = [original];
  result.unexpectedTests = ['UNRELATED-TEST'];
  assert.throws(() => checkConformance(report), /registration audit/);
});

test('ANSI gate retains outcomes when auditing character aliases and conditional registrations', () => {
  const report = reportWithFailures();
  for (const result of report.results) {
    const observed = new Set(result.files.flatMap(file => inventory.testNames[file] ?? []));
    if (result.name === 'reader') {
      observed.delete('SET-SYNTAX-FROM-CHAR-TRAIT-X-LATIN_SMALL_LETTER_A');
      observed.add('SET-SYNTAX-FROM-CHAR-TRAIT-X-a');
      result.pass--; result.fail++;
      result.failures.push({ test: 'SET-SYNTAX-FROM-CHAR-TRAIT-X-a', reason: 'Still fails' });
    }
    if (result.name === 'eval-and-compile') {
      for (let index = 4; index <= 11; index++) observed.delete(`DECLARATION.${index}`);
      result.pass -= 8; result.registered -= 8;
    }
    if (result.name === 'objects') {
      observed.add('DEFGENERIC.30'); result.pass++; result.registered++;
    }
    Object.assign(result, auditRegistrations(result.files, observed, inventory, result.expectedRegistered));
  }
  report.pass = report.results.reduce((sum, result) => sum + result.pass, 0);
  report.fail = 1;
  report.passRate = 100 * report.pass / (report.pass + report.fail);
  const result = checkConformance(report);
  assert.equal(result.total, 21937);
  assert.equal(result.fail, 1);
});
