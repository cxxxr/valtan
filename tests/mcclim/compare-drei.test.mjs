import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {compareDrei} from './compare-drei.mjs';

const read = name => JSON.parse(fs.readFileSync(new URL(name, import.meta.url)));

// Keep the real browser's camelCase fields and per-file registration records.
// The completed checks below are synthetic verifier fixtures, not a target pass.
function fixture() {
  const reference = read('./drei-all-host-results.json');
  const target = read('./drei-image-upstream-results.json');
  const observedNames = new Set(target.checks.resultCounts.map(row => row.test));
  Object.assign(target, {status: 'passed', complete: true, suite: 'DREI-ALL', selfCompilation: true,
    sourceResources: structuredClone(reference.sourceResources)});
  Object.assign(target.checks, {status: 'passed', features: ['VALTAN', 'MCCLIM', 'CLIM-WITHOUT-SWANK'],
    failed: 0, skipped: 0, unexecuted: [], nonpassed: [],
    resultCounts: reference.resultCounts.map(row => ({...row,
      test: observedNames.has(`VALTAN-GRAY:${row.test}`) ? `VALTAN-GRAY:${row.test}` : row.test}))});
  target.checks.resultCounts.find(row => row.test === 'DREI-TESTS::ARGLIST-KEYWORD-P').count = 9;
  retotal(target);
  return {target, reference};
}
function retotal(target) {
  target.checks.passed = target.checks.results = target.checks.resultCounts.reduce((sum, row) => sum + row.count, 0);
}

test('compare full inventory while retaining native failures separately', () => {
  const {target, reference} = fixture(), report = compareDrei(target, reference);
  assert.deepEqual(report.errors, []);
  assert.equal(report.complete, true);
  assert.equal(report.registered, 596);
  assert.equal(report.registrationEntries, 615);
  assert.equal(report.results, 3756);
  assert.equal(report.selfCompilationSources, 34);
  assert.equal(report.implementationVariants.length, 1);
  assert.equal(report.identityMappings.length, 13);
  const metadata = read('./drei-coverage-metadata.json');
  assert.deepEqual(report.identityMappings.map(row => row.native).sort(),
    metadata.grayProtocolMappings.map(row => row.printed).sort());
  assert.ok(metadata.grayProtocolMappings.every(row => row.package === 'SB-GRAY' &&
    row.nativeCount === row.retainedTargetCount));
  assert.equal(report.nativeStatus, 'failed');
  assert.deepEqual(report.nativeFailures, reference.nonpassed);
  assert.equal(report.nativeFailures.length, 11);
  assert.equal(reference.registeredTests.length - reference.resultCounts.length, 1);
});

test('the actual retained partial browser run cannot pass', () => {
  const report = compareDrei(read('./drei-image-upstream-results.json'), read('./drei-all-host-results.json'));
  assert.equal(report.complete, false);
  assert.ok(report.errors.includes('Target has unexecuted tests'));
  assert.ok(report.errors.includes('Target has skipped assertions'));
  assert.ok(!report.errors.includes('Target assertion counts is missing'));
});

test('a target failure is rejected even when native has the same failure', () => {
  const {target, reference} = fixture();
  target.status = target.checks.status = 'failed';
  target.checks.failed = 1; target.checks.passed--;
  target.checks.nonpassed = [structuredClone(reference.nonpassed[0])];
  const report = compareDrei(target, reference);
  assert.equal(report.complete, false);
  assert.ok(report.errors.includes('Target has failed assertions'));
});

test('equal totals cannot hide a missing case or moved assertions', () => {
  for (const variant of ['case', 'assertions', 'duplicate']) {
    const {target, reference} = fixture();
    if (variant === 'case') {
      const original = reference.registeredTests[0];
      for (const file of target.checks.files)
        file.registered = file.registered.map(name => name === original ? 'DREI-TESTS::UNEXPECTED' : name);
    } else if (variant === 'assertions') {
      const rows = target.checks.resultCounts.filter(row => row.count > 1 && row.test !== 'DREI-TESTS::ARGLIST-KEYWORD-P');
      rows[0].count--; rows[1].count++;
    } else {
      const file = target.checks.files.find(file => file.registered.length > 0);
      file.registered.push(file.registered[0]); target.checks.registrationEntries++;
    }
    assert.equal(compareDrei(target, reference).complete, false);
  }
});

test('missing sources, resources, metadata, modes or successful checks fail', () => {
  for (const mutate of [
    t => {t.complete = false;},
    t => {t.suite = 'DREI-TESTS';},
    t => {t.selfCompilation = false;},
    t => {t.checks.features = ['VALTAN'];},
    t => {t.checks.skipped = 1;},
    t => {t.checks.unexecuted = ['DREI-TESTS::SELF-COMPILATION-TEST'];},
    t => {delete t.checks.resultCounts;},
    t => {delete t.checks.registrationEntries;},
    t => {t.checks.resultCounts[0] = {...t.checks.resultCounts[1]}; retotal(t);},
    t => {t.checks.files[0].error = 'read error';},
    t => {t.sources.reverse();},
    t => {t.sources[0].sha256 = '0'.repeat(64);},
    t => {t.upstreamDefinition.sha256 = '0'.repeat(64);},
    t => {t.sourceResources.pop();},
    t => {t.sourceResources[0].sha256 = '0'.repeat(64);},
  ]) {
    const {target, reference} = fixture(); mutate(target);
    assert.equal(compareDrei(target, reference).complete, false);
  }
});

test('keyword variant requires exact counts and implementation context', () => {
  for (const variant of ['count', 'native', 'target']) {
    const {target, reference} = fixture();
    if (variant === 'count') {
      target.checks.resultCounts.find(row => row.test === 'DREI-TESTS::ARGLIST-KEYWORD-P').count = 8;
      retotal(target);
    } else if (variant === 'native') reference.features = reference.features.filter(name => name !== 'SBCL');
    else target.checks.features = target.checks.features.filter(name => name !== 'VALTAN');
    assert.equal(compareDrei(target, reference).complete, false);
  }
});

test('Gray mapping does not erase arbitrary package identities', () => {
  const {target, reference} = fixture();
  const original = 'VALTAN-GRAY:STREAM-READ-CHAR', other = 'OTHER-PACKAGE:STREAM-READ-CHAR';
  for (const file of target.checks.files)
    file.registered = file.registered.map(name => name === original ? other : name);
  target.checks.resultCounts.find(row => row.test === original).test = other;
  const report = compareDrei(target, reference);
  assert.equal(report.complete, false);
  assert.ok(report.unexpected.includes(other));
});

test('incomplete or internally inconsistent native reference is rejected', () => {
  for (const mutate of [
    r => {r.complete = false;},
    r => {r.status = 'passed';},
    r => {r.skipped = 1;},
    r => {r.unexecuted = [r.registeredTests[0]];},
    r => {r.nonpassed.pop();},
    r => {r.nonpassed[0].test = 'UNREGISTERED';},
    r => {r.resultCounts[0].count++;},
    r => {r.registeredTests[0] = r.registeredTests[1];},
  ]) {
    const {target, reference} = fixture(); mutate(reference);
    assert.equal(compareDrei(target, reference).complete, false);
  }
});
