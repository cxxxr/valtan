import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {compareUpstream} from './compare-upstream.mjs';

// Construct the target-shaped equivalent of the recorded native inventory.
// These fixtures validate the comparer; they are not target execution evidence.
function fixture() {
  const reference = JSON.parse(fs.readFileSync(new URL('./upstream-host-results.json', import.meta.url)));
  const sources = reference.sourcePlan.map(row => ({original: row.file, file: row.file, sha256: row.sha256}));
  const checks = {status: 'passed', features: reference.features.filter(f => f !== 'FAKE-PROGV-KLUDGE'),
    registered: reference.registered, 'registration-entries': reference.registrationEntries,
    files: sources.map((row, i) => ({file: row.file, error: null, registered: i ? [] : [...reference.registeredTests]})),
    'result-counts': reference.resultCounts.map(row => ({...row})),
    failed: 0, skipped: 0, unexecuted: [], nonpassed: []};
  for (const row of checks['result-counts'])
    if (['CLIM-TESTS::STRESS.9', 'CLIM-TESTS::STRESS.0'].includes(row.test)) row.count = 33;
  const target = {status: 'passed', complete: true, suite: 'MCCLIM', sources, checks,
    upstreamDefinition: {file: reference.upstreamDefinition, sha256: reference.upstreamDefinitionSha256}};
  retotal(target);
  return {target, reference};
}
function retotal(target) {
  target.checks.passed = target.checks.results = target.checks['result-counts'].reduce((n, row) => n + row.count, 0);
}
test('exact inventory allows recorded feature variants and zero-assertion cases', () => {
  const {target, reference} = fixture(), report = compareUpstream(target, reference);
  assert.deepEqual(report.errors, []); assert.equal(report.status, 'passed');
  assert.equal(report.registered, 193); assert.equal(report.results, 2245);
  assert.equal(report.featureVariants.length, 2);
  assert.equal(reference.registeredTests.length - reference.resultCounts.length, 2);
});
test('equal totals cannot hide a missing test or assertions moved between tests', () => {
  for (const variant of ['registration', 'assertions']) {
    const {target, reference} = fixture();
    if (variant === 'registration') target.checks.files[0].registered[0] = 'CLIM-TESTS::UNEXPECTED';
    else {target.checks['result-counts'][0].count--; target.checks['result-counts'][1].count++;}
    const report = compareUpstream(target, reference);
    assert.equal(report.status, 'failed');
    if (variant === 'registration') assert.equal(report.missing.length, 1);
    else assert.equal(report.differences.length, 2);
  }
});
test('source drift, missing MCCLIM, load failure and duplicate registrations fail', () => {
  for (const mutate of [
    t => {t.sources[0].sha256 = '0'.repeat(64);},
    t => {t.upstreamDefinition.sha256 = '0'.repeat(64);},
    t => {t.checks.features = t.checks.features.filter(f => f !== 'MCCLIM');},
    t => {t.checks.files[0].error = 'read failure';},
    t => {t.checks.files[0].registered[0] = t.checks.files[0].registered[1];},
    t => {t.checks['result-counts'][0] = {...t.checks['result-counts'][1]}; retotal(t);},
  ]) {
    const {target, reference} = fixture(); mutate(target);
    assert.equal(compareUpstream(target, reference).status, 'failed');
  }
});
test('failed, skipped, unfinished and unexecuted runs cannot pass the comparison', () => {
  for (const mutate of [
    t => {t.status = 'failed';}, t => {t.complete = false;},
    t => {t.checks.failed = 1;}, t => {t.checks.skipped = 1;},
    t => {t.checks.unexecuted = [t.checks.files[0].registered[0]];},
    t => {t.checks.nonpassed = [{test: 'failed'}];},
    t => {delete t.checks['result-counts'];},
  ]) {
    const {target, reference} = fixture(); mutate(target);
    assert.equal(compareUpstream(target, reference).complete, false);
  }
});
test('feature exception does not accept arbitrary counts or the wrong feature context', () => {
  for (const variant of ['count', 'target', 'reference']) {
    const {target, reference} = fixture();
    if (variant === 'count') {
      target.checks['result-counts'].find(row => row.test === 'CLIM-TESTS::STRESS.9').count++;
      retotal(target);
    } else if (variant === 'target') target.checks.features.push('FAKE-PROGV-KLUDGE');
    else reference.features = reference.features.filter(f => f !== 'FAKE-PROGV-KLUDGE');
    assert.equal(compareUpstream(target, reference).status, 'failed');
  }
});
