import test from 'node:test';
import assert from 'node:assert/strict';
import { summarizeUpstream } from './portability-report.mjs';
const names = ['COMPARE-FILES', 'STRING-TESTS', 'SEQUENCE-TESTS', 'ERROR-HANDLING-TESTS',
  'UNREAD-CHAR-TESTS', 'COLUMN-TESTS', 'MAKE-EXTERNAL-FORMAT-TESTS', 'PEEK-BYTE-TESTS', 'IN-MEMORY-STREAM-TESTS'];
const success = ['BABEL-TEST FIRST', 'BABEL-TEST SECOND', 'PORTABILITY-SUITE babel passed 2 7',
  ...names.map(n => `...PORTABILITY-SUITE flexi-${n} passed`), 'PORTABILITY-UPSTREAM-COMPLETE'].join('\n');
test('upstream result requires complete independent registration coverage', () => {
  assert.equal(summarizeUpstream(success, ['FIRST', 'SECOND']).status, 'passed');
  for (const text of [success.replace('BABEL-TEST SECOND\n', ''), success.replace('BABEL-TEST SECOND', 'BABEL-TEST FIRST'),
    success.replace('flexi-STRING-TESTS', 'flexi-COMPARE-FILES'), success.replace('PORTABILITY-UPSTREAM-COMPLETE', '')]) {
    assert.equal(summarizeUpstream(text, ['FIRST', 'SECOND']).complete, false);
    assert.equal(summarizeUpstream(text, ['FIRST', 'SECOND']).status, 'failed');
  }
});
test('complete upstream failures remain failures', () => {
  const text = success.replace('babel passed', 'babel failed').replace('BABEL-TEST SECOND', 'BABEL-TEST SECOND\nBABEL-FAIL SECOND: condition')
    .replace('flexi-STRING-TESTS passed', 'flexi-STRING-TESTS failed');
  const result = summarizeUpstream(text, ['FIRST', 'SECOND']);
  assert.equal(result.complete, true);
  assert.equal(result.status, 'failed');
  assert.deepEqual(result.babelFailures, ['SECOND']);
  assert.equal(result.flexiSuites.filter(s => s.status === 'failed').length, 1);
});

import { combinePortability } from './portability-combine.mjs';
test('combined coverage rejects missing or overlapping worker scenarios', () => {
  const expectedCases = Array.from({length: 4}, (_, index) => ({index, scenario: `scenario-${index}`}));
  const babel = {suite: 'babel', complete: true, status: 'passed', babelTests: ['TEST'], expectedBabelTests: ['TEST']};
  const flexi = {jobs: 2, complete: true, status: 'passed', comparisons: 32, expectedCases,
    workers: [0, 1].map(index => ({index, complete: true, cases: expectedCases.filter(c => c.index % 2 === index),
      suites: (index ? names.slice(0,1) : names).map(n => ({name: 'flexi-' + n, status: 'passed'}))}))};
  assert.equal(combinePortability(babel, flexi).complete, true);
  assert.equal(combinePortability({...babel, status: 'failed'}, flexi).status, 'failed');
  assert.equal(combinePortability(babel, {...flexi, workers: flexi.workers.slice(0,1)}).complete, false);
  const missingSuite = structuredClone(flexi); missingSuite.workers[0].suites.pop();
  assert.equal(combinePortability(babel, missingSuite).complete, false);
  const duplicate = structuredClone(flexi); duplicate.workers[0].cases[0] = duplicate.workers[1].cases[0];
  assert.equal(combinePortability(babel, duplicate).complete, false);
  assert.equal(combinePortability(babel, {...flexi, comparisons: 24}).complete, false);
});

import { normalizeFlexiCase, summarizeFlexiWorker } from './flexi-report.mjs';
test('external-format identities are ignored but encoding options are retained', () => {
  const a = '#<FLEXI-STREAMS::FLEXI-UTF-16-FORMAT (:UTF-16 :LITTLE-ENDIAN T) {ABC123}>';
  const b = '#<FLEXI-UTF-16-FORMAT (:UTF-16 :LITTLE-ENDIAN T) A>';
  assert.equal(normalizeFlexiCase(a), normalizeFlexiCase(b));
  assert.notEqual(normalizeFlexiCase(a), normalizeFlexiCase(b.replace('ENDIAN T', 'ENDIAN NIL')));
  assert.throws(() => normalizeFlexiCase('#<UNKNOWN 1>'));
});
test('worker outcomes require exact scenarios and completion, while retaining failures', () => {
  const expected = [{index: 0, scenario: 'first'}, {index: 1, scenario: 'second'}];
  const text = 'FLEXI-CASE 1 second\nPORTABILITY-SUITE flexi-COMPARE-FILES failed\nFLEXI-SHARD 1 2 2 1\nFLEXI-SHARD-COMPLETE 1\n';
  const result = summarizeFlexiWorker(text, expected, 1, 2);
  assert.equal(result.complete, true); assert.equal(result.status, 'failed');
  for (const changed of [text.replace('second', 'first'), text.replace('FLEXI-SHARD 1 2 2 1', 'FLEXI-SHARD 1 2 2 2'),
    text.replace('FLEXI-SHARD-COMPLETE 1', '')]) assert.equal(summarizeFlexiWorker(changed, expected, 1, 2).complete, false);
});
