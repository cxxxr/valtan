import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { runWorker, manifest, revision } from './run.mjs';
import { auditRegistrations } from './registration-audit.mjs';

test('registration audit permits only the named upstream conditional alternatives', () => {
  const declarationFile = 'eval-and-compile/declaration.lsp';
  const genericFile = 'objects/defgeneric.lsp';
  const declarations = Array.from({ length: 11 }, (_, i) => `DECLARATION.${i + 1}`);
  const inventory = { testNames: { [declarationFile]: declarations, [genericFile]: ['DEFGENERIC.1'] } };
  const files = [declarationFile, genericFile];
  const actual = [...declarations.slice(0, 3), 'DEFGENERIC.1', 'DEFGENERIC.30'];
  const result = auditRegistrations(files, actual, inventory, 12);
  assert.equal(result.effectiveExpectedRegistered, 5);
  assert.equal(result.registrationNameMismatch, false);
  assert.equal(result.referenceInventoryMismatch, false);
  assert.equal(result.conditionalRegistrations.length, 2);
  // Equal counts cannot hide substitution of an unrelated test.
  assert.equal(auditRegistrations(files, [...actual.slice(1), 'UNEXPECTED'], inventory, 12).registrationNameMismatch, true);
  // A partially missing guarded group is not an audited alternative.
  assert.equal(auditRegistrations(files, [...actual, 'DECLARATION.4'], inventory, 12).registrationNameMismatch, true);
  // The extra test still has to run; auditing never changes its outcome.
  assert.deepEqual(result.unexpectedTests, ['DEFGENERIC.30']);
});

test('pinned upstream files and unmodified comparator are present', () => {
  const root = new URL('../../submodules/ansi-test/', import.meta.url);
  assert.equal(execFileSync('git', ['-C', root.pathname, 'rev-parse', 'HEAD'], { encoding: 'utf8' }).trim(), revision);
  assert.equal(Object.keys(manifest).length, 24);
  for (const files of Object.values(manifest)) for (const file of files) assert.ok(fs.existsSync(new URL(file, root)), file);
  const rt = fs.readFileSync(new URL('rt.lsp', root), 'utf8');
  const comparator = rt.slice(rt.indexOf('(defun equalp-with-case'), rt.indexOf('(defun compile*')).trim();
  assert.ok(fs.readFileSync(new URL('./rt-compat.lisp', import.meta.url), 'utf8').includes(comparator));
});

test('registration aliases preserve each character and cannot hide missing tests', () => {
  const file = 'reader/set-syntax-from-char.lsp';
  const prefix = 'SET-SYNTAX-FROM-CHAR-TRAIT-X-';
  const names = ['LATIN_CAPITAL_LETTER_A', 'LATIN_SMALL_LETTER_A', 'EXCLAMATION_MARK'];
  const inventory = { testNames: { [file]: names.map(name => prefix + name) } };
  const actual = ['A', 'a', '!'].map(name => prefix + name);
  const audit = observed => auditRegistrations([file], observed, inventory, 3);
  const result = audit(actual);
  assert.equal(result.registrationNameMismatch, false);
  assert.equal(result.effectiveExpectedRegistered, 3);
  assert.equal(result.aliasedRegistrations.length, 3);
  assert.equal(audit(actual.slice(1)).registrationNameMismatch, true);
  assert.equal(audit([actual[0], actual[0], actual[2]]).registrationNameMismatch, true);
  assert.equal(audit([actual[0], prefix + 'UNKNOWN', actual[2]]).registrationNameMismatch, true);
  assert.equal(audit([...actual, prefix + names[0]]).registrationNameMismatch, true);
  const otherFile = 'reader/other.lsp';
  assert.equal(auditRegistrations([otherFile], actual,
    { testNames: { [otherFile]: inventory.testNames[file] } }, 3).registrationNameMismatch, true);
});

test('adapter preserves values, case-sensitive comparison and failure reporting', async () => {
  const statuses = [];
  const r = await runWorker('protocol', ['protocol.lsp'], { fixture: true, timeout: 15000, onProgress: p => statuses.push(p.status) });
  assert.equal(statuses[0], 'running');
  assert.equal(statuses.at(-1), 'finished');
  assert.equal(r.complete, true, JSON.stringify(r));
  assert.equal(r.registered, 9);
  assert.equal(r.pass, 4);
  assert.equal(r.fail, 5);
  assert.equal(r.unexecuted, 0);
  assert.match(r.diagnostics, /INLINE-PROTOCOL-REGRESSION/);
  assert.deepEqual(r.failures.slice(-2).map(failure => failure.test), ['mixed-Case', 'MIXED-CASE']);
});

test('reader failure cannot produce a complete result', async () => {
  const r = await runWorker('reader', ['read-error.lsp'], { fixture: true, timeout: 15000 });
  assert.equal(r.complete, false);
  assert.ok(r.errors.some(e => e.kind === 'read-error'));
  assert.deepEqual(r.unreadFiles, ['read-error.lsp']);
});

test('large and circular failure diagnostics retain failures and allow subsequent tests', async () => {
  const r = await runWorker('diagnostic-limits', ['diagnostic-limits.lsp'], { fixture: true, timeout: 15000 });
  assert.equal(r.complete, true, JSON.stringify(r));
  assert.equal(r.registered, 4);
  assert.equal(r.pass, 1);
  assert.equal(r.fail, 3);
  assert.equal(r.unexecuted, 0);
  assert.deepEqual(r.failures.map(failure => failure.test), [
    'LARGE-FAILURE-REMAINS-A-FAILURE',
    'CIRCULAR-FAILURE-REMAINS-A-FAILURE',
    'CIRCULAR-CONDITION-REMAINS-A-FAILURE',
  ]);
  assert.ok(r.failures.every(failure => failure.reason.length < 2000));
  assert.match(r.failures[0].reason, /diagnostic display/);
});

test('runtime conformance regressions', async () => {
  const r = await runWorker('core-regressions', ['core-regressions.lsp'], { fixture: true, timeout: 30000 });
  assert.equal(r.complete, true, JSON.stringify(r));
  assert.equal(r.registered, 20);
  assert.equal(r.pass, 20, JSON.stringify(r.failures));
  assert.equal(r.fail, 0);
});

for (const [file, count] of [['type-regressions.lsp', 17], ['numeric-type-regressions.lsp', 8], ['compiler-regressions.lsp', 73], ['compilation-file-regressions.lsp', 5], ['place-regressions.lsp', 15], ['cons-regressions.lsp', 5], ['literal-regressions.lsp', 4], ['container-regressions.lsp', 7], ['hash-growth.lsp', 1], ['registry.lsp', 3], ['package-regressions.lsp', 15], ['number-regressions.lsp', 29], ['iteration-regressions.lsp', 4], ['character-hash-regressions.lsp', 5], ['displacement-regressions.lsp', 7], ['adjust-array-regressions.lsp', 8], ['format-regressions.lsp', 25], ['reader-escape-regressions.lsp', 8], ['reader-syntax-regressions.lsp', 10], ['printer-regressions.lsp', 10], ['pprint-regressions.lsp', 24], ['stream-regressions.lsp', 15], ['file-regressions.lsp', 10], ['pathname-regressions.lsp', 6], ['clos-regressions.lsp', 35], ['class-change-regressions.lsp', 8], ['integer-field-regressions.lsp', 5], ['transcendental-regressions.lsp', 6], ['structure-class-regressions.lsp', 5], ['structure-layout-regressions.lsp', 13], ['environment-regressions.lsp', 20], ['sequence-regressions.lsp', 12]]) {
  test(file, async () => {
    const r = await runWorker(file, [file], { fixture: true, timeout: 120000 });
    assert.equal(r.complete, true, JSON.stringify(r));
    assert.equal(r.registered, count);
    assert.equal(r.pass, count, JSON.stringify(r.failures));
    assert.equal(r.fail, 0);
    if (file === 'format-regressions.lsp') assert.match(r.diagnostics, /FORMAT-FLUSH-REGRESSION/);
  });
}

test('native exceptions fail their test and allow subsequent tests to run', async () => {
  const r = await runWorker('native-error', ['native-error.lsp'], { fixture: true, timeout: 15000 });
  assert.equal(r.complete, true, JSON.stringify(r));
  assert.equal(r.registered, 2);
  assert.equal(r.pass, 1);
  assert.equal(r.fail, 1);
  assert.match(r.failures[0].reason, /Uncaught JavaScript exception: SyntaxError/);
});

test('compiled artifacts load in a fresh process without their source', () => {
  const scratch = fs.mkdtempSync(path.join(os.tmpdir(), 'valtan-compiled-artifact-'));
  try {
    for (const file of ['compile-artifact-producer.lsp', 'compile-artifact-consumer.lsp']) {
      const output = execFileSync(process.execPath, [new URL('./worker.mjs', import.meta.url).pathname, file], {
        cwd: scratch,
        env: { ...process.env, VALTAN_ANSI_FIXTURE: '1', VALTAN_ANSI_MODE: 'eval' },
        encoding: 'utf8', timeout: 30000, maxBuffer: 4 * 1024 * 1024,
      });
      const events = output.split('\n').filter(line => line.startsWith('@@ANSI '))
        .map(line => JSON.parse(line.slice(7)));
      assert.ok(events.some(event => event.kind === 'complete'), output);
      assert.equal(events.filter(event => event.kind === 'pass').length, 1, output);
      assert.equal(events.filter(event => event.kind === 'fail' || event.kind.endsWith('-error')).length, 0, output);
      assert.equal(fs.existsSync(path.join(scratch, 'restart-source.lisp')), false);
    }
  } finally {
    fs.rmSync(scratch, { recursive: true, force: true });
  }
});

test('missing registrations cannot produce a complete benchmark result', async () => {
  const r = await runWorker('registration-count', ['protocol.lsp'], {
    fixture: true, timeout: 15000, expectedRegistered: 10,
  });
  assert.equal(r.registered, 9);
  assert.equal(r.registrationMismatch, true);
  assert.equal(r.complete, false);
});

test('a hung test is killed and retained as unexecuted', async () => {
  const pendingWhileRunning = [];
  const r = await runWorker('timeout', ['timeout.lsp'], {
    fixture: true, timeout: 3000,
    onProgress: p => { if (p.status === 'running') pendingWhileRunning.push(p.pendingTest); },
  });
  assert.equal(r.complete, false);
  assert.equal(r.timedOut, true);
  assert.equal(r.unexecuted, 1);
  assert.match(r.pendingTest, /NEVER-FINISHES/);
  assert.ok(pendingWhileRunning.some(name => /NEVER-FINISHES/.test(name)));
});

// This small upstream smoke check does not establish a full conformance rate.
test('unmodified upstream ATOM definitions actually execute in Valtan', async () => {
  const r = await runWorker('upstream-atom', ['auxiliary/cons-aux.lsp', 'cons/atom.lsp'], { timeout: 300000, expectedRegistered: 5 });
  assert.equal(r.registered, 5, JSON.stringify(r));
  assert.equal(r.pass, 5, JSON.stringify(r.failures));
  assert.equal(r.fail, 0);
  assert.equal(r.complete, true, JSON.stringify(r.errors));
  assert.ok(r.completedFiles.includes('cons/atom.lsp'));
});
