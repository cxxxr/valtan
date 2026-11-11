import fs from 'node:fs';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
import path from 'node:path';
export function combinePortability(babel, flexi) {
  const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
  const expected = flexi.expectedCases ?? [];
  const workers = flexi.workers ?? [];
  const suiteNames = ['COMPARE-FILES', 'STRING-TESTS', 'SEQUENCE-TESTS', 'ERROR-HANDLING-TESTS',
    'UNREAD-CHAR-TESTS', 'COLUMN-TESTS', 'MAKE-EXTERNAL-FORMAT-TESTS', 'PEEK-BYTE-TESTS', 'IN-MEMORY-STREAM-TESTS'].map(n => 'flexi-' + n);
  const workerCoverage = expected.length > 0 && expected.every((c, i) => c.index === i) &&
    workers.length === flexi.jobs && workers.every((w, i) => w.index === i && w.complete &&
      same(w.cases, expected.filter(c => c.index % flexi.jobs === i)) &&
      same(w.suites?.map(s => s.name), i === 0 ? suiteNames : suiteNames.slice(0, 1)));
  const complete = !!babel.complete && babel.suite === 'babel' && babel.babelTests?.length > 0 &&
    same(babel.babelTests, babel.expectedBabelTests) && !!flexi.complete && workerCoverage &&
    flexi.comparisons === expected.length * 8;
  const flexiSuites = (workers[0]?.suites ?? []).map(s => s.name === 'flexi-COMPARE-FILES'
    ? { name: s.name, status: workers.every(w => w.suites?.find(s => s.name === 'flexi-COMPARE-FILES')?.status === 'passed') ? 'passed' : 'failed' } : s);
  return { schemaVersion: 3, timestamp: new Date().toISOString(),
    scope: 'Complete default Babel test selection and all nine FLEXI-STREAMS suites, including every file-comparison scenario across disjoint workers',
    complete, status: complete && babel.status === 'passed' && !(babel.babelFailures?.length) && flexi.status === 'passed' &&
      workers.every(w => w.suites.every(s => s.status === 'passed')) ? 'passed' : 'failed',
    babelTests: babel.babelTests, babelFailures: babel.babelFailures, babelAssertions: babel.babelAssertions,
    flexiSuites, comparisons: flexi.comparisons, runs: { babel, flexi } };
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const [babelPath, flexiPath, output] = process.argv.slice(2);
  if (!babelPath || !flexiPath || !output || process.argv.length !== 5) throw new Error('Usage: portability-combine.mjs babel.json flexi.json output.json');
  const result = combinePortability(JSON.parse(fs.readFileSync(babelPath)), JSON.parse(fs.readFileSync(flexiPath)));
  result.aggregationSha256 = createHash('sha256').update(fs.readFileSync(fileURLToPath(import.meta.url))).digest('hex');
  fs.writeFileSync(output, JSON.stringify(result, null, 2) + '\n');
  console.log(`Upstream coverage complete=${result.complete}, status=${result.status}`);
  if (result.status !== 'passed') process.exitCode = 1;
}
