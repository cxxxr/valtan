import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
// External-format objects print their complete normalized format followed by an
// implementation-specific identity. Compare formats, never object addresses.
export function normalizeFlexiCase(text) {
  const result = text.replace(/#<(?:FLEXI-STREAMS::)?FLEXI-[\w-]+ (\([^<>]*\)) (?:\{[0-9A-Fa-f]+\}|[0-9A-Fa-f]+)>/g, '$1');
  if (result.includes('#<')) throw new Error('Unrecognized external-format rendering in comparison plan');
  return result;
}
export function summarizeFlexiWorker(result, expected, index, jobs) {
  const cases = [...result.matchAll(/^FLEXI-CASE (\d+) (.+)$/gm)].map(m => ({index: Number(m[1]), scenario: normalizeFlexiCase(m[2])}));
  const suites = [...result.matchAll(/PORTABILITY-SUITE (flexi-\S+) (passed|failed)$/gm)].map(m => ({name: m[1], status: m[2]}));
  const own = expected.filter(c => c.index % jobs === index).map(c => ({...c, scenario: normalizeFlexiCase(c.scenario)}));
  const marker = result.match(/^FLEXI-SHARD (\d+) (\d+) (\d+) (\d+)$/m);
  const suiteNames = ['flexi-COMPARE-FILES', ...(index ? [] : ['STRING-TESTS', 'SEQUENCE-TESTS', 'ERROR-HANDLING-TESTS', 'UNREAD-CHAR-TESTS', 'COLUMN-TESTS', 'MAKE-EXTERNAL-FORMAT-TESTS', 'PEEK-BYTE-TESTS', 'IN-MEMORY-STREAM-TESTS'].map(n => 'flexi-' + n))];
  const complete = !!marker && marker.slice(1).map(Number).every((v, i) => v === [index, jobs, expected.length, own.length][i]) &&
    JSON.stringify(cases) === JSON.stringify(own) && JSON.stringify(suites.map(s => s.name)) === JSON.stringify(suiteNames) &&
    new RegExp(`^FLEXI-SHARD-COMPLETE ${index}$`, 'm').test(result) && !/Valtan error:/.test(result);
  return {index, cases, suites, complete, status: complete && suites.every(s => s.status === 'passed') ? 'passed' : 'failed'};
}
export function summarizeFlexiLogs(report, readLog) {
  if (!Number.isInteger(report.jobs) || report.jobs < 1 || report.jobs > 64) throw new Error('Invalid worker count');
  const expectedCases = report.expectedCases.map(c => ({...c, scenario: normalizeFlexiCase(c.scenario)}));
  const workers = Array.from({length: report.jobs}, (_, index) => {
    try { return summarizeFlexiWorker(readLog(index), expectedCases, index, report.jobs); }
    catch(error) { return {index, complete: false, status: 'failed', error: String(error)}; }
  });
  const complete = expectedCases.length > 0 && expectedCases.every((c, i) => c.index === i) && workers.every(w => w.complete);
  return {...report, expectedCases, workers, complete,
    comparisons: workers.reduce((n, w) => n + (w.cases?.length ?? 0) * 8, 0),
    status: complete && workers.every(w => w.status === 'passed') ? 'passed' : 'failed'};
}
if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const [reference, logs, output] = process.argv.slice(2);
  if (!reference || !logs || !output || process.argv.length !== 5) throw new Error('Usage: flexi-report.mjs reference.json logs-directory output.json');
  const report = summarizeFlexiLogs(JSON.parse(fs.readFileSync(reference)), i => fs.readFileSync(path.join(logs, `worker-${i}.log`), 'utf8'));
  report.aggregationSha256 = createHash('sha256').update(fs.readFileSync(fileURLToPath(import.meta.url))).digest('hex');
  fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
  console.log(`FLEXI coverage complete=${report.complete}, status=${report.status}, comparisons=${report.comparisons}`);
  if (report.status !== 'passed') process.exitCode = 1;
}
