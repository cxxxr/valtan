import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';

// These upstream tests use the implementation's Gray protocol symbols as
// their names. SBCL prints the inherited SB-GRAY symbols without a prefix;
// Valtan prints the corresponding exported VALTAN-GRAY symbols explicitly.
const grayTests = new Set(['STREAM-READ-CHAR', 'STREAM-UNREAD-CHAR', 'STREAM-READ-CHAR-NO-HANG',
  'STREAM-PEEK-CHAR', 'STREAM-LISTEN', 'STREAM-READ-LINE', 'STREAM-WRITE-CHAR',
  'STREAM-LINE-COLUMN', 'STREAM-START-LINE-P', 'STREAM-WRITE-STRING', 'STREAM-TERPRI',
  'STREAM-FRESH-LINE', 'STREAM-ADVANCE-TO-COLUMN']);

// Native failures are retained as reference evidence, never used to excuse a
// target failure. Both suites, self-compilation and every registration count.
export function compareDrei(target, reference) {
  const errors = [], differences = [], implementationVariants = [];
  const require = (condition, message) => {if (!condition) errors.push(message);};
  const array = (value, label) => {
    require(Array.isArray(value), `${label} is missing`);
    return Array.isArray(value) ? value : [];
  };
  const unique = (values, label) => {
    require(values.every(value => typeof value === 'string'), `${label} contains an invalid name`);
    require(new Set(values).size === values.length, `${label} contains duplicates`);
  };
  const checks = target.checks ?? {};
  require(target.status === 'passed' && target.complete === true && checks.status === 'passed',
    'Target run is not complete and passed');
  require(reference.complete === true && ['passed', 'failed'].includes(reference.status),
    'Native reference is not complete');
  for (const [label, run] of [['Target', target], ['Native', reference]])
    require(run.suite === 'DREI-ALL' && run.selfCompilation === true,
      `${label} did not enable both Drei suites and self-compilation`);
  const targetFeatures = array(checks.features, 'Target features');
  const nativeFeatures = array(reference.features, 'Native features');
  const knownImplementations = targetFeatures.includes('VALTAN') && nativeFeatures.includes('SBCL');
  const canonical = (name, side) => {
    if (!knownImplementations || typeof name !== 'string') return name;
    const protocolName = side === 'Native' ? name : name.startsWith('VALTAN-GRAY:') ? name.slice(12) : null;
    return grayTests.has(protocolName) ? `GRAY-STREAMS::${protocolName}` : name;
  };
  require(targetFeatures.includes('MCCLIM') && targetFeatures.includes('VALTAN'), 'Target features are incomplete');
  const nativeFailures = array(reference.nonpassed, 'Native nonpassed assertions');
  for (const [label, run] of [['Target', checks], ['Native', reference]]) {
    require(run.skipped === 0, `${label} has skipped assertions`);
    require(array(run.unexecuted, `${label} unexecuted tests`).length === 0, `${label} has unexecuted tests`);
    require(Number.isSafeInteger(run.results) && run.results > 0 &&
      Number.isSafeInteger(run.passed) && run.passed >= 0 &&
      Number.isSafeInteger(run.failed) && run.failed >= 0 &&
      run.passed + run.failed === run.results, `${label} assertion totals are inconsistent`);
  }
  require(checks.failed === 0 && checks.passed === checks.results, 'Target has failed assertions');
  require(array(checks.nonpassed, 'Target nonpassed assertions').length === 0, 'Target has nonpassed assertions');
  require(nativeFailures.length === reference.failed, 'Native failure details do not match the total');
  require((reference.failed === 0) === (reference.status === 'passed'), 'Native status disagrees with its failures');

  const digest = value => typeof value === 'string' && /^[a-f0-9]{64}$/.test(value);
  require(digest(reference.upstreamDefinitionSha256) &&
    target.upstreamDefinition?.sha256 === reference.upstreamDefinitionSha256, 'ASDF definition hash differs');
  const relative = (file, definition) => typeof file === 'string' && typeof definition === 'string'
    ? path.relative(path.dirname(definition), file) : null;
  function compareSources(actual, expected, actualFile, label) {
    const left = array(actual, `Target ${label}`), right = array(expected, `Native ${label}`);
    require(right.length > 0 && left.length === right.length, `${label} length differs`);
    const leftNames = left.map(row => relative(row[actualFile], target.upstreamDefinition?.file));
    const rightNames = right.map(row => relative(row.file, reference.upstreamDefinition));
    unique(leftNames, `Target ${label}`); unique(rightNames, `Native ${label}`);
    for (let i = 0; i < right.length; i++) {
      require(leftNames[i] === rightNames[i], `${label} identity or order differs at ${i}`);
      require(digest(right[i].sha256) && left[i]?.sha256 === right[i].sha256,
        `${label} hash differs: ${rightNames[i]}`);
    }
    return left;
  }
  const sources = compareSources(target.sources, reference.sourcePlan, 'original', 'test sources');
  const resources = compareSources(target.sourceResources, reference.sourceResources, 'file', 'self-compilation sources');
  const files = array(checks.files, 'Target loaded files');
  require(files.length === sources.length, 'Loaded source count differs');
  const registrations = files.flatMap((row, i) => {
    require(row.error === null, `Source load failed: ${row.file}`);
    require(typeof row.file === 'string' && row.file === sources[i]?.file, `Loaded source differs at ${i}`);
    return array(row.registered, `Registrations for ${row.file}`);
  });
  require(registrations.every(name => typeof name === 'string'), 'Target contains an invalid registration');
  // Drei re-registers some names while loading. Compare the total separately
  // from the unique inventory; rejecting all duplicates would reject upstream.
  const actual = new Set(registrations.map(name => canonical(name, 'Target')));
  const nativeNames = array(reference.registeredTests, 'Native registrations');
  const names = nativeNames.map(name => canonical(name, 'Native'));
  unique(names, 'Native registrations');
  const expected = new Set(names);
  require(nativeFailures.every(row => expected.has(canonical(row.test, 'Native'))),
    'Native failure details reference an unregistered test');
  require(registrations.length === checks.registrationEntries &&
    checks.registrationEntries === reference.registrationEntries, 'Registration entry totals differ');
  require(Number.isSafeInteger(reference.registrationEntries) && reference.registrationEntries >= names.length,
    'Native registration entry total is invalid');
  require(names.length > 0 && names.length === reference.registered &&
    actual.size === checks.registered && actual.size === names.length, 'Unique registration totals differ');
  const missing = names.filter(name => !actual.has(name));
  const unexpected = [...actual].filter(name => !expected.has(name));
  require(missing.length === 0 && unexpected.length === 0, 'Registered test inventory differs');

  function counts(rows, registered, label, total) {
    const map = new Map(); let sum = 0;
    for (const row of array(rows, `${label} assertion counts`)) {
      const name = canonical(row.test, label);
      require(registered.has(name), `${label} counts an unregistered test: ${row.test}`);
      require(!map.has(name), `${label} repeats assertion count: ${row.test}`);
      require(Number.isSafeInteger(row.count) && row.count > 0, `${label} has an invalid count: ${row.test}`);
      map.set(name, row.count); sum += row.count;
    }
    require(sum === total, `${label} per-test counts do not match the total`);
    return map;
  }
  const targetCounts = counts(checks.resultCounts, actual, 'Target', checks.results);
  const nativeCounts = counts(reference.resultCounts, expected, 'Native', reference.results);
  for (const test of names) {
    const native = nativeCounts.get(test) ?? 0, valtan = targetCounts.get(test) ?? 0;
    if (native === valtan) continue;
    // This test iterates LAMBDA-LIST-KEYWORDS, then makes one negative check.
    // SBCL includes SB-INT:&MORE; Valtan lists the eight standard keywords.
    if (test === 'DREI-TESTS::ARGLIST-KEYWORD-P' && native === 10 && valtan === 9 &&
        nativeFeatures.includes('SBCL') && targetFeatures.includes('VALTAN'))
      implementationVariants.push({test, native, valtan, reason: 'SBCL has the additional SB-INT:&MORE lambda-list keyword'});
    else differences.push({test, native, valtan});
  }
  require(differences.length === 0, 'Per-test assertion counts differ');
  const identityMappings = knownImplementations ? [...grayTests]
    .filter(name => nativeNames.includes(name) && registrations.includes(`VALTAN-GRAY:${name}`))
    .map(name => ({canonical: `GRAY-STREAMS::${name}`, native: name, valtan: `VALTAN-GRAY:${name}`})) : [];
  return {status: errors.length ? 'failed' : 'passed', complete: errors.length === 0,
    scope: 'Complete target Drei execution, exact unique registrations and per-test assertion coverage against native inventory; native failures are retained separately',
    sources: sources.length, selfCompilationSources: resources.length,
    registered: actual.size, registrationEntries: registrations.length, results: checks.results,
    missing, unexpected, differences, identityMappings, implementationVariants, nativeStatus: reference.status,
    nativeFailures, errors};
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const [targetFile, referenceFile, output] = process.argv.slice(2);
  if (!targetFile || !referenceFile || !output)
    throw Error('Usage: compare-drei.mjs target.json native.json comparison.json');
  const targetBytes = fs.readFileSync(targetFile), referenceBytes = fs.readFileSync(referenceFile);
  const hash = bytes => createHash('sha256').update(bytes).digest('hex');
  const report = compareDrei(JSON.parse(targetBytes), JSON.parse(referenceBytes));
  Object.assign(report, {targetFile: path.resolve(targetFile), referenceFile: path.resolve(referenceFile),
    targetSha256: hash(targetBytes), referenceSha256: hash(referenceBytes)});
  fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
  console.log(JSON.stringify({status: report.status, complete: report.complete,
    registered: report.registered, results: report.results,
    nativeFailures: report.nativeFailures.length, errors: report.errors, output}));
  if (!report.complete) process.exitCode = 1;
}
