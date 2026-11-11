import fs from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {createHash} from 'node:crypto';

// Compare completed runs, including assertions hidden inside reader branches.
// Registration totals alone cannot detect those omissions.
export function compareUpstream(target, reference) {
  const errors = [], differences = [], featureVariants = [];
  const require = (condition, message) => {if (!condition) errors.push(message);};
  const checks = target.checks ?? {};
  const array = (value, label) => {
    require(Array.isArray(value), `${label} is missing`);
    return Array.isArray(value) ? value : [];
  };
  const unique = (items, label) => {
    require(items.every(item => typeof item === 'string'), `${label} contains invalid names`);
    require(new Set(items).size === items.length, `${label} contains duplicate names`);
  };
  require(target.suite === 'MCCLIM' && reference.suite === 'MCCLIM', 'Expected the complete MCCLIM suite');
  require(target.status === 'passed' && target.complete === true, 'Target run is not complete and passed');
  require(reference.status === 'passed' && reference.complete === true, 'Reference run is not complete and passed');
  require(checks.status === 'passed', 'Target checks did not pass');
  const targetFeatures = array(checks.features, 'Target features');
  const referenceFeatures = array(reference.features, 'Reference features');
  require(targetFeatures.includes('MCCLIM') && referenceFeatures.includes('MCCLIM'), 'MCCLIM feature is missing');
  for (const [label, run] of [['Target', checks], ['Reference', reference]]) {
    require(run.failed === 0 && run.skipped === 0, `${label} has failed or skipped assertions`);
    require(array(run.unexecuted, `${label} unexecuted tests`).length === 0, `${label} has unexecuted tests`);
    require(array(run.nonpassed, `${label} nonpassed assertions`).length === 0, `${label} has nonpassed assertions`);
    require(Number.isSafeInteger(run.results) && run.results > 0 && run.passed === run.results,
      `${label} assertion totals are inconsistent`);
  }
  const sources = array(target.sources, 'Target sources');
  const baselineSources = array(reference.sourcePlan, 'Reference sources');
  require(sources.length > 0 && sources.length === baselineSources.length, 'Source plan length differs');
  const digest = value => typeof value === 'string' && /^[a-f0-9]{64}$/.test(value);
  require(digest(reference.upstreamDefinitionSha256) &&
    target.upstreamDefinition?.sha256 === reference.upstreamDefinitionSha256, 'ASDF definition hash differs');
  const relative = (file, definition) => typeof file === 'string' && typeof definition === 'string'
    ? path.relative(path.dirname(definition), file) : null;
  const sourceNames = sources.map(row => relative(row.original, target.upstreamDefinition?.file));
  const baselineNames = baselineSources.map(row => relative(row.file, reference.upstreamDefinition));
  unique(sourceNames, 'Target sources'); unique(baselineNames, 'Reference sources');
  for (let i = 0; i < baselineSources.length; i++) {
    require(sourceNames[i] === baselineNames[i], `Source order or identity differs at ${i}`);
    require(digest(baselineSources[i].sha256) && sources[i]?.sha256 === baselineSources[i].sha256,
      `Source hash differs: ${baselineNames[i]}`);
  }
  const files = array(checks.files, 'Target loaded files');
  require(files.length === sources.length, 'Loaded source count differs');
  const targetNames = files.flatMap((row, i) => {
    require(row.error === null, `Source load failed: ${row.file}`);
    require(typeof row.file === 'string' && row.file === sources[i]?.file, `Loaded source differs at ${i}`);
    return array(row.registered, `Registrations for ${row.file}`);
  });
  const referenceNames = array(reference.registeredTests, 'Reference registrations');
  for (const [label, names, run, entries] of [
    ['Target', targetNames, checks, checks['registration-entries']],
    ['Reference', referenceNames, reference, reference.registrationEntries],
  ]) {
    unique(names, `${label} registrations`);
    require(names.length > 0 && names.length === run.registered && names.length === entries,
      `${label} registration totals are inconsistent`);
  }
  const actual = new Set(targetNames), expected = new Set(referenceNames);
  const missing = referenceNames.filter(name => !actual.has(name));
  const unexpected = targetNames.filter(name => !expected.has(name));
  require(missing.length === 0 && unexpected.length === 0, 'Registered test inventory differs');
  const counts = (rows, names, label, total) => {
    const map = new Map(); let sum = 0;
    for (const row of array(rows, `${label} assertion counts`)) {
      require(names.has(row.test), `${label} counts an unregistered test: ${row.test}`);
      require(!map.has(row.test), `${label} repeats assertion count: ${row.test}`);
      require(Number.isSafeInteger(row.count) && row.count > 0, `${label} has an invalid count: ${row.test}`);
      map.set(row.test, row.count); sum += row.count;
    }
    require(sum === total, `${label} per-test counts do not match the total`);
    return map;
  };
  const targetCounts = counts(checks['result-counts'], actual, 'Target', checks.results);
  const referenceCounts = counts(reference.resultCounts, expected, 'Reference', reference.results);
  for (const test of referenceNames) {
    const native = referenceCounts.get(test) ?? 0, valtan = targetCounts.get(test) ?? 0;
    if (native === valtan) continue;
    // These two upstream forms enumerate more dynamic-variable classes without
    // FAKE-PROGV-KLUDGE. Require the exact known counts, not an arbitrary increase.
    if (['CLIM-TESTS::STRESS.9', 'CLIM-TESTS::STRESS.0'].includes(test) &&
        referenceFeatures.includes('FAKE-PROGV-KLUDGE') && !targetFeatures.includes('FAKE-PROGV-KLUDGE') &&
        native === 17 && valtan === 33) {
      featureVariants.push({test, native, valtan, feature: 'FAKE-PROGV-KLUDGE'});
    } else differences.push({test, native, valtan});
  }
  require(differences.length === 0, 'Per-test assertion counts differ');
  return {status: errors.length ? 'failed' : 'passed', complete: errors.length === 0,
    scope: 'Source plan, exact registered test inventory, successful execution and per-test assertion coverage against native MCCLIM',
    sources: sources.length, registered: targetNames.length, results: checks.results,
    missing, unexpected, differences, featureVariants, errors};
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const [targetFile, referenceFile, output] = process.argv.slice(2);
  if (!targetFile || !referenceFile || !output)
    throw Error('Usage: compare-upstream.mjs target.json reference.json comparison.json');
  const targetBytes = fs.readFileSync(targetFile), referenceBytes = fs.readFileSync(referenceFile);
  const hash = bytes => createHash('sha256').update(bytes).digest('hex');
  const report = compareUpstream(JSON.parse(targetBytes), JSON.parse(referenceBytes));
  Object.assign(report, {targetFile: path.resolve(targetFile), referenceFile: path.resolve(referenceFile),
    targetSha256: hash(targetBytes), referenceSha256: hash(referenceBytes)});
  fs.writeFileSync(output, JSON.stringify(report, null, 2) + '\n');
  console.log(JSON.stringify(report));
  if (!report.complete) process.exitCode = 1;
}
