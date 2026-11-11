// Coverage and outcomes are separate: a fully executed failing suite is complete.
export function summarizeUpstream(result, expectedBabelTests, suite = 'all') {
  const babelTests = [...result.matchAll(/^BABEL-TEST (.+)$/gm)].map(m => m[1]);
  const babelFailures = [...result.matchAll(/^BABEL-FAIL ([^:]+):/gm)].map(m => m[1]);
  const babel = result.match(/^PORTABILITY-SUITE babel (passed|failed) (\d+) (\d+)$/m);
  const flexiSuites = [...result.matchAll(/PORTABILITY-SUITE (flexi-\S+) (passed|failed)$/gm)]
    .map(m => ({ name: m[1], status: m[2] }));
  const expectedFlexi = ['COMPARE-FILES', 'STRING-TESTS', 'SEQUENCE-TESTS', 'ERROR-HANDLING-TESTS',
    'UNREAD-CHAR-TESTS', 'COLUMN-TESTS', 'MAKE-EXTERNAL-FORMAT-TESTS', 'PEEK-BYTE-TESTS', 'IN-MEMORY-STREAM-TESTS'];
  const same = (a, b) => a.length === b.length && a.every((x, i) => x === b[i]);
  const complete = (suite === 'flexi' || !!babel && same(babelTests, expectedBabelTests) && Number(babel[2]) === babelTests.length) &&
    (suite === 'babel' || same(flexiSuites.map(s => s.name.replace(/^flexi-/, '')), expectedFlexi)) &&
    /^PORTABILITY-UPSTREAM-(COMPLETE|PASS)$/m.test(result) && !/Valtan error:/.test(result);
  return { complete, status: complete && (suite === 'flexi' || babel[1] === 'passed') && !babelFailures.length &&
    flexiSuites.every(s => s.status === 'passed') ? 'passed' : 'failed',
    expectedBabelTests, babelTests, babelFailures, babelAssertions: babel ? Number(babel[3]) : 0, flexiSuites };
}
