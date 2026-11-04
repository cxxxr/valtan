// These alternatives come from conditionals in the pinned, unmodified suite.
// They are registration differences, never exclusions of failing test bodies.
const declarationTests = Array.from({ length: 8 }, (_, i) => `DECLARATION.${i + 4}`);

// This upstream macro constructs test names from (OR (CHAR-NAME c) (STRING c)).
// SBCL names every ASCII character; Valtan names only the standard controls.
// Pair the exact generated names, preserving letter case and test cardinality.
const traitFile = 'reader/set-syntax-from-char.lsp';
const traitPrefix = 'SET-SYNTAX-FROM-CHAR-TRAIT-X-';
const asciiNames = {
  AMPERSAND: '&', APOSTROPHE: "'", ASTERISK: '*', CIRCUMFLEX_ACCENT: '^',
  COMMA: ',', COMMERCIAL_AT: '@', DOLLAR_SIGN: '$', EQUALS_SIGN: '=',
  EXCLAMATION_MARK: '!', GRAVE_ACCENT: '`', 'GREATER-THAN_SIGN': '>',
  'HYPHEN-MINUS': '-', LEFT_CURLY_BRACKET: '{', LEFT_PARENTHESIS: '(',
  LEFT_SQUARE_BRACKET: '[', 'LESS-THAN_SIGN': '<', LOW_LINE: '_', NUMBER_SIGN: '#',
  PERCENT_SIGN: '%', PLUS_SIGN: '+', QUESTION_MARK: '?', QUOTATION_MARK: '"',
  REVERSE_SOLIDUS: '\\', RIGHT_CURLY_BRACKET: '}', RIGHT_PARENTHESIS: ')',
  RIGHT_SQUARE_BRACKET: ']', SEMICOLON: ';', SOLIDUS: '/', TILDE: '~', VERTICAL_LINE: '|',
};
for (const letter of 'ABCDEFGHIJKLMNOPQRSTUVWXYZ') {
  asciiNames[`LATIN_CAPITAL_LETTER_${letter}`] = letter;
  asciiNames[`LATIN_SMALL_LETTER_${letter}`] = letter.toLowerCase();
}

export function auditRegistrations(files, observedNames, inventory, expectedRegistered) {
  const reference = new Set(files.flatMap(file => inventory.testNames[file] ?? []));
  const observed = new Set(observedNames);
  const missingReferenceTests = [...reference].filter(name => !observed.has(name));
  const unexpectedTests = [...observed].filter(name => !reference.has(name));
  const missing = new Set(missingReferenceTests), extra = new Set(unexpectedTests);
  const conditionalRegistrations = [];
  const aliasedRegistrations = [];

  if (files.includes(traitFile)) {
    const fileReference = new Set(inventory.testNames[traitFile] ?? []);
    for (const [unicodeName, character] of Object.entries(asciiNames)) {
      const referenceName = traitPrefix + unicodeName, actualName = traitPrefix + character;
      if (fileReference.has(referenceName) && missing.has(referenceName) && extra.has(actualName)) {
        missing.delete(referenceName);
        extra.delete(actualName);
        aliasedRegistrations.push({ file: traitFile, referenceName, actualName,
          reason: 'The upstream test name uses the implementation-defined CHAR-NAME or the character itself.' });
      }
    }
  }

  if (files.includes('eval-and-compile/declaration.lsp') &&
      declarationTests.every(name => reference.has(name) && missing.has(name))) {
    for (const name of declarationTests) missing.delete(name);
    conditionalRegistrations.push({
      file: 'eval-and-compile/declaration.lsp', tests: declarationTests, delta: -8,
      reason: 'The upstream guard registers these eight tests only when an unknown declaration signals a warning.',
    });
  }
  if (files.includes('objects/defgeneric.lsp') && !reference.has('DEFGENERIC.30') && extra.has('DEFGENERIC.30')) {
    extra.delete('DEFGENERIC.30');
    conditionalRegistrations.push({
      file: 'objects/defgeneric.lsp', tests: ['DEFGENERIC.30'], delta: 1,
      reason: 'The upstream guard registers this test when the metaclass of STANDARD-GENERIC-FUNCTION is a subtype of STANDARD-CLASS.',
    });
  }
  return {
    effectiveExpectedRegistered: expectedRegistered + conditionalRegistrations.reduce((sum, change) => sum + change.delta, 0),
    referenceInventoryMismatch: reference.size !== expectedRegistered,
    missingReferenceTests, unexpectedTests, conditionalRegistrations, aliasedRegistrations,
    registrationNameMismatch: missing.size > 0 || extra.size > 0,
  };
}
