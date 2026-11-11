import assert from 'node:assert/strict';

// Read only the version-1 forms emitted by COMPILE-FILE. This is deliberately
// not a general Lisp reader: unexpected forms and truncated output must fail.
export function readForms(artifact) {
  const header = '; Valtan compiled JavaScript, version 1\n';
  assert.ok(artifact.startsWith(header), 'Unsupported compiled artifact header');
  let position = header.length;
  const forms = [];
  while (position < artifact.length) {
    while (position < artifact.length && /\s/.test(artifact[position])) position++;
    if (position === artifact.length) break;
    const call = artifact.startsWith('(FUNCALL ', position);
    if (call) position += 9;
    const prefix = '(FFI:JS-EVAL "';
    assert.ok(artifact.startsWith(prefix, position), `Unexpected artifact form at ${position}`);
    position += prefix.length;
    let code = '', closed = false;
    while (position < artifact.length) {
      let character = artifact[position++];
      if (character === '"') { closed = true; break; }
      if (character === '\\') {
        assert.ok(position < artifact.length, 'Truncated string escape');
        character = artifact[position++];
      }
      code += character;
    }
    assert.ok(closed, 'Unterminated code string');
    const suffix = call ? '))' : ')';
    assert.equal(artifact.slice(position, position + suffix.length), suffix, 'Malformed artifact wrapper');
    position += suffix.length;
    forms.push({code, call});
  }
  return forms;
}
