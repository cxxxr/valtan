const { execFileSync } = require('child_process');

module.exports = function(source) {
  const output = execFileSync('valtan', [], {input: source});
  return output.toString();
}
