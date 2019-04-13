const { execSync } = require('child_process');

module.exports = function(source) {
  const output = execFileSync('clscript', [], {input: source});
  return output.toString();
}
