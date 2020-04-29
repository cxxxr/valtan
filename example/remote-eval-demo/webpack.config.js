const path = require('path');
const fs = require('fs');

module.exports = {
  mode: 'none',
  entry: './.valtan-cache/remote-eval-demo.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist')
  },
  resolve: {
    modules: [
      'node_modules',
      fs.readFileSync('.valtan-path', 'utf-8')
    ]
  }
};
