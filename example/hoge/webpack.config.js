const path = require('path');
const fs = require('fs');

module.exports = {
  mode: 'none',
  // target: 'node',
  entry: './hoge.js',
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