const path = require('path');

module.exports = {
  mode: 'none',
  // target: 'node',
  entry: './react-counter.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist')
  },
  resolve: {
    modules: [
      'node_modules',
      path.resolve(__dirname, 'lib')
    ]
  }
};
