const path = require('path');

module.exports = {
  mode: 'none',
  entry: './example/example.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist')
  },
  resolve: {
    modules: [
      'node_modules',
      path.resolve(__dirname, 'lib')
    ]
  },
  resolveLoader: {
    modules: [
      'node_modules',
      path.resolve(__dirname, 'cl-loader')
    ]
  },
  module: {
    rules: [
      { test: /\.lisp$/, loader: "./cl-loader" }
    ]
  }
};
