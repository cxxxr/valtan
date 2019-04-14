const path = require('path');

module.exports = {
  entry: './example/main.lisp',
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
