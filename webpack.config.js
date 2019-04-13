const path = require('path');

module.exports = {
  entry: './js/main.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist')
  },
  resolveLoader: {
    modules: ['node_modules', path.resolve(__dirname, 'cl-loader')]
  },
  module: {
    rules: [
      { test: /\.lisp$/, loader: "cl-loader" }
    ]
  }
};
