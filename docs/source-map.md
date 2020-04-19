## install source-map-loader
```
npm i source-map-loader
```

## defsystem
```
(defsystem "system-name"
  :source-map t
  ...)  
```

## webpack.config.js
```
module.exports = {
  ...
  devtool: 'inline-source-map',
  module: {
    rules: [
      {
        test: /\.js$/,
        use: ["source-map-loader"],
        enforce: "pre"
      }
    ]
  }
}
```
