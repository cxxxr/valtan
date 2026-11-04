Enable source maps in the ASDF system definition:

```lisp
(defsystem "system-name"
  :source-map t
  ...)
```

When `valtan build` compiles that system, Valtan writes a `.map` file beside each generated JavaScript
file and adds a `sourceMappingURL` reference. The generated files are consumed by the Vite-based
browser and Node skeletons; a separate `source-map-loader` or webpack configuration is not required.
