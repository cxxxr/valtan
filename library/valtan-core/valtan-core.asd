;; -*- mode:lisp -*-

(defsystem "valtan-core"
  :serial t
  :components ((:file "lisp/defpackage" :if-feature :valtan)
               (:file "lisp/valtan-package")
               (:file "lisp/compiler-utils" :if-feature (:not :valtan))
               (:file "lisp/host-system-defs" :if-feature (:not :valtan))
               (:file "lisp/target-system-defs" :if-feature :valtan)
               (:file "lisp/constants")
               (:file "lisp/control")
               (:file "lisp/destructuring-bind")
               (:file "lisp/setf")
               (:file "lisp/ffi")
               (:file "lisp/cons")
               (:file "lisp/condition")
               (:file "lisp/struct")
               (:file "lisp/symbol")
               (:file "lisp/type")
               (:file "lisp/number")
               (:file "lisp/array")
               (:file "lisp/character")
               (:file "lisp/string")
               (:file "lisp/function" :if-feature :valtan)
               (:file "lisp/sequence" :if-feature :valtan)
               (:file "lisp/hashtable" :if-feature :valtan)
               (:file "lisp/package" :if-feature :valtan)
               (:file "lisp/stream" :if-feature :valtan)
               (:file "lisp/print" :if-feature :valtan)
               (:file "lisp/read" :if-feature :valtan)
               (:file "lisp/file" :if-feature :valtan)
               (:file "lisp/pkg" :if-feature :valtan)
               (:file "lisp/clos" :if-feature :valtan)
               (:file "lisp/restart" :if-feature :valtan)
               (:file "lisp/catch-throw" :if-feature :valtan)
               (:file "compiler/variables")
               (:file "compiler/parse-body")
               (:file "compiler/util")
               (:file "compiler/error")
               (:file "compiler/hir")
               (:file "compiler/pass1")
               (:file "compiler/hir-walker" :if-feature (:not :valtan))
               (:file "compiler/type-infer" :if-feature (:not :valtan))
               (:file "compiler/hir-optimize" :if-feature (:not :valtan))
               (:file "compiler/pass2")
               (:file "compiler/compiler")
               (:file "lisp/compilation" :if-feature :valtan)))
