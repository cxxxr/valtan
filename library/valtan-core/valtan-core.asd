;; -*- mode:lisp -*-

(defsystem "valtan-core/system"
  :serial t
  :components ((:file "lisp/defpackage" :if-feature :valtan)
               (:file "lisp/valtan-package")
               (:file "lisp/compiler-utils" :if-feature (:not :valtan))
               (:file "lisp/host-system-defs" :if-feature (:not :valtan))
               (:file "lisp/target-system-defs" :if-feature :valtan)
               (:file "lisp/host-literal" :if-feature (:not :valtan))))

(defsystem "valtan-core/common-lisp"
  :around-compile (lambda (thunk)
                    (let ((*readtable*
                            (symbol-value
                             (find-symbol "*VALTAN-READTABLE*"
                                          (find-package :valtan-core)))))
                      (funcall thunk)))
  :depends-on ("valtan-core/system")
  :serial t
  :components ((:file "lisp/constants")
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
               (:file "lisp/function")
               (:file "lisp/sequence")
               (:file "lisp/hashtable")
               (:file "lisp/package")
               (:file "lisp/stream")
               (:file "lisp/print")
               (:file "lisp/read")
               (:file "lisp/file" :if-feature :valtan)
               (:file "lisp/pkg" :if-feature :valtan)
               (:file "lisp/clos" :if-feature :valtan)
               (:file "lisp/restart" :if-feature :valtan)
               (:file "lisp/catch-throw" :if-feature :valtan)))

(defsystem "valtan-core/compiler"
  :depends-on ("valtan-core/common-lisp")
  :serial t
  :components ((:file "compiler/variables")
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

(defsystem "valtan-core"
  :depends-on ("valtan-core/system"
               "valtan-core/common-lisp"
               "valtan-core/compiler"))
