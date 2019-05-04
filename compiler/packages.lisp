(defpackage :compiler
  (:use :cl)
  (:export :compile-stdin))

(defpackage :system
  (:use)
  (:export :unquote
           :unquote-splicing
           :quasiquote
           :fset
           :add-global-macro
           :add-symbol-macro
           :%error))

(defpackage :ffi
  (:use)
  (:export :console.log
           :make-object
           :object-get
           :object-set
           :ref
           :set
           :instanceof))

(defpackage :clscript-system
  (:use))
