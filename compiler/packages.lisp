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
           :%error
           :make-structure
           :structure-ref
           :structure-set))

(defpackage :ffi
  (:use)
  (:export #:instanceof
           #:require
           #:ref
           #:set
           #:var
           #:new
           #:%aget
           #:define-function
           #:define
           #:object
           #:aget
           #:console.log
           #:js-eval
           #:cl->js
           #:js->cl
           #:typeof))

(defpackage :clscript-system
  (:use))

(defpackage :js
  (:use))
