(defpackage :compiler
  (:use :cl)
  (:export :build-system))

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
           #:aget
           #:define-function
           #:define
           #:object
           #:array
           #:console.log
           #:js-eval
           #:cl->js
           #:js->cl
           #:typeof))

(defpackage :valtan-system
  (:use :cl))

(defpackage :js
  (:use))

(defpackage :valtan-user
  (:use :cl))
