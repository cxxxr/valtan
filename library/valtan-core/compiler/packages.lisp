(defpackage :compiler
  (:use :cl)
  (:export :build-system
           :compile-toplevel))

(defpackage :system
  (:nicknames :*)
  (:use)
  (:export :make-symbol
           :put-symbol-plist
           :symbol-name
           :symbol-package-name
           :fset
           :map-package-symbols
           :put
           :package-name
           :%package-nicknames
           :intern
           :find-symbol
           :make-package
           :%add
           :%sub
           :%negate
           :%mul
           :%rem
           :%=
           :%/=
           :%>
           :%<
           :%>=
           :%<=
           :%floor
           :%logand
           :apply
           :%car
           :%cdr
           :%rplaca
           :%rplacd
           :js-array-to-list
           :list-to-js-array
           :multiple-value-call
           :make-structure
           :%copy-structure
           :structure-p
           :%structure-name
           :%structure-slot-count
           :%structure-ref
           :%structure-set
           :error
           :%code-char
           :%char-code
           :js-array-to-array
           :js-string-to-array
           :array-to-js-string
           :string-append
           :string-append*
           :%defun
           :%defpackage
           :%in-package
           :fdefinition-setf
           :defmacro*
           :named-lambda
           :unquote
           :unquote-splicing
           :quasiquote))

(defpackage :ffi
  (:use)
  (:export #:instanceof
           #:require
           #:export
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

(defpackage :js
  (:use))
