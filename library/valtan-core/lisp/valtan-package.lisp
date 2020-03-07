(cl:defpackage :compiler
  (:use :cl)
  (:export :compile-toplevel))

;; TODO:
(cl:defvar compiler::*in-host-runtime* nil)

(cl:defpackage :ffi
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

(cl:defpackage :js
  (:use))

(cl:defpackage :system
  (:nicknames :*)
  (:use)
  (:export :*get-stdin-line-function*
           :make-symbol
           :put-symbol-plist
           :symbol-name
           :symbol-value
           :symbol-function
           :symbol-plist
           :symbol-package-name
           :fset
           :map-package-symbols
           :put
           :package-name
           :package-nicknames
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
           :make-raw-string
           :expand-raw-string
           :code-to-raw-string
           :sub-raw-string/2
           :sub-raw-string/3
           :concat-raw-string/2
           :concat-raw-string/3
           :raw-string-upcase
           :raw-string-downcase
           :make-raw-array
           :fill-raw-array
           :make-map
           :map-get
           :map-set
           :map-length
           :map-clear
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

(cl:defpackage :valtan-core
  (:use)
  (:import-from
   :common-lisp
   :t
   :nil
   :&body
   :&rest
   :&key
   :&optional
   #+(or)
   (let ((*print-case* :downcase))
     (pprint (with-open-file (in (asdf:system-relative-pathname :valtan-core "./compiler/pass1.lisp"))
               (loop :with eof := '#:eof
                     :for form := (read in nil eof)
                     :until (eq form eof)
                     :when (and (consp form)
                                (member (first form)
                                        '(def-pass1-form def-transform)
                                        :test #'string-equal)
                                (string= :common-lisp (package-name (symbol-package (second form)))))
                     :collect (intern (string (second form)) :keyword)))))
   :defun :defmacro :define-symbol-macro :lambda :defvar :defparameter :quote :setq :if :progn :function :let :let* :flet :labels :macrolet
   :symbol-macrolet :unwind-protect :block :return-from :tagbody :go :locally :declaim :eval-when :in-package

   :declare :declaim :ignore :ignorable :ftype :function

   "CHARACTERP" "EQ" "VALUES" "CONS" "CONSP" "FUNCTIONP" "INTEGERP" "NUMBERP" "LIST-ALL-PACKAGES" "PACKAGEP" "FMAKUNBOUND" "MAKUNBOUND" "SET"
   "FBOUNDP" "BOUNDP" "SYMBOLP"))
