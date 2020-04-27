(cl:defpackage :compiler
  (:use :cl)
  (:export :def-implementation
           :compile-toplevel
           :make-emitter-stream
           :join-emitter-stream
           :set-source-map))

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
  (:export :+null+
           :*get-stdin-line-function*
           :write-raw-string-to-stdout
           :read-whole-file
           :make-symbol
           :put-symbol-plist
           :symbol-name
           :symbol-value
           :symbol-function
           :symbol-plist
           :symbol-package-name
           :fset
           :%set
           :map-package-symbols
           :put
           :package-name
           :package-nicknames
           :intern
           :find-symbol
           :make-package
           :export
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
           :raw-array-to-list
           :list-to-raw-array
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
           :number-to-raw-string
           :make-raw-array
           :raw-array-length
           :raw-array-ref
           :raw-array-set
           :fill-raw-array
           :make-map
           :map-get
           :map-set
           :map-length
           :map-clear
           :function-name
           :unknown-object-to-string
           :raw-array-to-array
           :raw-string-to-array
           :array-to-raw-string
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

   :declare :declaim :special :ignore :ignorable :ftype :function

   "CHARACTERP" "EQ" "VALUES" "CONS" "CONSP" "FUNCTIONP" "INTEGERP" "NUMBERP" "LIST-ALL-PACKAGES" "PACKAGEP" "FMAKUNBOUND" "MAKUNBOUND"
   "FBOUNDP" "BOUNDP" "SYMBOLP"))
