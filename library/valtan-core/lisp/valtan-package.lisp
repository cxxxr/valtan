#-valtan
(progn
  (defpackage :valtan-core (:use))
  (defpackage :system
    (:nicknames :*)
    (:use)
    (:export :%make-symbol
             :put-symbol-plist
             :%symbol-name
             :symbol-package-name
             :fset
             :map-package-symbols
             :%put
             :%package-name
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
             :quasiquote)))

#+valtan
(*:%defpackage :valtan-core)
#+valtan
(*:%defpackage "COMPILER")

(cl:eval-when (:compile-toplevel)
  (cl:defpackage :valtan-core
    (:use)
    #-valtan
    (:import-from
     :common-lisp
     :&body
     :&rest
     :&optional
     . #.(with-open-file (in (asdf:system-relative-pathname :valtan "library/valtan-core/compiler/pass1.lisp"))
           (loop :with eof := '#:eof
                 :for form := (read in nil eof)
                 :until (eq form eof)
                 :when (and (consp form)
                            (member (first form)
                                    '(def-pass1-form def-transform)
                                    :test #'string-equal)
                            (string= :common-lisp (package-name (symbol-package (second form)))))
                 :collect (string (second form)))))))
