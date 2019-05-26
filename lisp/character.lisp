(in-package :common-lisp)

(defstruct (character (:predicate characterp)
                      (:copier nil)
                      (:conc-name char-))
  (code 0 :read-only t))

(defun code-char (code)
  (unless (and (integerp code) (<= 0 code))
    (error "type error"))
  (make-character :code code))
