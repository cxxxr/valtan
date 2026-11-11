(defpackage :net.didierverna.asdf-flv
  (:use :cl)
  (:export :set-file-local-variable :set-file-local-variables))
(in-package :net.didierverna.asdf-flv)

(defun make-variable-file-local (symbol)
  (check-type symbol symbol)
  (pushnew symbol system::*file-local-variables*))

(defmacro set-file-local-variable (symbol)
  `(make-variable-file-local ',symbol))

(defun make-variables-file-local (&rest symbols)
  (dolist (symbol symbols) (make-variable-file-local symbol)))

(defmacro set-file-local-variables (&rest symbols)
  `(make-variables-file-local ,@(mapcar (lambda (symbol) `',symbol) symbols)))
