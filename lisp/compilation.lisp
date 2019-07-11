(in-package :common-lisp)

(defun compile-toplevel (x)
  (let ((ir (compiler::pass1-toplevel x t)))
    (with-output-to-string (*standard-output*)
      (compiler::pass2-toplevel ir))))

(defun eval (x)
  (let* ((code (format nil "(function (lisp) { 'use strict'; ~A; })" (compile-toplevel x)))
         (fn ((ffi:ref "eval") (system::array-to-js-string code))))
    (funcall fn (ffi:ref "lisp"))))
