(in-package :common-lisp)

(defvar *features* nil)

(defun compile-toplevel (x)
  (let ((ir (compiler::pass1-toplevel x t)))
    (with-output-to-string (*standard-output*)
      (compiler::pass2-toplevel ir))))

(defun eval (x)
  (ffi:js-eval (compile-toplevel x)))
