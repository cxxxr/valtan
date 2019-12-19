(in-package :common-lisp)

(defvar *features* '(:valtan))

(defun compile-toplevel (x)
  (let ((ir (compiler::pass1-toplevel x t)))
    (with-output-to-string (*standard-output*)
      (compiler::p2-toplevel ir))))

(defun eval (x)
  (ffi:js-eval (compile-toplevel x)))

(defun macroexpand-1 (x)
  (let ((compiler::*lexenv* nil))
    (compiler::%macroexpand-1 x)))

(defun macroexpand (x)
  (multiple-value-bind (form expanded-p)
      (macroexpand-1 x)
    (if expanded-p
        (macroexpand-1 form)
        form)))
