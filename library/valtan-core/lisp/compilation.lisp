(in-package :common-lisp)

(defvar *features* '(:valtan))

(defun eval (x)
  (ffi:js-eval (compiler:compile-toplevel x)))

(defun macroexpand-1 (form &optional environment)
  (compiler::!macroexpand-1 form environment))

(defun macroexpand (form &optional environment)
  (declare (ignore environment))
  (multiple-value-bind (form expanded-p)
      (macroexpand-1 form)
    (if expanded-p
        (macroexpand-1 form)
        form)))
