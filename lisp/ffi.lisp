(in-package :common-lisp)

(defmacro ffi:define-function (name arguments &body body)
  `(progn
     (ffi:var ,(string name))
     (ffi:set (ffi:ref ,(string name))
              (lambda ,arguments ,@body))))

(defmacro ffi:console.log (&rest args)
  `((ffi:ref "console" "log") ,@args))

(defun ffi::js-eval (x)
  (let* ((code (format nil "(function(lisp) { 'use strict'; ~A; });" x))
         (fn (ffi::%js-eval (system::array-to-js-string code))))
    (funcall fn (ffi:ref "lisp"))))
