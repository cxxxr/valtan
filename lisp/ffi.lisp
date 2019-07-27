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

(defun ffi::parse-float (string)
  ((ffi:ref "parseFloat") (system::array-to-js-string string)))

(defun ffi::lisp-to-js-value (value)
  (cond ((stringp value)
         (system::array-to-js-string value))
        ((eq value t)
         (ffi::%ref "true"))
        ((eq value nil)
         (ffi::%ref "false"))
        ((functionp value)
         (lambda (&rest args)
           (apply value (mapcar #'ffi::lisp-to-js-value args))))
        (t
         value)))

(defun ffi::js-to-lisp-value (value)
  (cond ((eq (ffi:typeof value)
             (system::array-to-js-string "string"))
         (system::js-string-to-array value))
        ((eq value (ffi::%ref "true"))
         t)
        ((eq value (ffi::%ref "false"))
         nil)
        (t
         value)))
