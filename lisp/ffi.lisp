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
         (fn (js:eval code)))
    (funcall fn (ffi:ref "lisp"))))

(defun ffi::cl->js (value)
  (cond ((stringp value)
         (system::array-to-js-string value))
        ((eq value t)
         (ffi::ref "true"))
        ((eq value nil)
         (ffi::ref "false"))
        ((functionp value)
         (lambda (&rest args)
           (apply value (mapcar #'ffi::cl->js args))))
        (t
         value)))

(defun ffi::js->cl (value)
  (cond ((eq (ffi:typeof value)
             (system::array-to-js-string "string"))
         (system::js-string-to-array value))
        ((eq value (ffi::ref "true"))
         t)
        ((eq value (ffi::ref "false"))
         nil)
        (t
         value)))
