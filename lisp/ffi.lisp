(in-package :common-lisp)

(defmacro ffi:define-function (name arguments &body body)
  `(progn
     (ffi:var ,name)
     (ffi:set ,name
              (lambda ,arguments ,@body))))
