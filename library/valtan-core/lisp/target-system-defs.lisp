(cl:in-package :valtan-core)

(cl:defun system:make-raw-string ()
  (ffi:new (ffi:ref "String")))
