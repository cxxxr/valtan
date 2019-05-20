(in-package :common-lisp)

(defun stringp (x)
  (equal (ffi:typeof x) "string"))

(defun string (x)
  (cond ((characterp x)
         ((ffi:ref "String" "fromCharCode") (ffi:ref x "code")))
        ((symbolp x)
         (symbol-name x))
        (t
         (error "type error"))))

