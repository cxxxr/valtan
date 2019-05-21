(in-package :common-lisp)

(defun stringp (x)
  (equal (ffi:typeof x) "string"))

(defun string (x)
  (cond ((characterp x)
         ((ffi:ref "String" "fromCharCode") (char-code x)))
        ((symbolp x)
         (symbol-name x))
        (t
         (error "type error"))))
