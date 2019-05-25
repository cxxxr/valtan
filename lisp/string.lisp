(in-package :common-lisp)

(defun string (x)
  (cond ((characterp x)
         ((ffi:ref "String" "fromCharCode") (char-code x)))
        ((symbolp x)
         (symbol-name x))
        (t
         (error "type error"))))

(defun string= (x y)
  (equal (string x)
         (string y)))
