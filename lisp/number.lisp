(in-package :common-lisp)

(defun numberp (x)
  (equal (ffi:typeof x) "number"))

(defun integerp (x)
  (eq (ffi:ref "true")
      ((ffi:ref "Number" "isInteger") x)))

(defun plusp (x)
  (< 0 x))

(defun 1+ (x)
  (+ x 1))
