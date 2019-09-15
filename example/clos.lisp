(in-package :common-lisp)

(defclass foo ()
  ((x
    :initarg :x
    :accessor foo-x)
   (y)
   z)
  (:default-initargs :x 10))

(print (find-class 'foo))
