(in-package :common-lisp)

(defclass foo ()
  ((x
    :initarg :x
    :accessor foo-x)
   (y)
   z))

(print (find-class 'foo))
