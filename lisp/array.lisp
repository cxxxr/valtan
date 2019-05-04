(in-package :common-lisp)

(defun make-array (n &key initial-element)
  (let ((vector (ffi:make-object "type" "array")))
    (dotimes (i n)
      (ffi:object-set vector i initial-element))
    vector))
