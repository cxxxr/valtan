(in-package :common-lisp)

(defstruct (array (:copier nil)
                  (:predicate arrayp)
                  (:constructor %make-array))
  data)

(defun make-array (dimensions &key initial-element)
  (unless (integerp dimensions)
    (error "error"))
  (let ((array (ffi:new (ffi:ref "Array") dimensions)))
    ((ffi:ref array "fill") initial-element)
    (%make-array :data array)))
