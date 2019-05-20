(in-package :common-lisp)

(defstruct (array (:copier nil)
                  (:predicate arrayp)
                  (:constructor %make-array))
  data)

(defun make-array (dimensions &key element-type initial-element initial-contents adjustable
                                   fill-pointer displaced-to displaced-index-offset)
  (unless (integerp dimensions)
    (error "error"))
  (let ((array (ffi:new (ffi:ref "Array") dimensions)))
    ((ffi:ref array "fill") initial-element)
    (%make-array :data array)))
