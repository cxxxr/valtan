(in-package :common-lisp)

(defstruct (array (:copier nil)
                  (:predicate arrayp)
                  (:constructor %make-array))
  data
  fill-pointer)

(defun make-array (dimensions &key element-type initial-element initial-contents adjustable
                                   fill-pointer displaced-to displaced-index-offset)
  (unless (integerp dimensions)
    (error "error"))
  (when (and (listp dimensions)
             (cdr dimensions)
             fill-pointer)
    (error "Only vectors can have fill pointers."))
  (unless (or (eq fill-pointer t) (eq fill-pointer nil)
              (and (integerp fill-pointer)
                   (<= 0 fill-pointer)))
    ;(error "Bad fill-pointer: ~S" fill-pointer)
    (error "Bad fill-pointer"))
  (let ((array (ffi:new (ffi:ref "Array") dimensions)))
    ((ffi:ref array "fill") initial-element)
    (%make-array :data array
                 :fill-pointer fill-pointer)))

(defun array-has-fill-pointer-p (array)
  (if (array-fill-pointer array)
      t
      nil))
