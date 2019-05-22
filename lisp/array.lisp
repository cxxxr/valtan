(in-package :common-lisp)

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
    (ffi:set (ffi:ref array "fill-pointer") fill-pointer)
    array))

(defun aref (array sub)
  (unless (arrayp array)
    (error "type error"))
  (when (or (< sub 0) (<= (ffi:ref array "length") sub))
    (error "index error"))
  (ffi:index array sub))

(defun (setf aref) (value array sub)
  (unless (arrayp array)
    (error "type error"))
  (when (or (< sub 0) (<= (ffi:ref array "length") sub))
    (error "index error"))
  (ffi:set (ffi:index array sub) value))

(defun arrayp (x)
  (or (stringp x)
      (eq (ffi:instanceof x (ffi:ref "Array"))
          (ffi:ref "true"))))

(defun array-has-fill-pointer-p (array)
  (and (equal (ffi:typeof array) "object")
       (not (equal (ffi:ref array "fill-pointer")
                   (ffi:ref "undefined")))))
