(in-package :common-lisp)

(defstruct (array (:constructor %make-array)
                  (:copier nil)
                  (:predicate arrayp))
  contents
  fill-pointer
  rank
  length)

(defun dimensions-to-total-size (dimensions)
  (if (integerp dimensions)
      dimensions
      (let ((total-size (first dimensions)))
        (dolist (d (rest dimensions))
          (setq total-size (* total-size d)))
        total-size)))

(defun make-array (dimensions &key element-type initial-element initial-contents adjustable
                                   fill-pointer displaced-to displaced-index-offset)
  (unless (integerp dimensions)
    (error "error"))
  (when (and (listp dimensions)
             (cdr dimensions)
             fill-pointer)
    (error "Only vectors can have fill pointers."))
  ;; TODO element-typeがcharacterの場合にstringに変換
  (unless (or (eq fill-pointer t) (eq fill-pointer nil)
              (and (integerp fill-pointer)
                   (<= 0 fill-pointer)))
    ;(error "Bad fill-pointer: ~S" fill-pointer)
    (error "Bad fill-pointer"))
  (let ((contents (ffi:new (ffi:ref "Array") dimensions)))
    ((ffi:ref contents "fill") initial-element)
    (%make-array :contents contents
                 :fill-pointer fill-pointer
                 :rank (if (listp dimensions)
                           (length dimensions)
                           1)
                 :length (dimensions-to-total-size dimensions))))

(defun aref (array sub)
  (unless (arrayp array)
    (error "type error"))
  (when (or (< sub 0) (<= (array-length array) sub))
    (error "index error"))
  (ffi:index (array-contents array) sub))

(defun (setf aref) (value array sub)
  (unless (arrayp array)
    (error "type error"))
  (when (or (< sub 0) (<= (array-length array) sub))
    (error "index error"))
  (ffi:set (ffi:index (array-contents array) sub) value))

(defun vectorp (x)
  (and (arrayp x) (= 1 (array-rank x))))

(defun array-has-fill-pointer-p (array)
  (not (null (array-fill-pointer array))))
