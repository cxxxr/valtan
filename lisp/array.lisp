(in-package :common-lisp)

(defstruct (array (:constructor %make-array)
                  (:copier nil)
                  (:predicate arrayp))
  contents
  fill-pointer
  rank
  length
  element-type)

(defun dimensions-to-total-size (dimensions)
  (if (integerp dimensions)
      dimensions
      (let ((total-size (first dimensions)))
        (dolist (d (rest dimensions))
          (setq total-size (* total-size d)))
        total-size)))

(defun check-dimensions-and-initial-contents (dimensions initial-contents)
  (let ((initial-contents-length (length initial-contents)))
    (unless (= dimensions
               initial-contents-length)
      (error "error")
      #+(or)
      (error "There are ~D elements in the :INITIAL-CONTENTS, but the vector length is ~D."
             initial-contents-length
             dimensions))))

(defun make-array-contents-with-initial-contents (size element-type initial-contents)
  (cond ((eq element-type 'character)
         (error "trap"))
        (t
         (let ((js-array (ffi:new (ffi:ref "Array") size)))
           (do ((i 0 (1+ i))
                (initial-contents* initial-contents (cdr initial-contents*)))
               ((null initial-contents*))
             (ffi:set (ffi:index js-array i) (car initial-contents*)))
           js-array))))

(defun make-array-contents-with-initial-element (size element-type initial-element initial-element-p)
  (cond ((eq element-type 'character)
         ((ffi:ref ((ffi:ref "String" "fromCharCode")
                    (if initial-element-p
                        (char-code initial-element)
                        0))
                   "repeat")
          size))
        (t
         ((ffi:ref (ffi:new (ffi:ref "Array") size) "fill") initial-element))))

(defun make-array (dimensions &key (element-type t)
                                   (initial-element nil initial-element-p)
                                   (initial-contents nil initial-contents-p)
                                   adjustable
                                   fill-pointer
                                   displaced-to
                                   displaced-index-offset)
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
  (cond ((and initial-contents-p initial-element-p)
         (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
        (initial-contents-p
         (check-dimensions-and-initial-contents dimensions initial-contents)))
  (let ((contents (if initial-contents-p
                      (make-array-contents-with-initial-contents dimensions
                                                                 element-type
                                                                 initial-contents)
                      (make-array-contents-with-initial-element dimensions
                                                                element-type
                                                                initial-element
                                                                initial-element-p))))
    (%make-array :contents contents
                 :fill-pointer fill-pointer
                 :rank (if (listp dimensions)
                           (length dimensions)
                           1)
                 :length (dimensions-to-total-size dimensions)
                 :element-type element-type)))

(defun system::js-array-to-array (js-array)
  (%make-array :contents js-array
               :rank 1
               :length (ffi:ref js-array "length")
               :element-type t))

(defun system::js-string-to-array (js-string)
  (%make-array :contents js-string
               :rank 1
               :length (ffi:ref js-string "length")
               :element-type 'character))

(defun system::array-to-js-string (array)
  (array-contents array))

(defun vector (&rest args)
  (make-array (length args)
              :initial-contents args))

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
  (cond ((eq (array-element-type array) 'character)
         (unless (characterp value)
           (error "type error"))
         (setf (array-contents array)
               ((ffi:ref ((ffi:ref (array-contents array) "substring") 0 sub)
                         "concat")
                value
                ((ffi:ref (array-contents array) "substring") (1+ sub))))
         value)
        (t
         (ffi:set (ffi:index (array-contents array) sub) value))))

(defun vectorp (x)
  (and (arrayp x) (= 1 (array-rank x))))

(defun array-has-fill-pointer-p (array)
  (not (null (array-fill-pointer array))))
