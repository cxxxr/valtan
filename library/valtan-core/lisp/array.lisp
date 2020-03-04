#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defstruct (array (:constructor %make-array)
                  (:copier nil)
                  (:predicate arrayp))
  contents
  fill-pointer
  rank
  length
  element-type)

(defun upgraded-array-element-type (typespec)
  (cond ((symbolp typespec)
         (case typespec
           ((character base-char standard-char extended-char)
            'character)
           (otherwise
            t)))
        (t t)))

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
      (error "There are ~D elements in the :INITIAL-CONTENTS, but the vector length is ~D."
             initial-contents-length
             dimensions))))

(declaim (ftype function map elt))

(defun make-array-contents-with-initial-contents (size element-type initial-contents)
  (cond ((eq element-type 'character)
         (let* ((len (length initial-contents))
                (raw-string (system:make-raw-string)))
           (do ((i 0 (1+ i)))
               ((>= i len))
             (setq raw-string
                   (system:concat-raw-string/2 raw-string
                                               (system:array-to-js-string
                                                (string (elt initial-contents i))))))
           raw-string))
        (t
         (let ((raw-array (system:make-raw-array size))
               (i 0))
           (map nil
                (lambda (content)
                  (ffi:set (ffi:aget raw-array i) content)
                  (incf i))
                initial-contents)
           raw-array))))

(declaim (ftype function char-code))

(defun make-array-contents-with-initial-element (size element-type initial-element initial-element-p)
  (cond ((eq element-type 'character)
         (system:expand-raw-string (system:code-to-raw-string
                                    (if initial-element-p
                                        (char-code initial-element)
                                        0))
                                   size))
        (t
         (system:fill-raw-array (system:make-raw-array size)
                                initial-element))))

(defun make-array (dimensions &key (element-type t)
                                   (initial-element nil initial-element-p)
                                   (initial-contents nil initial-contents-p)
                                   adjustable
                                   fill-pointer
                                   displaced-to
                                   displaced-index-offset)
  (declare (ignore adjustable displaced-to displaced-index-offset))
  (when (and (consp dimensions) (cdr dimensions))
    (error "error"))
  (when (listp dimensions)
    (setq dimensions (car dimensions)))
  (unless (integerp dimensions)
    (error "error"))
  (when (and (listp dimensions)
             (cdr dimensions)
             fill-pointer)
    (error "Only vectors can have fill pointers."))
  (unless (or (eq fill-pointer t) (eq fill-pointer nil)
              (and (integerp fill-pointer)
                   (<= 0 fill-pointer)))
    (error "Bad fill-pointer: ~S" fill-pointer))
  (when (eq fill-pointer t)
    (assert (integerp dimensions))
    (setq fill-pointer dimensions))
  (cond ((and initial-contents-p initial-element-p)
         (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
        (initial-contents-p
         (check-dimensions-and-initial-contents dimensions initial-contents)))
  (setq element-type (upgraded-array-element-type element-type))
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

(defun *:js-array-to-array (js-array)
  (%make-array :contents js-array
               :rank 1
               :length (ffi:ref js-array "length")
               :element-type t))

(defun simple-make-string (js-string)
  (%make-array :contents js-string
               :rank 1
               :length (ffi:ref js-string "length")
               :element-type 'character))

(defun *:js-string-to-array (js-string)
  (simple-make-string js-string))

(defun *:array-to-js-string (array)
  (if (array-fill-pointer array)
      (system:sub-raw-string/3 (array-contents array) 0 (array-fill-pointer array))
      (array-contents array)))

(defun vector (&rest args)
  (make-array (length args)
              :initial-contents args))

(defun fill-pointer (array)
  (array-fill-pointer array))

(defun (cl:setf fill-pointer) (fill-pointer array)
  (setf (array-fill-pointer array) fill-pointer))

(defun array-dimension (array axis-number)
  (assert (zerop axis-number))
  (array-length array))

(defun aref (array sub)
  (unless (arrayp array)
    (type-error array 'array))
  (when (or (< sub 0) (<= (array-length array) sub))
    (error "index error"))
  (ffi:aget (array-contents array) sub))

(defun (cl:setf aref) (value array sub)
  (unless (arrayp array)
    (type-error array 'array))
  (when (or (< sub 0) (<= (array-length array) sub))
    (error "index error"))
  (cond ((eq (array-element-type array) 'character)
         (unless (characterp value)
           (type-error value 'character))
         (setf (array-contents array)
               (system:concat-raw-string/3 (system:sub-raw-string/3 (array-contents array) 0 sub)
                                           value
                                           (system:sub-raw-string/2 (array-contents array) (1+ sub))))
         value)
        (t
         (ffi:set (ffi:aget (array-contents array) sub) value))))

(defun svref (vector index)
  (aref vector index))

(defun (cl:setf svref) (value vector index)
  (setf (aref vector index) value))

(defun vectorp (x)
  (and (arrayp x) (= 1 (array-rank x))))

(defun simple-vector-p (x)
  (and (arrayp x)
       (= 1 (array-rank x))
       (not (array-has-fill-pointer-p x))))

(defun simple-bit-vector-p (x)
  (simple-vector-p x))

(defun array-has-fill-pointer-p (array)
  (not (null (array-fill-pointer array))))

(defun array-length-with-fill-pointer (array)
  (or (array-fill-pointer array)
      (array-length array)))

(defun array-total-size (array)
  (ffi:ref (array-contents array) "length"))

(defun vector-pop (vector)
  (when (or (null (array-fill-pointer vector))
            (>= 0 (array-fill-pointer vector)))
    (error "error"))
  (decf (array-fill-pointer vector))
  (ffi:aget (array-contents vector) (array-fill-pointer vector)))

(defun vector-push (new-element vector)
  (when (or (null (array-fill-pointer vector))
            (>= 0 (array-fill-pointer vector)))
    (error "error"))
  (let ((i (array-fill-pointer vector)))
    (when (>= i (array-total-size vector))
      (error "error"))
    (incf (array-fill-pointer vector))
    (setf (aref vector i) new-element)
    i))
