#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defstruct (array (:constructor %make-array)
                  (:copier nil)
                  (:predicate arrayp))
  contents
  dimensions
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
  (if (consp dimensions)
      (let ((total-size (first dimensions)))
        (dolist (d (rest dimensions))
          (setq total-size (* total-size d)))
        total-size)
      dimensions))

(defun check-dimensions-and-initial-contents (dimensions rank initial-contents)
  (case rank
    (0)
    (1 (unless (= dimensions
                  (length initial-contents))
         (error "There are ~D elements in the :INITIAL-CONTENTS, but the vector length is ~D."
                (length initial-contents)
                dimensions)))
    (otherwise
     (labels ((f (dimensions initial-contents)
                (cond ((null dimensions))
                      ((/= (first dimensions) (length initial-contents))
                       (error "There are ~D elements in the :INITIAL-CONTENTS, but the vector length is ~D."
                              (length initial-contents)
                              (first dimensions)))
                      (t
                       (dolist (content initial-contents)
                         (f (cdr dimensions) content))))))
       (f dimensions initial-contents)))))

(defun dimensions-total-size (dimensions)
  (let ((size 1))
    (dotimes (d dimensions size)
      (setq size (* size d)))))

(defun make-array-contents-with-initial-contents (dimensions rank element-type initial-contents)
  (when (and (eq element-type 'character) (= rank 1))
    (let* ((len (length initial-contents))
           (raw-string (system:make-raw-string)))
      (do ((i 0 (1+ i)))
          ((>= i len))
        (setq raw-string
              (system:concat-raw-string/2 raw-string
                                          (system:array-to-raw-string
                                           (string (elt initial-contents i))))))
      (return-from make-array-contents-with-initial-contents raw-string)))
  (let ((i -1)
        (raw-array (system:make-raw-array (dimensions-total-size dimensions))))
    (labels ((f (dimensions initial-contents)
               (cond ((null dimensions))
                     ((null (cdr dimensions))
                      (dolist (content initial-contents)
                        (system:raw-array-set raw-array (incf i) content)))
                     (t
                      (dolist (content initial-contents)
                        (f (cdr dimensions) content))))))
      (f dimensions initial-contents)
      raw-array)))

(defun make-array-contents-with-initial-element (dimensions rank element-type initial-element initial-element-p)
  (when (and (eq element-type 'character) (= rank 1))
    (let ((initial-raw-string
            (system:code-to-raw-string
             (if initial-element-p
                 (char-code initial-element)
                 0)))
          (raw-string (system:make-raw-string)))
      (do ((i 0 (1+ i)))
          ((>= i dimensions))
        (setq raw-string
              (system:concat-raw-string/2 raw-string initial-raw-string)))
      (return-from make-array-contents-with-initial-element raw-string)))
  (when (= rank 1)
    (return-from make-array-contents-with-initial-element
      (system:fill-raw-array (system:make-raw-array dimensions)
                             initial-element)))
  (let* ((total-size (dimensions-total-size dimensions))
         (raw-array (system:make-raw-array total-size)))
    (system:fill-raw-array raw-array initial-element)))

(defun make-array (dimensions &key (element-type t)
                                   (initial-element nil initial-element-p)
                                   (initial-contents nil initial-contents-p)
                                   adjustable
                                   fill-pointer
                                   displaced-to
                                   displaced-index-offset)
  (declare (ignore adjustable displaced-to displaced-index-offset))
  (let (rank)
    (cond ((null dimensions)
           (setq rank 0
                 dimensions 0))
          ((atom dimensions)
           (setq rank 1))
          ((null (cdr dimensions))
           (setq rank 1
                 dimensions (car dimensions)))
          (t
           (setq rank (length dimensions))))
    ;; この時点でdimensionsは整数か長さ2以上のリスト
    (when (and (listp dimensions) fill-pointer)
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
           (check-dimensions-and-initial-contents dimensions rank initial-contents)))
    (setq element-type (upgraded-array-element-type element-type))
    (let ((contents (if initial-contents-p
                        (make-array-contents-with-initial-contents dimensions
                                                                   rank
                                                                   element-type
                                                                   initial-contents)
                        (make-array-contents-with-initial-element dimensions
                                                                  rank
                                                                  element-type
                                                                  initial-element
                                                                  initial-element-p))))
      (%make-array :contents contents
                   :dimensions (case rank
                                 (0 nil)
                                 (1 (list dimensions))
                                 (otherwise dimensions))
                   :fill-pointer fill-pointer
                   :rank rank
                   :length (dimensions-to-total-size dimensions)
                   :element-type element-type))))

(defun *:raw-array-to-array (js-array)
  (%make-array :contents js-array
               :rank 1
               :length (system:raw-array-length js-array)
               :element-type t))

(defun simple-make-string (raw-string)
  (%make-array :contents raw-string
               :rank 1
               :length (system:raw-array-length raw-string)
               :element-type 'character))

(defun *:raw-string-to-array (raw-string)
  (simple-make-string raw-string))

(defun *:array-to-raw-string (array)
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
  (system:raw-array-ref (array-contents array) sub))

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
         (system:raw-array-set (array-contents array) sub value))))

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
  (system:raw-array-ref (array-contents vector) (array-fill-pointer vector)))

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
