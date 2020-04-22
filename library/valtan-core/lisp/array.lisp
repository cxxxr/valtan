#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defstruct (array (:constructor %make-array)
                  (:copier nil)
                  (:predicate arrayp))
  contents
  dimensions
  (total-size 0 :read-only t)
  fill-pointer
  rank
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
    (1 (unless (= (car dimensions)
                  (length initial-contents))
         (error "There are ~D elements in the :INITIAL-CONTENTS, but the vector length is ~D."
                (length initial-contents)
                (car dimensions))))
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
    (dolist (d dimensions size)
      (setq size (* size d)))))

(defun make-array-contents-with-initial-contents (dimensions rank element-type initial-contents)
  (cond ((and (eq element-type 'character) (= rank 1))
         (let* ((len (length initial-contents))
                (raw-string (system:make-raw-string)))
           (do ((i 0 (1+ i)))
               ((>= i len))
             (setq raw-string
                   (system:concat-raw-string/2 raw-string
                                               (system:array-to-raw-string
                                                (string (elt initial-contents i))))))
           raw-string))
        ((= rank 0)
         (let ((raw-array (system:make-raw-array 1)))
           (system:raw-array-set raw-array 0 initial-contents)
           raw-array))
        (t
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
             raw-array)))))

(defun make-array-contents-with-initial-element (dimensions rank element-type initial-element initial-element-p)
  (cond ((and (eq element-type 'character) (= rank 1))
         (let ((initial-raw-string
                 (system:code-to-raw-string
                  (if initial-element-p
                      (char-code initial-element)
                      0)))
               (raw-string (system:make-raw-string))
               (length (car dimensions)))
           (do ((i 0 (1+ i)))
               ((>= i length))
             (setq raw-string
                   (system:concat-raw-string/2 raw-string initial-raw-string)))
           raw-string))
        ((= rank 1)
         (system:fill-raw-array (system:make-raw-array dimensions)
                                initial-element))
        ((= rank 0)
         (let ((raw-array (system:make-raw-array 1)))
           (system:raw-array-set raw-array 0 initial-element)
           raw-array))
        (t
         (let* ((total-size (dimensions-total-size dimensions))
                (raw-array (system:make-raw-array total-size)))
           (system:fill-raw-array raw-array initial-element)))))

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
           (setq rank 0))
          ((atom dimensions)
           (setq rank 1
                 dimensions (list dimensions)))
          ((null (cdr dimensions))
           (setq rank 1))
          (t
           (setq rank (length dimensions))))
    (when (and (/= rank 1) fill-pointer)
      (error "Only vectors can have fill pointers."))
    (unless (or (eq fill-pointer t) (eq fill-pointer nil)
                (and (integerp fill-pointer)
                     (<= 0 fill-pointer)))
      (error "Bad fill-pointer: ~S" fill-pointer))
    (when (eq fill-pointer t)
      (assert (= rank 1))
      (setq fill-pointer (car dimensions)))
    (cond ((and initial-contents-p initial-element-p)
           (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
          (initial-contents-p
           (check-dimensions-and-initial-contents dimensions rank initial-contents)))
    (setq element-type (upgraded-array-element-type element-type))
    (let* ((total-size (dimensions-total-size dimensions))
           (contents (if initial-contents-p
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
                   :dimensions dimensions
                   :total-size total-size
                   :fill-pointer fill-pointer
                   :rank rank
                   :element-type element-type))))

(defun *:raw-array-to-array (js-array)
  (let ((length (system:raw-array-length js-array)))
    (%make-array :contents js-array
                 :dimensions (list length)
                 :total-size length
                 :rank 1
                 :element-type t)))

(defun simple-make-string (raw-string)
  (let ((length (system:raw-array-length raw-string)))
    (%make-array :contents raw-string
                 :dimensions (list length)
                 :total-size length
                 :rank 1
                 :element-type 'character)))

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
  (nth axis-number (array-dimensions array)))

(defun subscripts-error (n-subscripts rank)
  (error "Wrong number of subscripts, ~D, for array of rank ~D." n-subscripts rank))

(defun array-row-major-index (array subscripts)
  (assert (listp subscripts))
  (unless (arrayp array)
    (type-error array 'array))
  (let ((rank (array-rank array))
        (dimensions (array-dimensions array)))
    (unless (= rank (length subscripts))
      (subscripts-error (length subscripts) (array-rank array)))
    (do ((axis (1- rank) (1- axis))
         (chunk-size 1)
         (result 0))
        ((minusp axis) result)
      (let ((index (nth axis subscripts))
            (dim (nth axis dimensions)))
        (when (or (< index 0) (<= dim index))
          (error "Invalid index ~D for axis ~D of ~S, should be a non-negative integer below ~D."
                 index axis array index))
        (incf result (* chunk-size index))
        (setq chunk-size (* chunk-size dim))))))

(defun aref (array &rest subscripts)
  (system:raw-array-ref (array-contents array) (array-row-major-index array subscripts)))

(defun %string-set (array index value)
  (unless (characterp value)
    (type-error value 'character))
  (when (or (< index 0) (<= (array-total-size array) index))
    (error "Invalid index ~D for ~S, should be a non-negative integer below ~D."
           index array index))
  (setf (array-contents array)
        (system:concat-raw-string/3 (system:sub-raw-string/3 (array-contents array) 0 index)
                                    value
                                    (system:sub-raw-string/2 (array-contents array) (1+ index))))
  value)

(defun (cl:setf aref) (value array &rest subscripts)
  (unless (arrayp array)
    (type-error array 'array))
  (cond ((eq (array-element-type array) 'character)
         (unless (and subscripts (null (cdr subscripts)))
           (subscripts-error (length subscripts) 1))
         (%string-set array (car subscripts) value))
        (t
         (system:raw-array-set (array-contents array)
                               (array-row-major-index array subscripts)
                               value))))

(defun row-major-aref (array index)
  (unless (arrayp array)
    (type-error array 'array))
  (when (or (< index 0) (<= (array-total-size array) index))
    (error "Invalid index ~D for ~S, should be a non-negative integer below ~D."
           index array (array-total-size array)))
  (system:raw-array-ref (array-contents array) index))

(defun (cl:setf row-major-aref) (value array index)
  (unless (arrayp array)
    (type-error array 'array))
  (cond ((eq (array-element-type array) 'character)
         (%string-set array index value))
        (t
         (when (or (< index 0) (<= (array-total-size array) index))
           (error "Invalid index ~D for ~S, should be a non-negative integer below ~D."
                  index array (array-total-size array)))
         (system:raw-array-set (array-contents array) index value))))

(defun svref (vector index)
  (system:raw-array-ref (array-contents vector) index))

(defun (cl:setf svref) (value vector index)
  (system:raw-array-set (array-contents vector) index value))

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
      (array-total-size array)))

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
