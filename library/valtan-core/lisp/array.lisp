#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defstruct (array (:constructor %make-array)
                  (:copier nil)
                  (:predicate arrayp))
  contents
  dimensions
  total-size
  displaced-to
  displaced-index-offset
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

(defun make-array-contents-with-initial-contents (dimensions total-size rank element-type initial-contents)
  (cond ((and (eq element-type 'character) (= rank 1))
         (let* ((len (length initial-contents))
                (raw-string (system:make-raw-string)))
           (unless (= (car dimensions) len)
             (error "There are ~D elements in the :INITIAL-CONTENTS, but the vector length is ~D."
                    (length initial-contents)
                    (car dimensions)))
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
               (raw-array (system:make-raw-array total-size)))
           (labels ((f (dimensions initial-contents)
                      (cond ((null dimensions))
                            (t
                             (when (/= (first dimensions) (length initial-contents))
                               (error "There are ~D elements in the :INITIAL-CONTENTS, but the vector length is ~D."
                                      (length initial-contents)
                                      (first dimensions)))
                             (if (null (cdr dimensions))
                                 (map nil
                                      (lambda (content)
                                        (system:raw-array-set raw-array (incf i) content))
                                      initial-contents)
                                 (map nil
                                      (lambda (content)
                                        (f (cdr dimensions) content))
                                      initial-contents))))))
             (f dimensions initial-contents)
             raw-array)))))

(defun make-array-contents-with-initial-element (dimensions total-size rank element-type initial-element initial-element-p)
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
         (system:fill-raw-array (system:make-raw-array (car dimensions))
                                initial-element))
        ((= rank 0)
         (let ((raw-array (system:make-raw-array 1)))
           (system:raw-array-set raw-array 0 initial-element)
           raw-array))
        (t
         (let ((raw-array (system:make-raw-array total-size)))
           (system:fill-raw-array raw-array initial-element)))))

(defun copy-array-contents (old-dimensions old new-dimensions new
                            initial-element initial-element-p)
  (let ((n 0) (o 0))
    (macrolet ((post-incf (x)
                 `(let ((old ,x)) (setq ,x (1+ old)) old)))
      (labels ((copy (new-dims old-dims)
                 (let ((old-key (car old-dims)))
                   (dotimes (i (car new-dims)
                               (when (and old-key (< i old-key))
                                 (incf o (apply #'* (- old-key i) (rest old-dims)))))
                     (unless (and old-key (< i old-key))
                       (setf old-dims nil))
                     (cond ((cdr new-dims)
                            (copy (cdr new-dims) (cdr old-dims)))
                           ((or old-dims initial-element-p)
                            (setf (row-major-aref new (post-incf n))
                                  (if old-dims
                                      (row-major-aref old (post-incf o))
                                      initial-element)))
                           (t (incf n)))))))
        (copy new-dimensions old-dimensions)
        new))))

(defun make-or-adjust-array (array dimensions &key (element-type (if array
                                                                     (array-element-type array)
                                                                     t))
                                                   (initial-element nil initial-element-p)
                                                   (initial-contents nil initial-contents-p)
                                                   adjustable
                                                   fill-pointer
                                                   displaced-to
                                                   displaced-index-offset)
  (declare (ignore adjustable))
  (let* ((dimensions (cond ((null dimensions) nil)
                           ((consp dimensions)
                            dimensions)
                           (t
                            (list dimensions))))
         (rank (length dimensions)))
    (when fill-pointer
      (when (/= rank 1)
        (error "Only vectors can have fill pointers."))
      (cond ((eq fill-pointer t)
             (setq fill-pointer (car dimensions)))
            ((not (and (integerp fill-pointer)
                       (<= 0 fill-pointer)))
             (error "Bad fill-pointer: ~S" fill-pointer))))
    (let* ((element-type (upgraded-array-element-type element-type))
           (total-size (apply #'* dimensions))
           (contents (cond (displaced-to
                            (when (or initial-contents-p initial-element-p)
                              (error ":INITIAL-ELEMENT may not be specified with the :DISPLACED-TO option"))
                            (unless displaced-index-offset
                              (setq displaced-index-offset 0))
                            (array-contents displaced-to))
                           (t
                            (when (and initial-contents-p initial-element-p)
                              (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
                            (if initial-contents-p
                                (make-array-contents-with-initial-contents dimensions
                                                                           total-size
                                                                           rank
                                                                           element-type
                                                                           initial-contents)
                                (make-array-contents-with-initial-element dimensions
                                                                          total-size
                                                                          rank
                                                                          element-type
                                                                          initial-element
                                                                          initial-element-p)))))
           (new-array
             (%make-array :contents contents
                          :dimensions dimensions
                          :total-size total-size
                          :displaced-to displaced-to
                          :displaced-index-offset displaced-index-offset
                          :fill-pointer fill-pointer
                          :rank rank
                          :element-type element-type)))
      (cond (array
             (copy-array-contents (array-dimensions array) array
                                  dimensions new-array
                                  initial-element
                                  initial-element-p)
             (setf (array-dimensions array) dimensions
                   (array-total-size array) total-size
                   (array-displaced-index-offset array) displaced-index-offset
                   (array-rank array) rank)
             new-array)
            (t
             new-array)))))

(defun make-array (dimensions &rest args)
  (apply #'make-or-adjust-array nil dimensions args))

(defun adjust-array (array dimensions &rest args)
  (apply #'make-or-adjust-array array dimensions args))

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

(defun adjustable-array-p (array)
  (if (array-fill-pointer array) t nil))

(defun fill-pointer (array)
  (array-fill-pointer array))

(defun (cl:setf fill-pointer) (fill-pointer array)
  (setf (array-fill-pointer array) fill-pointer))

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

(defun array-dimension (array axis-number)
  (nth axis-number (array-dimensions array)))

(defun array-displacement (array)
  (if (array-displaced-to array)
      (values (array-displaced-to array)
              (array-displaced-index-offset array))
      (values nil 0)))

(defun subscripts-error (n-subscripts rank)
  (error "Wrong number of subscripts, ~D, for array of rank ~D." n-subscripts rank))

(defun %array-row-major-index (array subscripts out-of-bounds-error-p)
  (assert (listp subscripts))
  (unless (arrayp array)
    (type-error array 'array))
  (let ((rank (array-rank array))
        (dimensions (array-dimensions array)))
    (unless (= rank (length subscripts))
      (subscripts-error (length subscripts) rank))
    (do ((axis (1- rank) (1- axis))
         (chunk-size 1)
         (result 0))
        ((minusp axis)
         (if (array-displaced-to array)
             (+ result (array-displaced-index-offset array))
             result))
      (let ((index (nth axis subscripts))
            (dim (nth axis dimensions)))
        (when (or (< index 0) (<= dim index))
          (if out-of-bounds-error-p
              (error "Invalid index ~D for axis ~D of ~S, should be a non-negative integer below ~D."
                     index axis array index)
              (return nil)))
        (incf result (* chunk-size index))
        (setq chunk-size (* chunk-size dim))))))

(defun array-row-major-index (array &rest subscripts)
  (%array-row-major-index array subscripts t))

(defun array-in-bounds-p (array &rest subscripts)
  (if (%array-row-major-index array subscripts nil)
      t
      nil))

(defun aref (array &rest subscripts)
  (system:raw-array-ref (array-contents array) (%array-row-major-index array subscripts t)))

(defun *::%vector-ref (vector index)
  (system:raw-array-ref (array-contents vector) index))

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
                               (%array-row-major-index array subscripts t)
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

(defun vector-pop (vector)
  (when (or (null (array-fill-pointer vector))
            (>= 0 (array-fill-pointer vector)))
    (error (error "The fill pointer of the vector ~S is zero." vector)))
  (decf (array-fill-pointer vector))
  (system:raw-array-ref (array-contents vector) (array-fill-pointer vector)))

(defun vector-push (new-element vector)
  (let ((fp (array-fill-pointer vector)))
    (when (and (array-fill-pointer vector)
               (<= 0 (array-fill-pointer vector)))
      (when (< fp (array-total-size vector))
        (incf (array-fill-pointer vector))
        (setf (aref vector fp) new-element)
        fp))))

(defun vector-push-extend (new-element vector)
  (let ((size (array-total-size vector))
        (fp (array-fill-pointer vector)))
    (when (>= fp size)
      (adjust-array vector (* (1+ size) 2)))
    (setf (aref vector fp) new-element)
    (incf (array-fill-pointer vector))
    fp))

(defun svref (vector index)
  (system:raw-array-ref (array-contents vector) index))

(defun (cl:setf svref) (value vector index)
  (system:raw-array-set (array-contents vector) index value))

(defun bit (bit-array &rest subscripts)
  (system:raw-array-ref (array-contents bit-array) (%array-row-major-index bit-array subscripts t)))

(defun (cl:setf bit) (bit bit-array &rest subscripts)
  (system:raw-array-set (array-contents bit-array) (%array-row-major-index bit-array subscripts t) bit))

(defun sbit (bit-array &rest subscripts)
  (system:raw-array-ref (array-contents bit-array) (%array-row-major-index bit-array subscripts t)))

(defun (cl:setf sbit) (bit bit-array &rest subscripts)
  (system:raw-array-set (array-contents bit-array) (%array-row-major-index bit-array subscripts t) bit))

(defun bit-and (bit-array-1 bit-array-2 &optional opt-arg)
  (unless (equal (array-dimensions bit-array-1)
                 (array-dimensions bit-array-2))
    (error "~S and ~S don't have the same dimensions." bit-array-1 bit-array-2))
  (let ((dst-bit-array
          (case opt-arg
            ((t)
             bit-array-1)
            ((nil)
             (make-array (array-dimensions bit-array-1) :element-type 'bit))
            (otherwise
             opt-arg))))
    (dotimes (i (array-total-size bit-array-1))
      (setf (row-major-aref dst-bit-array i)
            (*:%logand (system:raw-array-ref bit-array-1 i)
                       (system:raw-array-ref bit-array-2 i))))
    dst-bit-array))

(defun vector-length (vector)
  (or (array-fill-pointer vector)
      (array-total-size vector)))

(defun array-equalp (x y)
  (if (vectorp x)
      (and (vectorp y)
           (let ((len1 (vector-length x))
                 (len2 (vector-length y)))
             (and (= len1 len2)
                  (dotimes (i len1 t)
                    (unless (equalp (svref x i)
                                    (svref y i))
                      (return nil))))))
      (and (equal (array-dimensions x)
                  (array-dimensions y))
           (dotimes (i (array-total-size x) t)
             (unless (equalp (row-major-aref x i)
                             (row-major-aref y i))
               (return nil))))))
