((ffi:ref "console" "log") #j"hashtable-impl.lisp")

(defpackage :hash-table-impl
  (:use :cl)
  (:shadow :hash-table
           :hash-table-p
           :hash-table-test
           :hash-table-size
           :hash-table-rehash-size
           :hash-table-rehash-threshold
           :make-hash-table
           :hash-table-count
           :gethash
           :remhash
           :maphash
           :with-hash-table-iterator
           :clrhash
           :sxhash))
(in-package :hash-table-impl)

(defstruct (%hash-table (:copier nil)
                        (:predicate %hash-table-p)
                        (:constructor %make-hash-table-aux))
  table
  size
  test
  rehash-size
  rehash-threshold
  count)

(defun ensure-hash-key-test (test)
  (if (member test '(eq eql equal))
      test
      (cond ((eq test #'eq)
             'eq)
            ((eq test #'eql)
             'eql)
            ((eq test #'equal)
             'equal)
            (t
             (error "unexpected test: ~S" test)))))

(defun make-hash-table (&key (test 'eql) (size 16) (rehash-size 1.5) (rehash-threshold 1.0))
  (%make-hash-table-aux
   :table (make-array size :initial-element nil)
   :test (ensure-hash-key-test test)
   :size size
   :rehash-size rehash-size
   :rehash-threshold rehash-threshold
   :count 0))

(defun hash-table-p (object)
  (%hash-table-p object))

(defun hash-table-count (hash-table)
  (%hash-table-count hash-table))

(defun hash-table-rehash-size (hash-table)
  (%hash-table-rehash-threshold hash-table))

(defun hash-table-rehash-threshold (hash-table)
  (%hash-table-rehash-threshold hash-table))

(defun hash-table-size (hash-table)
  (%hash-table-size hash-table))

(defun hash-table-test (hash-table)
  (%hash-table-test hash-table))

(defun %hash-key-node (key hash-table)
  (let* ((hash (sxhash key))
         (index (mod hash (%hash-table-size hash-table)))
         (table (%hash-table-table hash-table))
         (test (ecase (%hash-table-test hash-table)
                 (eq #'eq)
                 (eql #'eql)
                 (equal #'equal))))
    (let ((node (aref table index)))
      (do ((node node (cdr node)))
          ((null node) nil)
        (let ((elt (car node)))
          (when (funcall test key (car elt))
            (return node)))))))

(defun gethash (key hash-table)
  (let ((node (%hash-key-node key hash-table)))
    (if node
        (values (cdar node) t)
        (values nil nil))))

(defun (setf gethash) (value key hash-table)
  (let* ((hash (sxhash key))
         (index (mod hash (%hash-table-size hash-table)))
         (table (%hash-table-table hash-table))
         (test (ecase (%hash-table-test hash-table)
                 (eq #'eq)
                 (eql #'eql)
                 (equal #'equal))))
    (let ((head (aref table index)))
      (do ((node head (cdr node)))
          ((null node)
           (setf (aref table index) (acons key value head)))
        (let ((elt (car node)))
          (when (funcall test key (car elt))
            (return (setf (cdar node) value)))))
      (incf (%hash-table-count hash-table))
      (rehash-if-required hash-table)
      value)))

(defun rehash-if-required (hash-table)
  (when (require-rehash-p hash-table)
    (let* ((rehash-size (hash-table-rehash-size hash-table))
           (new-size (if (and (< 1 rehash-size)
                              (integerp rehash-size))
                         (+ (hash-table-size hash-table)
                            rehash-size)
                         (* (hash-table-size hash-table)
                            rehash-size))))
      (rehash hahs-table new-size))))

(defun require-rehash-p (hash-table)
  (let ((rehash-threshold (hash-table-rehash-threshold hash-table)))
    (>= (/ (hash-table-count hash-table)
           (hash-table-size hash-table))
        rehash-threshold)))

(defun rehash (hash-table size)
  )

(defun remhash (key hash-table)
  (declare (ignore key hash-table)))

(defun maphash (function hash-table)
  (declare (ignore function hash-table)))

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (declare (ignore name hash-table body)))

(defun clrhash (hash-table)
  (declare (ignore hash-table)))

(defun sxhash (object)
  (cond ((null object)
         0)
        ((symbolp object)
         (sxhash-symbol object))
        ((listp object)
         (sxhash-list object))
        ((characterp object)
         (char-code object))
        ((stringp object)
         (sxhash-string object))
        ((arrayp object)
         (sxhash-array object))
        ((hash-table-p object)
         (sxhash-hash-table object))
        ((integerp object)
         (if (< 0 object)
             object
             (- object)))
        ((numberp object)
         (floor object))
        ;; ((*:structure-p object)
        ;;  (sxhash-structure object))
        (t
         (error "unknown type: ~S" object))))

(defun sxhash-symbol (symbol)
  (sxhash-string (symbol-name symbol)))

(defun sxhash-list (list)
  (do ((i 0 (1+ i))
       (list list (cdr list))
       (hash 0))
      ((or (< 10 i)
           (null list))
       hash)
    (incf hash (sxhash (car list)))))

(defun sxhash-string (string)
  (let ((hash 0)
        (length (length string)))
    (dotimes (i (if (< 10 length) 10 length))
      (setq hash (+ (* hash 3) (char-code (aref string i)))))
    hash))

(defun sxhash-array (array)
  (let ((hash 0)
        (length (array-total-size array)))
    (dotimes (i (if (< 10 length) 10 length))
      (setq hash (+ (* hash 3) (sxhash (row-major-aref array i)))))
    hash))

(defun sxhash-hash-table (hash-table)
  (sxhash-array (%hash-table-table hash-table)))

;; (defun sxhash-structure (structure)
;;   (let ((hash (sxhash (*:%structure-name structure))))
;;     (if (< 0 (*:%structure-slot-count structure))
;;         (sxhash (*:%structure-ref structure 0))
;;         0)))
