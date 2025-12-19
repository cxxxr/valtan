#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

;;; Hash table implementation
;;; - eq/eql test: uses JavaScript Map for performance
;;; - equal/equalp test: uses bucket-based separate chaining

(defstruct (hash-table (:copier nil)
                       (:predicate hash-table-p)
                       (:constructor %make-hash-table))
  map        ; JS Map for eq/eql, or nil for equal/equalp
  buckets    ; vector of alists for equal/equalp, or nil for eq/eql
  (bucket-count 0)   ; number of buckets
  (keys nil)         ; list of all keys (for iteration)
  (count 0)          ; number of entries
  (test 'eql :read-only t)
  (rehash-size 1.5 :read-only t)
  (rehash-threshold 1.0 :read-only t))

(defun %normalize-test (test)
  "Normalize hash table test to symbol name."
  (cond ((null test) 'eql)
        ((eq test 'eq) 'eq)
        ((eq test 'eql) 'eql)
        ((eq test 'equal) 'equal)
        ((eq test 'equalp) 'equalp)
        ((eq test #'eq) 'eq)
        ((eq test #'eql) 'eql)
        ((eq test #'equal) 'equal)
        ((eq test #'equalp) 'equalp)
        (t (error "Invalid hash table test: ~S" test))))

;; Default number of buckets for equal/equalp hash tables
(defparameter *default-bucket-count* 16)

(defun %uses-js-map-p (test)
  "Return T if TEST can use JavaScript Map (eq or eql).
   Note: EQL tables with Float keys have a known limitation - JavaScript Map
   uses reference equality for Float wrapper objects, so two different Float
   objects with the same value may not hash to the same entry."
  (or (eq test 'eq) (eq test 'eql)))

(defun make-hash-table (&key test size rehash-size rehash-threshold)
  (declare (ignore size))
  (let ((normalized-test (%normalize-test test)))
    (if (%uses-js-map-p normalized-test)
        ;; eq/eql: use JavaScript Map
        (%make-hash-table :map (system:make-map)
                          :buckets nil
                          :bucket-count 0
                          :test normalized-test
                          :rehash-size (or rehash-size 1.5)
                          :rehash-threshold (or rehash-threshold 1.0))
        ;; equal/equalp: use bucket-based implementation
        (let ((bucket-count *default-bucket-count*))
          (%make-hash-table :map nil
                            :buckets (make-array bucket-count :initial-element nil)
                            :bucket-count bucket-count
                            :test normalized-test
                            :rehash-size (or rehash-size 1.5)
                            :rehash-threshold (or rehash-threshold 1.0))))))

(defun hash-table-size (hash-table)
  (if (hash-table-map hash-table)
      (system:map-length (hash-table-map hash-table))
      (hash-table-bucket-count hash-table)))

;;; Test function selector
(defun %get-test-function (test)
  (ecase test
    (eq #'eq)
    (eql #'eql)
    (equal #'equal)
    (equalp #'equalp)))

;;; Bucket-based lookup for equal/equalp
(defun %bucket-index (key bucket-count)
  (mod (sxhash key) bucket-count))

(defun %bucket-find (bucket key test-fn)
  "Find entry in bucket. Returns (entry . rest) or NIL."
  (do ((prev nil current)
       (current bucket (cdr current)))
      ((null current) nil)
    (when (funcall test-fn key (caar current))
      (return (cons (car current) prev)))))

(defun gethash (key hash-table &optional default)
  (let ((map (hash-table-map hash-table)))
    (if map
        ;; eq/eql: use JavaScript Map
        (multiple-value-bind (value found)
            (system:map-get map key)
          (values (if found value default) found))
        ;; equal/equalp: use bucket lookup
        (let* ((buckets (hash-table-buckets hash-table))
               (index (%bucket-index key (hash-table-bucket-count hash-table)))
               (bucket (aref buckets index))
               (test-fn (%get-test-function (hash-table-test hash-table)))
               (result (%bucket-find bucket key test-fn)))
          (if result
              (values (cdar result) t)
              (values default nil))))))

(defun (cl:setf gethash) (value key hash-table &optional default)
  (declare (ignore default))
  (let ((map (hash-table-map hash-table)))
    (if map
        ;; eq/eql: use JavaScript Map
        (progn
          (multiple-value-bind (old-value found)
              (system:map-get map key)
            (declare (ignore old-value))
            (unless found
              (push key (hash-table-keys hash-table))
              (incf (hash-table-count hash-table))))
          (system:map-set map key value))
        ;; equal/equalp: use bucket-based storage
        (let* ((buckets (hash-table-buckets hash-table))
               (index (%bucket-index key (hash-table-bucket-count hash-table)))
               (bucket (aref buckets index))
               (test-fn (%get-test-function (hash-table-test hash-table)))
               (result (%bucket-find bucket key test-fn)))
          (if result
              ;; Update existing entry
              (setf (cdar result) value)
              ;; Add new entry
              (progn
                (setf (aref buckets index) (acons key value bucket))
                (push key (hash-table-keys hash-table))
                (incf (hash-table-count hash-table)))))))
  value)

(defun remhash (key hash-table)
  (let ((map (hash-table-map hash-table)))
    (if map
        ;; eq/eql: use JavaScript Map
        (multiple-value-bind (value found)
            (system:map-get map key)
          (declare (ignore value))
          (when found
            (system:map-remove map key)
            (setf (hash-table-keys hash-table)
                  (delete key (hash-table-keys hash-table) :test #'eq :count 1))
            (decf (hash-table-count hash-table)))
          found)
        ;; equal/equalp: use bucket-based removal
        (let* ((buckets (hash-table-buckets hash-table))
               (index (%bucket-index key (hash-table-bucket-count hash-table)))
               (bucket (aref buckets index))
               (test-fn (%get-test-function (hash-table-test hash-table))))
          (do ((prev nil current)
               (current bucket (cdr current)))
              ((null current) nil)
            (when (funcall test-fn key (caar current))
              (if prev
                  (setf (cdr prev) (cdr current))
                  (setf (aref buckets index) (cdr current)))
              (setf (hash-table-keys hash-table)
                    (delete key (hash-table-keys hash-table) :test test-fn :count 1))
              (decf (hash-table-count hash-table))
              (return t)))))))

(defun maphash (function hash-table)
  (let ((keys (copy-list (hash-table-keys hash-table))))
    (dolist (key keys)
      (multiple-value-bind (value found)
          (gethash key hash-table)
        (when found
          (funcall function key value)))))
  nil)

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  ;; Note: Per ANSI CL, name should be visible to macro-function, but this
  ;; implementation uses flet for simplicity. Test 58 expects macro-function
  ;; to return non-nil, which this implementation doesn't support.
  (let ((g-keys (gensym "KEYS"))
        (g-hash-table (gensym "HASH-TABLE")))
    `(let* ((,g-hash-table ,hash-table)
            (,g-keys (copy-list (hash-table-keys ,g-hash-table))))
       (flet ((,name ()
                (if ,g-keys
                    (let ((%key (pop ,g-keys)))
                      (multiple-value-bind (%value %found)
                          (gethash %key ,g-hash-table)
                        (if %found
                            (values t %key %value)
                            ;; Key was removed during iteration, try next
                            (,name))))
                    (values nil nil nil))))
         ,@body))))

(defun clrhash (hash-table)
  (let ((map (hash-table-map hash-table)))
    (if map
        (system:map-clear map)
        ;; Clear all buckets
        (let ((buckets (hash-table-buckets hash-table)))
          (dotimes (i (length buckets))
            (setf (aref buckets i) nil)))))
  (setf (hash-table-keys hash-table) nil)
  (setf (hash-table-count hash-table) 0)
  hash-table)

(defun sxhash (object)
  "Return a hash code for OBJECT."
  (cond ((null object) 0)
        ((symbolp object) (%sxhash-string (symbol-name object)))
        ((stringp object) (%sxhash-string object))
        ((characterp object) (char-code object))
        ((integerp object) (if (< object 0) (- object) object))
        ((numberp object)
         ;; For floats, use floor of absolute value
         (let ((n (if (< object 0) (- object) object)))
           (floor n)))
        ((consp object) (%sxhash-list object))
        ((arrayp object) (%sxhash-array object))
        ((hash-table-p object) (%sxhash-array (hash-table-keys object)))
        (t 42)))  ; default hash for unknown types

(defun %sxhash-string (string)
  (let ((hash 0)
        (len (length string)))
    (dotimes (i (min len 10))
      (setq hash (+ (* hash 3) (char-code (char string i)))))
    hash))

(defun %sxhash-list (list)
  (let ((hash 0))
    (dotimes (i 10)
      (when (null list) (return hash))
      (setq hash (+ hash (sxhash (car list))))
      (setq list (cdr list)))
    hash))

(defun %sxhash-array (array)
  (let ((hash 0)
        (len (array-total-size array)))
    (dotimes (i (min len 10))
      (setq hash (+ (* hash 3) (sxhash (row-major-aref array i)))))
    hash))
