(in-package :common-lisp)

#+(or)
(export '(hash-table
          hash-table-p
          hash-table-test
          hash-table-size
          hash-table-rehash-size
          hash-table-rehash-threshold
          make-hash-table
          hash-table-count))

(defstruct (hash-table (:copier nil)
                       (:constructor %make-hash-table))
  object
  count*
  (test :read-only t)
  (size :read-only t)
  (rehash-size :read-only t)
  (rehahs-threshold :read-only t))

(defun make-hash-table (&key test size rehash-size rehash-threshold)
  (%make-hash-table :object (ffi:make-object)
                    :count* 0
                    :test test
                    :size size
                    :rehash-size rehash-size
                    :rehash-threshold rehash-threshold))

(defun hash-table-count (hash-table)
  (hash-table-count* hash-table))

