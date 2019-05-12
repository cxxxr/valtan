(in-package :common-lisp)

#+(or)
(export '(hash-table
          hash-table-p
          hash-table-test
          hash-table-size
          hash-table-rehash-size
          hash-table-rehash-threshold
          make-hash-table
          hash-table-count
          gethash
          remhash
          maphash
          with-hash-table-iterator
          clrhash
          sxhash))

(defstruct (hash-table (:copier nil)
                       (:predicate hash-table-p)
                       (:constructor %make-hash-table))
  object
  count*
  keys
  (test :read-only t)
  (size :read-only t)
  (rehash-size :read-only t)
  (rehash-threshold :read-only t))

(defun make-hash-table (&key test size rehash-size rehash-threshold)
  (%make-hash-table :object (ffi:make-object)
                    :count* 0
                    :test test
                    :size size
                    :rehash-size rehash-size
                    :rehash-threshold rehash-threshold))

(defun hash-table-count (hash-table)
  (hash-table-count* hash-table))

(defun gethash (key hash-table &optional default)
  (let ((value (ffi:object-get (hash-table-object hash-table) key)))
    (if (eq (ffi:typeof value) "undefined")
        (values default nil)
        (values value t))))

(defun (setf gethash) (value key hash-table &optional default)
  (when (eq (ffi:object-get (hash-table-object hash-table) key)
            (ffi:ref "undefined"))
    (incf (hash-table-count* hash-table)))
  (ffi:object-set (hash-table-object hash-table) key value)
  value)

(defun remhash (key hash-table)
  (multiple-value-bind (value found)
      (gethash key hash-table)
    (declare (ignore value))
    (setf (gethash key hash-table) (ffi:ref "undefined"))
    (when found
      (decf (hash-table-count* hash-table)))
    found))

(defun maphash (function hash-table)
  )

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  )

(defun clrhash (hash-table)
  )
