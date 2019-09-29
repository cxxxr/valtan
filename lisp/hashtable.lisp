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

;;; XXX: jsのMapを使っているのでhash-tableのtestがequal, equalpの場合に機能しない

(defstruct (hash-table (:copier nil)
                       (:predicate hash-table-p)
                       (:constructor %make-hash-table))
  object
  (keys nil)
  (test nil :read-only t)
  (size nil :read-only t)
  (rehash-size nil :read-only t)
  (rehash-threshold nil :read-only t))

(defun make-hash-table (&key test size rehash-size rehash-threshold)
  (%make-hash-table :object (ffi:new (ffi:ref "Map"))
                    :test test
                    :size size
                    :rehash-size rehash-size
                    :rehash-threshold rehash-threshold))

(defun hash-table-count (hash-table)
  (ffi:ref (hash-table-object hash-table) "size"))

(defun gethash (key hash-table &optional default)
  (let ((value ((ffi:ref (hash-table-object hash-table) "get") key)))
    (if (eq value (ffi:ref "undefined"))
        (values default nil)
        (values value t))))

(defun (setf gethash) (value key hash-table &optional default)
  ((ffi:ref (hash-table-object hash-table) "set") key value)
  (push key (hash-table-keys hash-table))
  value)

(defun remhash (key hash-table)
  (multiple-value-bind (value found)
      (gethash key hash-table)
    (declare (ignore value))
    (setf (gethash key hash-table) (ffi:ref "undefined"))
    found))

(defun maphash (function hash-table)
  (let* ((object (hash-table-object hash-table))
         (keys (hash-table-keys hash-table)))
    (dolist (key keys)
      (let ((value ((ffi:ref object "get") key)))
        (unless (eq value (ffi:ref "undefined"))
          (funcall function key value)))))
  nil)

#+(or)
(defmacro with-hash-table-iterator ((name hash-table) &body body)
  )

(defun clrhash (hash-table)
  ((ffi:ref (hash-table-object hash-table) "clear"))
  hash-table)
