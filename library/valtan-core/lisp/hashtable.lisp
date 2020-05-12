#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

;;; XXX: jsのMapを使っているのでhash-tableのtestがequal, equalpの場合に機能しない

(defstruct (hash-table (:copier nil)
                       (:predicate hash-table-p)
                       (:constructor %make-hash-table))
  map
  (keys nil)
  (test nil :read-only t)
  ;; (size nil :read-only t)
  (rehash-size nil :read-only t)
  (rehash-threshold nil :read-only t))

(defun make-hash-table (&key test size rehash-size rehash-threshold)
  (declare (ignore size))
  (%make-hash-table :map (system:make-map)
                    :test test
                    ;; :size size
                    :rehash-size rehash-size
                    :rehash-threshold rehash-threshold))

(defun hash-table-size (hash-table)
  (hash-table-count hash-table))

(defun hash-table-count (hash-table)
  (system:map-length (hash-table-map hash-table)))

(defun gethash (key hash-table &optional default)
  (multiple-value-bind (value found)
      (system:map-get (hash-table-map hash-table) key)
    (values (if found value default)
            found)))

(defun (cl:setf gethash) (value key hash-table &optional default)
  (declare (ignore default))
  (system:map-set (hash-table-map hash-table) key value)
  (push key (hash-table-keys hash-table))
  value)

(defun remhash (key hash-table)
  (multiple-value-bind (value found)
      (gethash key hash-table)
    (declare (ignore value))
    (setf (gethash key hash-table) (ffi:ref "undefined"))
    found))

(defun maphash (function hash-table)
  (let* ((map (hash-table-map hash-table))
         (keys (hash-table-keys hash-table)))
    (dolist (key keys)
      (multiple-value-bind (value found)
          (system:map-get map key)
        (when found
          (funcall function key value)))))
  nil)

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (let ((g-keys (gensym "KEYS"))
        (g-hash-table (gensym "HASH-TABLE"))
        (g-map (gensym "MAP")))
    `(let* ((,g-hash-table ,hash-table)
            (,g-keys (hash-table-keys ,g-hash-table))
            (,g-map (hash-table-map ,g-hash-table)))
       (flet ((,name ()
                (when ,g-keys
                  (let* ((%key (pop ,g-keys))
                         (%value (system:map-get ,g-map %key)))
                    (values t %key %value)))))
         ,@body))))

(defun clrhash (hash-table)
  (system:map-clear (hash-table-map hash-table))
  hash-table)
