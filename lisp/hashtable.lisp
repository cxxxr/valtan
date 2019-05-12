(in-package :common-lisp)

(defstruct (hash-table (:copier nil)
                       (:constructor %make-hash-table))
  object
  %count)

(defun make-hash-table (&key test size rehash-size rehash-threshold)
  (%make-hash-table :object (ffi:make-object)
                    :%count 0))

