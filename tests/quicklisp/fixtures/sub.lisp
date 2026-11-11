(in-package :fixture-utils)
(defparameter *sub-value* (twice *read-value*))
;; The READTABLE binding assigned by reader.lisp must not leak into this file.
(assert (null (get-dispatch-macro-character #\# #\?)))
