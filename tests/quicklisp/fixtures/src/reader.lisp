(in-package :fixture-utils)
(eval-when (:compile-toplevel :execute)
  (setf *readtable* (copy-readtable *readtable*))
  (set-dispatch-macro-character #\# #\?
    (lambda (stream character parameter)
      (declare (ignore character parameter))
      (+ 1 (read stream t nil t)))))
(defparameter *read-value* #?41)
(assert (= (plus-three 4) 7))
