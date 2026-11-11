(in-package :fixture-utils)
(assert (eq *file-local-value* :outer))
(setf *file-local-value* :inside)
(defparameter *file-local-observed* *file-local-value*)
