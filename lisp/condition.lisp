(in-package :common-lisp)

(defun error (datum &rest arguments)
  ;(system:%error (format nil datum arguments))
  (system:%error (array-contents datum)))
