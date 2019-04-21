(in-package :compiler)

(defun compile-error (message &rest args)
  (apply #'error message args))
