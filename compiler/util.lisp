(in-package :compiler)

(defun make-keyword (x)
  (intern (princ-to-string x) :keyword))
