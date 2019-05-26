(in-package :compiler)

(defun make-keyword (x)
  (intern (princ-to-string x) :keyword))

(defvar *genvar-counter* 0)

(defun genvar (prefix)
  (format nil "~A_~D" prefix (incf *genvar-counter*)))
