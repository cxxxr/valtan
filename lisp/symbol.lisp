(in-package :common-lisp)

(defun getf (place indicator &optional default)
  (do ((list place (cddr list)))
      ((null list) default)
    (when (eq indicator (car list))
      (return (cadr list)))))

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator))
