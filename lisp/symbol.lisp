(in-package :common-lisp)

(defun getf (place indicator &optional default)
  (do ((list place (cddr list)))
      ((null list) default)
    (when (eq indicator (car list))
      (return (cadr list)))))

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defun %put (symbol indicator value)
  (let* ((plist (symbol-plist symbol))
         (mem (member indicator plist)))
    (if mem
        (setf (cadr mem) value)
        (system::put-symbol-plist symbol
                                  (list* indicator value plist)))
    value))

(defsetf get (symbol indicator &optional default)
    (value)
  `(%put ,symbol ,indicator ,value))
