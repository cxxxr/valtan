(in-package :common-lisp)

(defun make-symbol (string)
  (system::%make-symbol (array-contents string)))

(defun keywordp (x)
  (and (symbolp x)
       (eq (symbol-package x)
           (find-package :keyword))))

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

(defsetf get (symbol indicator)
    (value)
  `(%put ,symbol ,indicator ,value))
