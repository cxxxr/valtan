(in-package :common-lisp)

(defun make-symbol (string)
  (system::%make-symbol (array-contents string)))

(defun symbol-name (symbol)
  (system::js-string-to-array (system::%symbol-name symbol)))

(defun symbol-package (symbol)
  (find-package symbol))

(defun keywordp (x)
  (and (symbolp x)
       (eq (symbol-package x)
           (find-package :keyword))))

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defun (setf symbol-plist) (plist symbol)
  (system::put-symbol-plist symbol plist))

(defun %put (symbol indicator value)
  (let* ((plist (symbol-plist symbol))
         (mem (member indicator plist)))
    (if mem
        (setf (cadr mem) value)
        (setf (symbol-plist symbol)
              (list* indicator value plist)))
    value))

(defsetf get (symbol indicator)
    (value)
  `(%put ,symbol ,indicator ,value))
