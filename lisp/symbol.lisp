(in-package :common-lisp)

;; TODO: defconstantで定義する
(defvar +unbound+ '#:unbound)

(ffi:set (ffi:ref "lisp" "tValue") t)
(ffi:set (ffi:ref "lisp" "nilValue") nil)

(defstruct (symbol (:constructor %make-symbol)
                   (:copier nil)
                   (:predicate symbolp))
  name
  (%value +unbound+)
  (%function +unbound+)
  plist
  package)

(defun make-symbol (string)
  (%make-symbol :name string))

(defun keywordp (x)
  (and (symbolp x)
       (eq (symbol-package x)
           (find-package :keyword))))

(defun symbol-value (symbol)
  (let ((value (symbol-%value symbol)))
    (if (eq value +unbound+)
        (error "The variable ~S is unbound." symbol)
        value)))

(defun symbol-function (symbol)
  (let ((function (symbol-%function symbol)))
    (if (eq function +unbound+)
        (error "The function ~S is undefined." symbol)
        function)))

(defun system::fset (symbol function)
  (setf (symbol-%function symbol)
        function))

(defun system::set-symbol-value (symbol value)
  (setf (symbol-%value symbol) value))

(defun boundp (symbol)
  (not (eq +unbound+ (symbol-value symbol))))

(defun fboundp (symbol)
  (not (eq +unbound+ (symbol-function symbol))))

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

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
