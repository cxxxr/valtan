(cl:in-package :valtan-core)

(cl:defmacro system:multiple-value-call (function cl:&rest args)
  `(cl:multiple-value-call ,function ,@args))

(cl:defun system:structure-p (structure)
  (cl:declare (cl:ignore structure))
  cl:nil)

(cl:defun system:%structure-name (structure)
  (cl:declare (cl:ignore structure))
  cl:nil)

(cl:defun system:%structure-slot-count (structure)
  (cl:declare (cl:ignore structure))
  cl:nil)

(cl:defun system:%structure-ref (structure index)
  (cl:declare (cl:ignore structure index))
  cl:nil)

(cl:defmacro system:defmacro* (name lambda-list cl:&body body)
  `(cl:defmacro ,name ,lambda-list ,@body))
