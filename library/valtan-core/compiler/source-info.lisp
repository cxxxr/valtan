(in-package :compiler)

(defvar *source-info*)

(defun make-source-info ()
  (make-hash-table :test 'equal))

(defun source-info-add (source-info form position)
  (setf (gethash form source-info) position))
