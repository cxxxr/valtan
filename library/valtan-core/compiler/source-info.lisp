(in-package :compiler)

(defvar *source-info*)

(defun make-source-info ()
  (make-hash-table :test 'eq))

(defun add-source-info (form position)
  (setf (gethash form *source-info*) position)
  form)

(defun get-form-position (form)
  (and (boundp '*source-info*)
       (gethash form *source-info*)))
