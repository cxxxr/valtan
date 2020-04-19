(in-package :compiler)

(defvar *source-info*)

(defun make-source-info ()
  (make-hash-table :test 'eq))

(defun get-form-position (form)
  (and (boundp '*source-info*)
       (gethash form *source-info*)))

(defun add-source-info (form position)
  (setf (gethash form *source-info*) position)
  form)

(defun add-source-info* (form position)
  (when (boundp '*source-info*)
    (labels ((walk (form)
               (when (consp form)
                 (setf (gethash form *source-info*) position)
                 (walk (car form))
                 (walk (cdr form)))))
      (walk form))))
