(defpackage :valtan-host.reader
  (:use :cl)
  (:export :read-in-valtan
           :map-file-forms))
(in-package :valtan-host.reader)

(defmacro *:quasiquote (x)
  (compiler::expand-quasiquote x))

(defun ensure-raw-array (x)
  (if (typep x 'valtan-core::structure)
      (first (valtan-core::structure-values x))
      x))

(defun call-with-valtan-reader (package function)
  (let ((string-reader (fdefinition 'valtan-core.reader:string-reader))
        (array-reader (fdefinition 'valtan-core.reader:array-reader))
        (cl-to-js-reader (fdefinition 'valtan-core.reader:cl-to-js-reader)))
    (setf (fdefinition 'valtan-core.reader:string-reader)
          (lambda (&rest args)
            (ensure-raw-array (apply string-reader args))))
    (setf (fdefinition 'valtan-core.reader:array-reader)
          (lambda (&rest args)
            (ensure-raw-array (apply array-reader args))))
    (setf (fdefinition 'valtan-core.reader:cl-to-js-reader)
          (lambda (&rest args)
            (let ((form (apply cl-to-js-reader args)))
              (case (first form)
                ((ffi:ref)
                 (cons 'ffi:ref (mapcar #'ensure-raw-array (rest form))))
                ((ffi:cl->js)
                 form)))))
    (unwind-protect
         (let ((valtan-core::*package* package)
               (*package* package))
           (funcall function))
      (setf (fdefinition 'valtan-core.reader:string-reader) string-reader)
      (setf (fdefinition 'valtan-core.reader:array-reader) array-reader)
      (setf (fdefinition 'valtan-core.reader:cl-to-js-reader) cl-to-js-reader))))

(defun read-in-valtan ()
  (call-with-valtan-reader
   *package*
   (lambda ()
     (valtan-core::read))))

(defun map-file-forms (function file)
  (call-with-valtan-reader
   (find-package :valtan-user)
   (lambda ()
     (valtan-core::with-open-file (stream file)
       (loop :with eof-value := '#:eof
             :for form := (valtan-core::read stream nil eof-value)
             :until (eq form eof-value)
             :do (funcall function form))))))
