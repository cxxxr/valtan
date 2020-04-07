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
        (cl-to-js-reader (fdefinition 'valtan-core.reader:cl-to-js-reader))
        (cons-reader (fdefinition 'valtan-core.reader:cons-reader)))
    (setf (fdefinition 'valtan-core.reader:string-reader)
          (lambda (stream &rest args)
	    (let ((position (valtan-core::file-position stream))
		  (string (apply string-reader stream args)))
              (compiler::make-si-object position (ensure-raw-array string)))))
    (setf (fdefinition 'valtan-core.reader:array-reader)
          (lambda (stream &rest args)
	    (let ((position (valtan-core::file-position stream))
		  (array (apply array-reader stream args)))
	      (compiler::make-si-object position
					(ensure-raw-array array)))))
    (setf (fdefinition 'valtan-core.reader:cl-to-js-reader)
          (lambda (stream &rest args)
            (let ((position (valtan-core::file-position stream))
		  (form (apply cl-to-js-reader stream args)))
              (compiler::make-si-object position
					(case (first form)
					  ((ffi:ref)
					   (cons 'ffi:ref (mapcar #'ensure-raw-array (rest form))))
					  ((ffi:cl->js)
					   form))))))
    (setf (fdefinition 'valtan-core.reader:cons-reader)
          (lambda (stream &rest args)
            (let ((position (valtan-core::file-position stream))
                  (form (apply cons-reader stream args)))
	      (compiler::make-si-object position form))))
    (unwind-protect
         (let ((valtan-core::*package* package)
               (*package* package))
           (funcall function))
      (setf (fdefinition 'valtan-core.reader:string-reader) string-reader)
      (setf (fdefinition 'valtan-core.reader:array-reader) array-reader)
      (setf (fdefinition 'valtan-core.reader:cl-to-js-reader) cl-to-js-reader)
      (setf (fdefinition 'valtan-core.reader:cons-reader) cons-reader))))

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
