(defpackage :valtan-host.reader
  (:use :cl)
  (:export :read-in-valtan
           :read-from-string-in-valtan
           :map-file-forms))
(in-package :valtan-host.reader)

(defmacro *:backquote (x)
  (compiler::expand-backquote x))

(defun ensure-raw-array (x)
  (if (typep x 'valtan-core::structure)
      (first (valtan-core::structure-values x))
      x))

(defun stream-position (stream)
  (when (valtan-core::file-input-stream-p stream)
    (cons (valtan-core::file-input-stream-line stream)
          (valtan-core::file-input-stream-column stream))))

(defun call-with-valtan-reader (package function)
  (let ((string-reader (fdefinition 'valtan-core.reader:string-reader))
        (array-reader (fdefinition 'valtan-core.reader:array-reader))
        (cl-to-js-reader (fdefinition 'valtan-core.reader:cl-to-js-reader))
        (cons-reader (fdefinition 'valtan-core.reader:cons-reader))
        (compiler::*source-info* (compiler::make-source-info)))
    (setf (fdefinition 'valtan-core.reader:string-reader)
          (lambda (stream &rest args)
            (let ((position (stream-position stream))
                  (string (apply string-reader stream args)))
              (compiler::add-source-info (ensure-raw-array string) position))))
    (setf (fdefinition 'valtan-core.reader:array-reader)
          (lambda (stream &rest args)
            (let ((position (stream-position stream))
                  (array (apply array-reader stream args)))
              (compiler::add-source-info (ensure-raw-array array)
                                         position))))
    (setf (fdefinition 'valtan-core.reader:cl-to-js-reader)
          (lambda (stream &rest args)
            (let ((position (stream-position stream))
                  (form (apply cl-to-js-reader stream args)))
              (compiler::add-source-info (case (first form)
                                           ((ffi:ref)
                                            (cons 'ffi:ref (mapcar #'ensure-raw-array (rest form))))
                                           ((ffi:cl->js)
                                            form))
                                         position))))
    (setf (fdefinition 'valtan-core.reader:cons-reader)
          (lambda (stream &rest args)
            (let ((position (stream-position stream))
                  (form (apply cons-reader stream args)))
              (compiler::add-source-info form position))))
    (unwind-protect
         (let ((valtan-core::*package* package)
               (*package* package))
           (funcall function))
      (setf (fdefinition 'valtan-core.reader:string-reader) string-reader)
      (setf (fdefinition 'valtan-core.reader:array-reader) array-reader)
      (setf (fdefinition 'valtan-core.reader:cl-to-js-reader) cl-to-js-reader)
      (setf (fdefinition 'valtan-core.reader:cons-reader) cons-reader))))

(defun read-from-string-in-valtan (string)
  (call-with-valtan-reader
   *package*
   (lambda ()
     (valtan-core::read-from-string
      (system::make-structure-array! string)))))

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
