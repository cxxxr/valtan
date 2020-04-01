(defpackage :valtan-host.reader
  (:use :cl)
  (:export :read-in-valtan
           :map-file-forms))
(in-package :valtan-host.reader)

(defmacro *:quasiquote (x)
  (compiler::expand-quasiquote x))

(defun call-with-valtan-reader (package function)
  (let ((string-reader (fdefinition 'valtan-core.reader:string-reader))
        (array-reader (fdefinition 'valtan-core.reader:array-reader)))
    (setf (fdefinition 'valtan-core.reader:string-reader)
          (lambda (&rest args)
            (let ((valtan-string (apply string-reader args)))
              (first (valtan-core::structure-values valtan-string)))))
    (setf (fdefinition 'valtan-core.reader:array-reader)
          (lambda (&rest args)
            (let ((valtan-array (apply array-reader args)))
              (first (valtan-core::structure-values valtan-array)))))
    (unwind-protect
         (let ((valtan-core::*package* package)
               (*package* package))
           (funcall function))
      (setf (fdefinition 'valtan-core.reader:string-reader) string-reader)
      (setf (fdefinition 'valtan-core.reader:array-reader) array-reader))))

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
