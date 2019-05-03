(in-package :compiler)

(defun compile-stdin ()
  (let* ((*require-modules* '())
         (ir-forms
           (loop :with eof-value := '#:eof-value
                 :for form := (let ((*package* (find-package "CL-USER")))
                                (read *standard-input* nil eof-value))
                 :until (eq form eof-value)
                 :collect (pass1-toplevel form))))
    (write-line "import * as lisp from 'lisp';")
    (dolist (module *require-modules*)
      (format t "require('~A.lisp');~%" module))
    (pass2-toplevel-forms ir-forms)))

(defun compile-toplevel (form)
  (let ((*require-modules* '()))
    (pass2-toplevel (pass1-toplevel form))))

(defun compile-files (files)
  (unless (listp files) (setf files (list files)))
  (let ((*require-modules* '())
        (ir-forms '()))
    (dolist (file files)
      (with-open-file (in file)
        (loop :with eof-value := '#:eof-value
              :for form := (read in nil eof-value)
              :until (eq form eof-value)
              :do (push (pass1-toplevel form) ir-forms))))
    (write-line "import * as lisp from 'lisp';")
    (dolist (module *require-modules*)
      (format t "require('~A.lisp');~%" module))
    (pass2-toplevel-forms (nreverse ir-forms))
    (values)))

(defmacro with-js-beautify (&body body)
  `(let ((output
           (with-output-to-string (*standard-output*)
             (progn ,@body))))
     (with-input-from-string (in output)
       (uiop:run-program "js-beautify"
                         :input in
                         :output t))))
