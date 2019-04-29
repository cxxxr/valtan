(in-package :compiler)

(defun compile-stdin ()
  (write-line "import * as lisp from 'lisp';")
  (let ((ir-forms
          (loop :with eof-value := '#:eof-value
                :for form := (let ((*package* (find-package "CL-USER")))
                               (read *standard-input* nil eof-value))
                :until (eq form eof-value)
                :collect (pass1-toplevel form))))
    (pass2-toplevel-forms ir-forms)))

(defun compile-toplevel (form)
  (pass2-toplevel (pass1-toplevel form)))

(defun compile-toplevel-with-js-beautify (form)
  (let ((output
          (with-output-to-string (*standard-output*)
            (compile-toplevel form))))
    (with-input-from-string (in output)
      (uiop:run-program "js-beautify"
                        :input in
                        :output t))))
