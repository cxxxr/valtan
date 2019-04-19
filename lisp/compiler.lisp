(in-package :compiler)

(defun compile-toplevel (form)
  (pass2-toplevel form)
  (values))

(defun compile-stdin ()
  (write-line "import * as lisp from 'lisp';")
  (loop :with eof-value := '#:eof-value
        :for form := (let ((*package* (find-package "CL-USER")))
                       (read *standard-input* nil eof-value))
        :until (eq form eof-value)
        :do (compile-toplevel form)))
