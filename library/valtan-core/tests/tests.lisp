(cl:defpackage :valtan-core/tests
  (:use :cl))
(cl:in-package :valtan-core/tests)

(defvar *valtan-package* (find-package :valtan-core))

(defun test-pathname (name)
  (make-pathname :name name
                 :type "lisp"
                 :defaults (asdf:system-relative-pathname :valtan-core "tests/sacla-tests/")))

(defun test-form (form)
  (pprint form)
  (unless (eval form)
    (cerror "NG" "~S" form)))

(defmacro with-valtan-env (() &body body)
  `(let ((*package* *valtan-package*)
         (*readtable* valtan-core::*valtan-readtable*))
     ,@body))

(defun run-test (pathname)
  (with-open-file (in pathname)
    (loop :with eof := '#:eof
          :for form := (with-valtan-env ()
                         (read in nil eof))
          :until (eq form eof)
          :do (with-valtan-env ()
                (test-form form)))))

(defun main ()
  ;; (run-test (test-pathname "must-cons"))
  ;; (run-test (test-pathname "must-character"))
  ;; (run-test (test-pathname "must-symbol"))
  ;; (run-test (test-pathname "must-string"))
  (run-test (test-pathname "must-sequence")))
