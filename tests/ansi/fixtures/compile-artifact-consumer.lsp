(in-package :cl-test)

(deftest read-artifact-in-a-new-process
  (let ((previous (fboundp 'restarted-function)))
    (load "restart-source.fasl")
    (multiple-value-bind (value path) (funcall 'restarted-function)
      (values previous value (namestring path) (eval '(restarted-macro :again)))))
  nil (:function :macro) "relative/file.txt" (:again :macro))
