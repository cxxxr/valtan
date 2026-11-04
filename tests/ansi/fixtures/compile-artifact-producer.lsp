(in-package :cl-test)

(deftest write-artifact-for-another-process
  (progn
    (with-open-file (out "restart-source.lisp" :direction :output :if-exists :supersede)
      (write-line "(in-package :cl-test)" out)
      (write-line "(defmacro restarted-macro (x) (list 'quote (list x :macro)))" out)
      (write-line "(defun restarted-function () (values (restarted-macro :function) #p\"relative/file.txt\"))" out))
    (let ((path (compile-file "restart-source.lisp")))
      (delete-file "restart-source.lisp")
      (values (pathnamep path) (fboundp 'restarted-function))))
  t nil)
