(in-package :cl-test)

(defvar *compilation-events* nil)

(deftest compilation-artifact-survives-source-removal
  (let ((*compilation-events* nil))
    (fmakunbound 'compiled-regression-function)
    (with-open-file (out "compile-regression.lisp" :direction :output :if-exists :supersede)
      (write-line "(in-package :cl-test)" out)
      (write-line "(eval-when (:compile-toplevel) (push :compile *compilation-events*))" out)
      (write-line "(eval-when (:load-toplevel) (push :load *compilation-events*))" out)
      (write-line "(eval-when (:execute) (push :execute *compilation-events*))" out)
      (write-line "(defun compiled-regression-function () (values 17 :compiled))" out))
    (multiple-value-bind (artifact warnings failure) (compile-file "compile-regression.lisp")
      (let ((events (copy-list *compilation-events*))
            (bound (fboundp 'compiled-regression-function)))
        (delete-file "compile-regression.lisp")
        (load artifact)
        (delete-file artifact)
        (values events bound warnings failure *compilation-events*
                (multiple-value-list (funcall 'compiled-regression-function))))))
  (:compile) nil nil nil (:load :compile) (17 :compiled))

(deftest compiled-macro-definition-is-restored-by-load
  (progn
    (with-open-file (out "macro-regression.lisp" :direction :output :if-exists :supersede)
      (write-line "(in-package :cl-test)" out)
      (write-line "(defmacro artifact-macro (x) (list 'quote (list x :expanded)))" out)
      (write-line "(defun artifact-macro-user () (artifact-macro :value))" out))
    (let ((artifact (compile-file "macro-regression.lisp")))
      (fmakunbound 'artifact-macro)
      (load artifact)
      (values (eval '(artifact-macro :again)) (funcall 'artifact-macro-user))))
  (:again :expanded) (:value :expanded))

(deftest eval-when-obeys-execute-in-eval
  (let ((*compilation-events* nil))
    (eval '(eval-when (:compile-toplevel) (push :compile *compilation-events*)))
    (eval '(eval-when (:load-toplevel) (push :load *compilation-events*)))
    (eval '(eval-when (:execute) (push :execute *compilation-events*)))
    *compilation-events*)
  (:execute))

(deftest load-preserves-caller-bindings-and-open-stream
  (let* ((*package* (find-package :cl-test))
         (*readtable* (copy-readtable nil))
         (original-readtable *readtable*))
    (with-input-from-string
        (in "(setq *package* (find-package :cl-user) *readtable* (copy-readtable nil))")
      (values (load in) (eq *package* (find-package :cl-test))
              (eq *readtable* original-readtable) (open-stream-p in))))
  t t t t)

(deftest require-loads-provided-module-once
  (let ((*modules* nil) (*compilation-events* nil))
    (with-open-file (out "module-regression.lisp" :direction :output :if-exists :supersede)
      (write-line "(in-package :cl-test) (push :module *compilation-events*) (provide :fixture)" out))
    (require :fixture "module-regression.lisp")
    (require :fixture "module-regression.lisp")
    (values *compilation-events* *modules*))
  (:module) ("FIXTURE"))
