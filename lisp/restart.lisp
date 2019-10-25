(in-package :common-lisp)

(defvar *restarts* '())

(defstruct restart
  name
  function)

(defun find-restart (identier &optional condition)
  (find identier *restarts* :key #'restart-name))

(defun invoke-restart (restart-name &rest values)
  (let ((restart (find-restart restart-name)))
    (unless restart
      (error 'control-error
             :format-control "No restart ~S is active"
             :format-arguments (list restart-name)))
    (apply (restart-function restart) values)))

(defmacro restart-bind (bindings &body forms)
  `(let ((*restarts* *restarts*))
     ,@(mapcar (lambda (b)
                 `(push (make-restart :name ',(car b) :function ,(cadr b)) *restarts*))
               (reverse bindings))
     ,@forms))
