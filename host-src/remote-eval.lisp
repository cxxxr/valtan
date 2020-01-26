(defpackage :valtan-host.remote-eval
  (:use :cl)
  (:export :start
           :js-eval
           :repl))
(in-package :valtan-host.remote-eval)

(defvar *context*)

(defun start ()
  (setq *context* (remote-js:make-context))
  (remote-js:start *context*))

(defun js-eval (form)
  (remote-js:eval *context*
                  (compiler:compile-toplevel form nil)))

(defun repl ()
  (flet ((input ()
           (format t "~&> ")
           (force-output)
           (valtan-host.reader:read-in-valtan)))
    (let ((*package* (find-package :valtan-user)))
      (loop :for form := (input)
            :do (js-eval form)))))
