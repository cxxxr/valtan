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
           (format t "~&~A> " (system::%structure-ref
                               (valtan-core::package-name valtan-core::*package*)
                               0))
           (force-output)
           (let ((valtan-core::*standard-input* (valtan-core::make-standard-input-stream)))
             (valtan-host.reader:read-in-valtan))))
    (let ((*package* (find-package :valtan-user))
          (system:*get-stdin-line-function* (lambda ()
                                              (system::make-structure-array! (read-line)))))
      (loop :for form := (input)
            :do (js-eval form)))))
