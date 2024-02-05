(defpackage :valtan-cli.init-project
  (:use :cl)
  (:export :main))
(in-package :valtan-cli.init-project)

(defparameter +option-spec+
  '((("type" #\t) :type string :documentation "application type (\"browser\" or \"node\")")))

(defun usage ()
  (write-line "usage: valtan init [options] project-name")
  (write-line "options:")
  (command-line-arguments:show-option-help +option-spec+))

(defun usage-and-exit (exit-code)
  (usage)
  (uiop:quit exit-code))

(defun make-project (path cl-project:*skeleton-directory*)
  (cl-project:make-project path))

(defun init-project (args &key (type "node"))
  (unless (alexandria:length= 1 args)
    (usage-and-exit 1))
  (destructuring-bind (name) args
    (make-project (merge-pathnames name (probe-file "."))
                  (alexandria:eswitch (type :test #'string=)
                    ("browser"
                     (asdf:system-relative-pathname :valtan #p"skeleton/browser-skeleton/"))
                    ("node"
                     (asdf:system-relative-pathname :valtan #p"skeleton/node-skeleton/"))))))

(defun main (args)
  (handler-case (command-line-arguments:handle-command-line
                 +option-spec+
                 #'init-project
                 :command-line args
                 :rest-arity t)
    (error ()
      (usage-and-exit 1))))
