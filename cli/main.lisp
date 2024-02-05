(defpackage :valtan-cli
  (:use :cl)
  (:export :main))
(in-package :valtan-cli)

(defun usage ()
  (write-line "usage: valtan <command> [<args>]")
  (write-line "<comman>:")
  (write-line "  init")
  (write-line "  build"))

(defun usage-and-exit (exit-code)
  (usage)
  (uiop:quit exit-code))

(defun build-project (args)
  args)

(defun main (&optional (args (uiop:command-line-arguments)))
  (when (= 0 (length args))
    (usage-and-exit 1))
  (alexandria:switch ((first args) :test #'string=)
    ("init" (valtan-cli.init-project:main (rest args)))
    ("build" (valtan-cli.build-project:main (rest args)))
    (otherwise (usage-and-exit 1))))
