(defpackage :valtan-cli.build-server
  (:use :cl)
  (:export :main))
(in-package :valtan-cli.build-server)

(defun usage ()
  (write-line "usage: valtan build-server <system-file>"))

(defun usage-and-exit (exit-code)
  (usage)
  (uiop:quit exit-code))

(defun main (args)
  (unless (= 1 (length args))
    (usage-and-exit 1))
  (valtan-host:run-build-server (first args)))
