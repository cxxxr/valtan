(defpackage :valtan-cli.build-project
  (:use :cl)
  (:export :main))
(in-package :valtan-cli.build-project)

(defun usage ()
  (write-line "usage: valtan build <system-file>"))

(defun usage-and-exit (exit-code)
  (usage)
  (uiop:quit exit-code))

(defun prepare-path-file ()
  (with-open-file (out ".valtan-path"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ (asdf:system-relative-pathname :valtan "./kernel/") out)))

(defun main (args)
  (unless (= 1 (length args))
    (usage-and-exit 1))
  (prepare-path-file)
  (valtan-host.build:build-system (first args)))
