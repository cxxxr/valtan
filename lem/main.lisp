(defpackage :lem-valtan/main
  (:use :cl
        :valtan-host.util
        :valtan-host.build)
  (:export :start))
(in-package :lem-valtan/main)

(defpackage :valtan-user (:use :cl))

(defun start (system &key force)
  (valtan-host.build:build-application system :force force)
  (lem-valtan/remote-eval:start)
  (unwind-protect (lem-valtan/remote-eval:repl)
    (lem-valtan/remote-eval:stop)))
