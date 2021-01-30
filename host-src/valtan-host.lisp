(defpackage :valtan-host
  (:use :cl
        :valtan-host.util
        :valtan-host.build)
  (:export :js-beautify
           :with-js-beautify
           :build-system
           :build-application
           :run-build-server
           :start))
(in-package :valtan-host)

(defpackage :valtan-user (:use :cl))

(defun start (system)
  (valtan-host:build-application system :force t)
  (valtan-host.remote-eval:start)
  (valtan-host.remote-eval:repl))
