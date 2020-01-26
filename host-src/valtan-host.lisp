(defpackage :valtan-host
  (:use :cl
        :valtan-host.util
        :valtan-host.build)
  (:export :js-beautify
           :with-js-beautify
           :build-system
           :build-application
           :run-build-server
           :start-remote-eval-server))
(in-package :valtan-host)

(defpackage :valtan-user (:use :cl))

(defun start-remote-eval-server ()
  (valtan-host.remote-eval:start))
