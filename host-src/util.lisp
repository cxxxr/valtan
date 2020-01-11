(defpackage :valtan-host.util
  (:use :cl)
  (:export :js-beautify
           :with-js-beautify))
(in-package :valtan-host.util)

(defun js-beautify (text &optional (output *standard-output*))
  (with-input-from-string (in text)
    (uiop:run-program "js-beautify"
                      :input in
                      :output output)))

(defmacro with-js-beautify (&body body)
  `(let ((output
           (with-output-to-string (*standard-output*)
             (progn ,@body))))
     (js-beautify output)))
