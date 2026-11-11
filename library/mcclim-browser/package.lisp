(defpackage #:mcclim-browser
  (:use #:clim #:clim-lisp #:clim-backend #:clim-extensions)
  (:export #:browser-port #:browser-medium #:browser-frame-manager #:ready))
(in-package #:mcclim-browser)

(defun host-call (operation &rest arguments)
  (apply (ffi:aget (ffi:ref "globalThis.valtanMcclim") (ffi:cl->js operation))
         arguments))
(defun property (object name)
  (let ((value (ffi:aget object (ffi:cl->js name))))
    (unless (or (eq value (ffi:ref "undefined")) (eq value (ffi:ref "null")))
      (ffi:js->cl value))))
(defun ready () (host-call "ready"))
