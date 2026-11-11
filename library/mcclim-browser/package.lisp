(defpackage #:mcclim-browser
  (:use #:clim #:clim-lisp #:clim-backend #:clim-extensions)
  (:export #:browser-port #:browser-medium #:browser-frame-manager #:ready))
(in-package #:mcclim-browser)

(defun host-call (operation &rest arguments)
  ;; Host functions return one JavaScript value. Establish that value explicitly:
  ;; a tail FFI call otherwise leaves the Lisp multiple-value register unchanged,
  ;; which method combination can observe instead of the returned surface.
  (values (apply (ffi:aget (ffi:ref "globalThis.valtanMcclim") (ffi:cl->js operation))
                 arguments)))
(defun property (object name)
  (let ((value (ffi:aget object (ffi:cl->js name))))
    (unless (or (eq value (ffi:ref "undefined")) (eq value (ffi:ref "null")))
      (ffi:js->cl value))))
(defun ready () (host-call "ready"))
