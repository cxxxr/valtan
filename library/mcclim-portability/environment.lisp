(in-package #:clim-internals)

;; Install after the upstream core, before extensions whose load-time defaults
;; consult the environment (for example PostScript's document author).
(defun get-environment-variable (name)
  (let ((value (system:environment-op :getenv (ffi:cl->js name))))
    (unless (null value) (ffi:js->cl value))))
