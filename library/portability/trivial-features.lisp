(in-package :cl-user)
;;; Detect the JavaScript target, never the SBCL host that built the bundle.
(eval-when (:load-toplevel :execute)
  (let ((features (system:environment-op :features)))
    (dotimes (i (ffi:ref features "length"))
      (pushnew (intern (ffi:js->cl (ffi:aget features i)) :keyword) *features*))))
