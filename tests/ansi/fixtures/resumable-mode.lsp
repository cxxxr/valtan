(in-package :cl-user)
(setf compiler::*resumable-functions* t)
;; Fail preparation if this worker silently uses the ordinary calling path.
(assert (functionp
         ((ffi:ref "lisp" "getResumableImplementation")
          (compile nil '(lambda () :resumable-probe)))))
