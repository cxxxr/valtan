(in-package :cl-user)
(setf compiler::*resumable-functions* :dual)
(assert (functionp
         ((ffi:ref "lisp" "getResumableImplementation")
          (compile nil '(lambda () :dual-probe)))))
