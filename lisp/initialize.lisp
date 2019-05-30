(in-package :common-lisp)

(make-package "COMMON-LISP" :nicknames '("CL"))
(make-package "SYSTEM")
(make-package "FFI")
(make-package "KEYWORD")
(let ((cl-user (make-package "COMMON-LISP-USER" :nicknames '("CL-USER"))))
  (defvar *package* cl-user))

(defvar t 't)
(defvar nil 'nil)
