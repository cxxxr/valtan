(defsystem "clscript"
  :serial t
  :components ((:module "lisp"
                :components
                ((:file "packages")
                 (:file "host")
                 (:file "error")
                 (:file "ir")
                 (:file "pass1")
                 (:file "pass2")
                 (:file "compiler")))))
