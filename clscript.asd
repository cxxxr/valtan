(defsystem "clscript"
  :serial t
  :components ((:module "lisp"
                :components
                ((:file "host")
                 (:file "compiler")))))
