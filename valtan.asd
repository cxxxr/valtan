(defsystem "valtan"
  :serial t
  :components ((:module "compiler"
                :components
                ((:file "packages")
                 (:file "variables")
                 (:file "util")
                 (:file "error")
                 (:file "hir")
                 (:file "pass1")
                 (:file "pass2")
                 (:file "lir")
                 (:file "hir-to-lir")
                 (:file "compiler")))))
