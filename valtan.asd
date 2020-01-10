(defsystem "valtan"
  :serial t
  :components ((:module "compiler"
                :pathname "library/valtan/compiler/"
                :components
                ((:file "packages")
                 (:file "variables")
                 (:file "util")
                 (:file "error")
                 (:file "hir")
                 (:file "pass1")
                 (:file "hir-walker")
                 (:file "type-infer")
                 (:file "hir-optimize")
                 (:file "pass2")
                 ;; (:file "hir-to-lir")
                 ;; (:file "flow-graph")
                 (:file "compiler")))))
