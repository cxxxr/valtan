(defsystem "valtan"
  :components ((:module "target"
                :serial t
                :pathname "library/valtan/compiler"
                :components ((:file "packages")
                             (:file "variables")
                             (:file "util")
                             (:file "error")
                             (:file "hir")
                             (:file "pass1")
                             (:file "hir-walker")
                             (:file "type-infer")
                             (:file "hir-optimize")
                             (:file "pass2")
                             #+(or)(:file "hir-to-lir")
                             #+(or)(:file "flow-graph")))
               (:module "host"
                :pathname "host-src"
                :serial t
                :components ((:file "read")
                             (:file "compiler")))))
