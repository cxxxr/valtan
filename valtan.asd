(pushnew :valtan.pass2-new *features*)

(defsystem "valtan"
  :serial t
  :components ((:module "compiler"
                :components
                ((:file "packages")
                 (:file "variables")
                 (:file "util")
                 (:file "error")
                 (:file "hir")
                 (:file "hir-walker")
                 (:file "pass1")
                 (:file "hir-optimize")
                 #-valtan.pass2-new (:file "pass2")
                 (:file "pass2-new")
                 ;; (:file "hir-to-lir")
                 ;; (:file "flow-graph")
                 (:file "compiler")))))
