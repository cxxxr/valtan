(defsystem "valtan"
  :depends-on ("cl-ppcre"
               #+linux "inotify"
               "remote-js"
               "valtan-core")
  :components ((:module "target"
                :serial t
                :pathname "library/valtan-core/compiler"
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
                             #+(or)(:file "flow-graph")
                             (:file "compiler")))
               (:module "host"
                :pathname "host-src"
                :serial t
                :components ((:file "util")
                             (:file "host-reader")
                             (:file "system")
                             (:file "build")
                             (:file "remote-eval")
                             (:file "valtan-host")))))
