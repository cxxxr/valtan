(defsystem "valtan-cli"
  :depends-on ("valtan"
               "command-line-arguments"
               "cl-project")
  :serial t
  :pathname "cli"
  :components ((:file "init-project")
               (:file "build-project")
               (:file "main")))

(defsystem "valtan-cli/executable"
  :build-operation program-op
  :build-pathname "valtan"
  :entry-point "valtan-cli:main"
  :depends-on ("valtan-cli"))
