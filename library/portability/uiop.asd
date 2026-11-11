;; Runtime utilities only; the compiler keeps its own host ASDF/UIOP registry.
(asdf:defsystem "uiop"
  :version "3.3.7.4"
  :description "UIOP portable runtime utilities for Valtan"
  :license "MIT"
  :depends-on ("trivial-features")
  :serial t
  :components ((:module "uiop" :serial t
                :components ((:file "package") (:file "common-lisp")
                             (:file "utility") (:file "version") (:file "os")
                             (:file "valtan-os") (:file "pathname")
                             (:file "filesystem") (:file "stream")
                             (:file "valtan-filesystem")
                             (:file "launch-program") (:file "run-program")
                             (:file "valtan-run-program") (:file "driver")))))
