(asdf:defsystem "bordeaux-threads"
  :description "Bordeaux Threads 0.9.4 with Valtan cooperative threads"
  :version "0.9.4"
  :license "MIT"
  :depends-on ("alexandria" "global-vars" "trivial-features" "trivial-garbage")
  :pathname "bordeaux-threads/"
  :serial t
  :components ((:file "initialize")
               (:file "upstream/apiv1/pkgdcl")
               (:file "upstream/apiv1/bordeaux-threads")
               (:file "native-v1")
               (:file "upstream/apiv1/default-implementations")
               (:file "upstream/apiv2/pkgdcl")
               (:file "upstream/apiv2/bordeaux-threads")
               (:file "upstream/apiv2/timeout-interrupt")
               (:file "native-v2")
               (:file "atomics")
               (:file "upstream/apiv2/api-locks")
               (:file "upstream/apiv2/api-threads")
               (:file "upstream/apiv2/api-semaphores")
               (:file "upstream/apiv2/api-condition-variables")
               (:static-file "upstream/LICENSE")
               (:static-file "upstream/version.sexp")
               (:static-file "upstream/source.json")))

(asdf:defsystem "bordeaux-threads/test"
  :depends-on ("bordeaux-threads" "fiveam")
  :pathname "bordeaux-threads/upstream/test/"
  :serial t
  :components ((:file "tests-v1") (:file "pkgdcl")
               (:file "not-implemented") (:file "tests-v2")))
