(pushnew :fixture-asdf-feature *features*)
(asdf:defsystem "fixture-utils"
  :version "1.0"
  :depends-on ("net.didierverna.asdf-flv"
               (:feature (:not :fixture-no-optional-backend) "missing-optional-backend"))
  :components ((:module "src" :serial t
                :components ((:file "definitions")
                             (:static-file "MixedCase.dat")
                             (:file "file-local-first") (:file "file-local-second")
                             (:file "reader")
                             (:file "host-only" :if-feature :sbcl)
                             (:file "optional-backend" :if-feature (:not :fixture-no-optional-backend))))))
(asdf:defsystem "fixture-utils/sub"
  :depends-on ((:version "fixture-utils" "1.0"))
  :components ((:file "sub")))
