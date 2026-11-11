(asdf:defsystem "mcclim-browser"
  :description "McCLIM Canvas backend for Valtan browser workers"
  :depends-on ("clim")
  :serial t
  :components ((:file "package") (:file "port") (:file "medium")))
