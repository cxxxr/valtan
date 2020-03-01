(defsystem "valtan"
  :depends-on ("cl-ppcre"
               #+linux "inotify"
               "remote-js"
               "valtan-core")
  :pathname "host-src"
  :serial t
  :components ((:file "util")
               (:file "host-reader")
               (:file "system")
               (:file "build")
               (:file "remote-eval")
               (:file "valtan-host")))
