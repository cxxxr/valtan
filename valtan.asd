(defsystem "valtan"
  :depends-on ("cl-ppcre"
               "trivial-gray-streams"
               "cl-source-map"
               #+linux "inotify"
               "async-process"
               "st-json"
               "valtan-core")
  :pathname "host-src"
  :serial t
  :components ((:file "emitter-stream")
               (:file "util")
               (:file "reader")
               (:file "system")
               (:file "build")
               (:file "valtan-host")))
