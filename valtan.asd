(defsystem "valtan"
  :depends-on ("cl-ppcre"
               "trivial-gray-streams"
               "cl-source-map"
               #+linux "inotify"
               "trivial-ws"
               "async-process"
               "valtan-core")
  :pathname "host-src"
  :serial t
  :components ((:file "emitter-stream")
               (:file "util")
               (:file "reader")
               (:file "system")
               (:file "build")
               (:file "remote-eval")
               (:file "valtan-host")))
