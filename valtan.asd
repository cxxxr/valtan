#+linux
(pushnew :valtan.remote-eval *features*)

(defsystem "valtan"
  :depends-on ("cl-ppcre"
               "trivial-gray-streams"
               "cl-source-map"
               #+valtan.remote-eval "inotify"
               #+valtan.remote-eval "remote-js"
               "valtan-core")
  :pathname "host-src"
  :serial t
  :components ((:file "emitter-stream")
               (:file "util")
               (:file "host-reader")
               (:file "system")
               (:file "build")
               #+valtan.remote-eval (:file "remote-eval")
               (:file "valtan-host")))
