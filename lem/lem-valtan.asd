(defsystem "lem-valtan"
  :depends-on ("lem" "valtan" "trivial-ws")
  :serial t
  :components ((:file "remote-eval")
               (:file "valtan-mode")
               (:file "main")))
