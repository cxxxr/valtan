(defsystem "lem-valtan"
  :depends-on ("valtan" "trivial-ws")
  :serial t
  :components ((:file "remote-eval")
               (:file "valtan-mode")
               (:file "main")))
