(ffi:require js:readline-sync "readline-sync")

(let ((system::*get-stdin-line-function*
        (lambda ()
          (js:readline-sync.question))))
  (do () (nil)
    (print (eval (read)))
    (terpri)))
