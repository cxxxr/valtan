(ffi:require js:readline-sync "readline-sync")

(let ((*:*get-stdin-line-function*
        (lambda ()
          (ffi:js->cl (funcall js:readline-sync.question)))))
  (do ()
      (nil)
    (handler-case (print (eval (read)))
      (error (e) (princ e)))
    (terpri)))
