(ffi:require js:readline-sync "readline-sync")

(let ((system::*get-stdin-line-function*
        (lambda ()
          (let ((x (funcall js:readline-sync.question)))
            (ffi:js->cl x)))))
  (do () (nil)
    (print (eval (read)))
    (terpri)))
