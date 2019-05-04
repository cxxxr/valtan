(ffi:console.log "hello world")

(dotimes (i 10)
  (ffi:console.log i))

(defun fact (n)
  (cond ((= n 0) 1)
        (t (* n (fact (- n 1))))))

(ffi:console.log (fact 5))

(ffi:console.log (make-array 3))
(ffi:console.log (make-array 3 :initial-element 100))
