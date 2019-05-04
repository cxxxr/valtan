(ffi:console.log "hello world")

(dotimes (i 10)
  (ffi:console.log i))

(defun fact (n)
  (cond ((= n 0) 1)
        (t (* n (fact (- n 1))))))

(ffi:console.log (fact 5))

(ffi:console.log (make-array 3))
(ffi:console.log (make-array 3 :initial-element 100))

(ffi:console.log "==================== cons ====================")
(ffi:console.log (cons 1 2))
(ffi:console.log (car (cons 1 2)))
(ffi:console.log (cdr (cons 1 2)))
(ffi:console.log (consp (cons 1 2)))
(let ((x (cons 1 2)))
  (ffi:console.log (rplaca x 100))
  (ffi:console.log (rplacd x 200)))

(ffi:console.log "==================== &rest ====================")
(defun f1 (&rest args)
  (ffi:console.log args))

(f1 1 2 3)

(defun f2 (x y &rest z)
  (ffi:console.log x y z))

(f2 1 2)
(f2 1 2 3)
(f2 1 2 3 4)
