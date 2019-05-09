(ffi:console.log "hello world")
(ffi:console.log (list 1 2 3))

(defun f1 (&key (foo 100))
  foo)

(ffi:console.log (f1))
(ffi:console.log (f1 :foo 0))

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

(ffi:console.log "==================== defstruct ====================")
(defstruct foo
  x y z)
(ffi:console.log (make-foo))
(ffi:console.log (make-foo :x 10 :y 20 :z 30))
(ffi:console.log (make-foo :z 10 :x 20))
(ffi:console.log (make-foo :y 100))
(let ((foo (make-foo :x 12345 :y 200 :z 'foo)))
  (ffi:console.log (foo-x foo))
  (ffi:console.log (foo-y foo))
  (ffi:console.log (foo-z foo)))

(defstruct bar
  (x 100))
(ffi:console.log (make-bar))
(ffi:console.log (make-bar :x 0))

(defstruct (hoge (:constructor %make-hoge))
  x
  y
  z)
(ffi:console.log (%make-hoge :x 1 :y 2 :z 3))

(defstruct (piyo (:constructor %make-hoge (x y z)))
  x
  y
  z)
(ffi:console.log (%make-hoge 100 200 300))

(ffi:console.log "==================== Symbol ====================")
(ffi:console.log (symbol-plist 'foo))
