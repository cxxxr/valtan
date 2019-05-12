(ffi:console.log "hello world")
(ffi:console.log (list 1 2 3))

(defun f1 (&key (foo 100))
  foo)

(ffi:console.log (f1))
(ffi:console.log (f1 :foo 0))

(dotimes (i 10)
  (ffi:console.log i))

(dolist (i (list 1 2 3))
  (ffi:console.log i))

(dolist (x (list 'foo 'bar 'baz))
  (ffi:console.log "case" x
                   (case x
                     (foo 100)
                     (bar 200)
                     (otherwise 300))))

(dolist (k '(1 2 3 :four v () t 'other))
  (ffi:console.log
   (case k ((1 2) 'clause1)
         (3 'clause2)
         (nil 'no-keys-so-never-seen)
         ((nil) 'nilslot)
         ((:four v) 'clause4)
         ((t) 'tslot)
         (otherwise 'others))))

(defun fact (n)
  (cond ((= n 0) 1)
        (t (* n (fact (- n 1))))))

(ffi:console.log (fact 5))

(ffi:console.log (make-array 3))
(ffi:console.log (make-array 3 :initial-element 100))

(ffi:console.log "==================== JS Array ====================")
(let ((x (ffi:new (ffi:ref "Array") 10)))
  (ffi:console.log x)
  (ffi:set (ffi:index x 0) "a")
  (ffi:console.log (ffi:index x 0)))

(ffi:console.log "==================== cons ====================")
(ffi:console.log (cons 1 2))
(ffi:console.log (car (cons 1 2)))
(ffi:console.log (cdr (cons 1 2)))
(ffi:console.log (consp (cons 1 2)))
(let ((x (cons 1 2)))
  (ffi:console.log (rplaca x 100))
  (ffi:console.log (rplacd x 200)))
(let ((x (cons 1 2)))
  (setf (car x) 849213482)
  (ffi:console.log x))
(ffi:console.log (list* 1))
(ffi:console.log (list* 1 2))
(ffi:console.log (list* 1 2 3))
(ffi:console.log "member" (member 2 (list 1 2 3)))

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

(defstruct (piyo (:constructor %make-piyo (x y z)))
  x
  y
  z)
(let ((piyo (%make-piyo 100 200 300)))
  (ffi:console.log piyo)
  (setf (piyo-x piyo) 'test)
  (ffi:console.log piyo (piyo-x piyo)))

(let ((piyo (%make-piyo 1 2 3)))
  (ffi:console.log (eq piyo piyo))
  (ffi:console.log (eq piyo (copy-structure piyo)))
  (ffi:console.log (eq piyo (copy-piyo piyo)))
  (ffi:console.log (piyo-p piyo)))

(ffi:console.log "==================== Symbol ====================")
(ffi:console.log (symbol-plist 'foo))
(system::put-symbol-plist 'foo (list 'a 1 'b 2))
(ffi:console.log (symbol-plist 'foo))
(setf (get 'aaa 'key1) 100)
(ffi:console.log (get 'aaa 'key1))

(ffi:console.log "==================== Hash Table ====================")
(let ((x (make-hash-table)))
  (ffi:console.log x)
  (ffi:console.log (hash-table-p x))
  (ffi:console.log (hash-table-count x))
  (ffi:console.log (gethash "key1" x))
  (ffi:console.log (setf (gethash "key1" x) "value"))
  (ffi:console.log (gethash "key1" x)))
