(in-package :cl-user)

(defun perf-sum (n)
  (let ((sum 0)) (dotimes (i n sum) (incf sum i))))

(defun perf-fib (n)
  (if (< n 2) n (+ (perf-fib (- n 1)) (perf-fib (- n 2)))))

(defun perf-list (n)
  (let ((items nil) (sum 0))
    (dotimes (i n) (push i items))
    (dolist (x items sum) (incf sum x))))

(defun perf-vector (n)
  (let ((v (make-array n :initial-element 0)) (sum 0))
    (dotimes (i n) (setf (aref v i) i))
    (dotimes (i n sum) (incf sum (aref v i)))))

(defun perf-closure (n)
  (let* ((sum 0) (add (lambda (x) (incf sum x))))
    (dotimes (i n sum) (funcall add i))))

(defun perf-hash (n)
  (let ((table (make-hash-table)) (sum 0))
    (dotimes (i n) (setf (gethash i table) i))
    (dotimes (i n sum) (incf sum (gethash i table)))))

(defun perf-string (n)
  (let ((s (make-string n :initial-element #\a)) (count 0))
    (dotimes (i n count) (when (char= (char s i) #\a) (incf count)))))

(defclass perf-box () ((value :initarg :value :reader perf-value)))
(defmethod perf-increment ((box perf-box)) (1+ (perf-value box)))
(defun perf-clos (n)
  (let ((box (make-instance 'perf-box :value 3)) (sum 0))
    (dotimes (i n sum) (incf sum (perf-increment box)))))

(funcall (ffi:ref "globalThis" "valtanBenchmark") #j"sum" #'perf-sum 10000 49995000)
(funcall (ffi:ref "globalThis" "valtanBenchmark") #j"fib" #'perf-fib 20 6765)
(funcall (ffi:ref "globalThis" "valtanBenchmark") #j"list" #'perf-list 10000 49995000)
(funcall (ffi:ref "globalThis" "valtanBenchmark") #j"vector" #'perf-vector 10000 49995000)
(funcall (ffi:ref "globalThis" "valtanBenchmark") #j"closure" #'perf-closure 10000 49995000)
(funcall (ffi:ref "globalThis" "valtanBenchmark") #j"hash" #'perf-hash 1000 499500)
(funcall (ffi:ref "globalThis" "valtanBenchmark") #j"string" #'perf-string 10000 10000)
(funcall (ffi:ref "globalThis" "valtanBenchmark") #j"clos" #'perf-clos 100 400)
