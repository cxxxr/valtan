(in-package :cl-user)

;; Run outside the timed region, with values that cannot be constant folded.
(defun perf-check-numeric (x y)
  (list (+ x y) (- x y) (* x y) (/ x y)
        (= x y) (/= x y) (< x y) (> x y) (<= x y) (>= x y)
        (1+ x) (1- x) (zerop x)))
(assert (equal (perf-check-numeric 6 2) '(8 4 12 3 nil t nil t nil t 7 5 nil)))
(assert (equal (perf-check-numeric 0 2) '(2 -2 0 0 nil t t nil t nil 1 -1 t)))
(assert (equal (subseq (perf-check-numeric 1 2) 0 4) '(3 -1 2 1/2)))
(assert (equal (subseq (perf-check-numeric 9007199254740991 2) 0 3)
               '(9007199254740993 9007199254740989 18014398509481982)))
(assert (floatp (+ (identity 1.0) 2)))
(assert (= (+ (identity (complex 1 2)) 3) (complex 4 2)))

(let ((x 1)) (assert (= (+ x (setq x 4)) 5)))
(let ((x 1)) (assert (= (- x (setq x 4)) -3)))
(let ((x 1)) (assert (< x (setq x 4))))
(let ((seen nil))
  (handler-case (+ (identity 'bad) (progn (setq seen t) 2))
    (type-error () nil))
  (assert seen))
(assert (handler-case (progn (+ (identity 'bad) 2) nil) (type-error () t)))
(assert (handler-case (progn (< (identity (complex 1 2)) 2) nil) (type-error () t)))
(assert (handler-case (progn (/ (identity 1) 0) nil) (division-by-zero () t)))
(assert (handler-case (progn (1+ (identity 1) 2) nil) (program-error () t)))
(assert (handler-case (progn (<) nil) (program-error () t)))
(assert (= (+) 0))
(assert (= (*) 1))
(assert (= (- (identity 3)) -3))
(assert (= (+ (identity 1) 2 3) 6))
(flet ((+ (a b) (list a b))) (assert (equal (+ 1 2) '(1 2))))
(assert (null (car nil)))
(assert (null (cdr nil)))
(assert (handler-case (progn (car (identity 1)) nil) (type-error () t)))
(assert (handler-case (progn (cdr (identity 1)) nil) (type-error () t)))
(assert (char= (identity #\a) #\a))
(assert (not (char= (identity #\a) #\b)))
(assert (handler-case (progn (char= (identity 1) #\a) nil) (type-error () t)))
(assert (handler-case (progn (char= #\a (identity 1)) nil) (type-error () t)))
(assert (null (not (identity 0))))
(assert (atom (identity nil)))
(assert (listp (identity nil)))
(assert (not (listp (identity 1))))
(assert (handler-case (progn (setf (elt (copy-seq "012") 'index) #\a) nil)
          (type-error () t)))
(assert (handler-case (progn (setf (elt (copy-seq "012") "xyz") #\a) nil)
          (type-error () t)))

;; Values saved across cleanup must not alias the single-value register.
(assert (equal (multiple-value-list
                (unwind-protect (values 1 2 3) (+ (identity 8) 9))) '(1 2 3)))
(assert (equal (multiple-value-list
                (unwind-protect (+ (identity 1) 2) (values 8 9))) '(3)))
(assert (equal (multiple-value-list
                (unwind-protect (values) (+ (identity 8) 9))) nil))
(assert (equal (multiple-value-call #'list
                (+ (identity 1) 2) (values 4 5) (values) (1+ (identity 5))) '(3 4 5 6)))
(assert (equal (multiple-value-list
                (multiple-value-prog1 (+ (identity 1) 2) (+ (identity 5) 6))) '(3)))

;; The runtime compiler must use the same lowering and preserve evaluation order.
(assert (= (funcall (compile nil '(lambda (x) (+ x (setq x 4)))) 1) 5))
(assert (equal (multiple-value-list
                (funcall (compile nil '(lambda (x) (unwind-protect (values x 2) (+ x 9)))) 1))
               '(1 2)))
