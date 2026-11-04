(in-package :cl-test)

(deftest logarithm-base
  (values (< (abs (- (log 8 2) 3)) 0.000000000001) (= (log 8 0) 0))
  t t)

(deftest logarithm-large-integer
  (< (abs (- (log (ash 1 2000) 2) 2000)) 0.000000001)
  t)

(deftest logarithm-negative-real
  (let ((result (log -1)))
    (values (complexp result) (= (realpart result) 0)
            (< 3.14 (imagpart result) 3.15)))
  t t t)

(deftest abs-exact-integer
  (abs -123456789012345678901234567890)
  123456789012345678901234567890)

(deftest abs-float-type
  (let ((result (abs -3.5))) (values (= result 3.5) (floatp result)))
  t t)

(deftest abs-complex-magnitude (= (abs #c(3 4)) 5) t)

(deftest abs-type-error
  (handler-case (abs "invalid") (type-error () :rejected))
  :rejected)

(deftest ratios-normalize
  (values (/ 12 4) (/ -18 24) (/ 5 -15) (/ 0 9)
          (numerator (/ 18 -24)) (denominator (/ 18 -24)))
  3 -3/4 -1/3 0 -3 4)

(deftest ratios-arithmetic
  (values (+ 1/3 1/6) (- 2/3 1/2) (* 21/22 11/7) (/ 3/4 5/6) (abs -1/7))
  1/2 1/6 3/2 9/10 1/7)

(deftest ratios-exact-large
  (let* ((n (ash 1 200)) (r (/ (1+ n) n)))
    (values (> r 1) (< r (+ 1 (/ 2 n))) (= (* r n) (1+ n))
            (rationalp r) (type-of r)))
  t t t t ratio)

(deftest ratios-float-contagion
  (values (floatp (+ 1/3 0.5)) (= (+ 1/2 0.5) 1) (eql 1/2 0.5)
          (floatp (float 1/3)) (< (abs (- (float 1/3) 0.3333333333333333)) 1e-15))
  t t nil t t)

(deftest rational-exact-float
  (values (rational 0.5) (rational 0.1)
          (= (float (rational 0.1)) 0.1)
          (= 1/10 0.1))
  1/2 3602879701896397/36028797018963968 t nil)

(deftest ratios-compiled-literals
  (values (funcall (compile nil '(lambda () 123456789012345678901/7)))
          (funcall (compile nil '(lambda () #c(1/3 2/7)))))
  123456789012345678901/7 #c(1/3 2/7))

(deftest ratios-printer-reader
  (values (write-to-string 5/8) (write-to-string 5/8 :radix t)
          (write-to-string 10/17 :base 16 :radix t)
          (read-from-string "#xA/11"))
  "5/8" "#10r5/8" "#xA/11" 10/17)

(deftest ratios-rounded-division
  (values (multiple-value-list (floor -7/3 2/3))
          (multiple-value-list (ceiling -7/3 2/3))
          (multiple-value-list (truncate -7/3 2/3))
          (multiple-value-list (round -7/3 2/3))
          (mod -7/3 2/3) (rem -7/3 2/3))
  (-4 1/3) (-3 -1/3) (-3 -1/3) (-4 1/3) 1/3 -1/3)

(deftest complex-exact-arithmetic
  (values (+ #c(1/3 2/5) #c(2/3 -2/5))
          (* #c(1 2) #c(3 4)) (/ 2 #c(2 2))
          (- #c(1/3 -2/7)) (conjugate #c(3/4 1/2)))
  1 #c(-5 10) #c(1/2 -1/2) #c(-1/3 2/7) #c(3/4 -1/2))

(deftest complex-float-contagion
  (let ((x (* #c(1.0 2.0) #c(3 4))))
    (values (eql x #c(-5.0 10.0)) (floatp (realpart x))
            (complexp (- x x)) (eql #c(1 2) #c(1.0 2.0))
            (eql (imagpart 3.0) 0.0)))
  t t t nil t)

(deftest numeric-pairwise-inequality
  (values (/= 1 2 1) (/= 1 2 3) (/= 1/2 0.5) (/= #c(1 2) #c(1.0 2.0)))
  nil t nil nil)

(deftest integer-negative-powers
  (values (expt 2 -10) (expt 1/2 -3) (expt #c(0 1) 4)
          (eql (expt 2.0 0) 1.0) (read-from-string "1e-3"))
  1/1024 8 1 t 0.001)

(deftest numeric-real-type-errors
  (values (handler-case (complex #c(1 2)) (type-error () :rejected))
          (handler-case (< #c(1 2)) (type-error () :rejected))
          (handler-case (floor #c(1 2)) (type-error () :rejected))
          (handler-case (numerator 0.5) (type-error () :rejected))
          (handler-case (realpart 'a) (type-error () :rejected)))
  :rejected :rejected :rejected :rejected :rejected)

(deftest numeric-binary64-constants
  (values (= most-positive-double-float most-positive-single-float)
          (floatp least-positive-short-float)
          (> least-positive-double-float 0)
          (= (float (rational least-positive-double-float)) least-positive-double-float)
          (> (+ 1.0 double-float-epsilon) 1.0)
          (< (- 1.0 double-float-negative-epsilon) 1.0))
  t t t t t t)

(deftest integer-boolean-operations
  (values (logior 1 2 4) (logxor 7 3) (logeqv) (logcount -9)
          (logbitp most-positive-fixnum -1)
          (boole boole-andc1 3 15) (boole boole-orc2 3 15)
          (gcd -24 36 60) (lcm -6 15) (gcd) (lcm)
          (logand (1+ (ash 1 100)) 1)
          (= (logior (ash 1 100) 1) (1+ (ash 1 100)))
          (= (funcall (compile nil '(lambda (x) (logand x -1)))
                      (ash 1 100)) (ash 1 100)))
  7 4 -1 1 t 12 -13 12 30 0 1 1 t t)

(deftest float-rounded-division
  (values (multiple-value-list (ffloor -7/3 2/3))
          (multiple-value-list (fceiling -7/3 2/3))
          (multiple-value-list (ftruncate -7/3 2/3))
          (multiple-value-list (fround -7/3 2/3))
          (handler-case (/ 1 0) (division-by-zero () :rejected)))
  (-4.0 1/3) (-3.0 -1/3) (-3.0 -1/3) (-4.0 1/3) :rejected)

(deftest binary64-decomposition
  (values (multiple-value-list (integer-decode-float 1.0))
          (multiple-value-list (decode-float -0.5))
          (multiple-value-list (integer-decode-float least-positive-double-float))
          (float-precision 0.0) (float-precision least-positive-double-float)
          (float-digits 1.0) (float-radix 1.0))
  (4503599627370496 -52 1) (0.5 0 -1.0) (1 -1074 1) 0 1 53 2)

(deftest binary64-scaling
  (values (scale-float 0.5 1024) (scale-float 2.0 -1075)
          (float-sign -0.0 2.0)
          (handler-case (scale-float 1.0 1024) (floating-point-overflow () :rejected)))
  8.98846567431158d307 4.9406564584124654d-324 -2.0 :rejected)

(deftest floating-signed-zero-equality
  (values (eql 0.0 -0.0) (= 0.0 -0.0)
          (eql #c(1.0 -0.0) #c(1.0 0.0))
          (float-sign (abs -0.0)) (float-sign (imagpart -3.0)))
  nil t nil 1.0 -1.0)
