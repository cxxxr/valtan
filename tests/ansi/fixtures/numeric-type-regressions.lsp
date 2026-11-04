(in-package :cl-test)

(deftest numeric-types-open-and-closed-float-bounds
  (values (multiple-value-list (subtypep '(float 0.0 (1.0)) '(float 0.0 1.0)))
          (multiple-value-list (subtypep '(float 0.0 1.0) '(float 0.0 (1.0))))
          (multiple-value-list (subtypep '(single-float 1.0 2.0) '(double-float 0.0 3.0))))
  (t t) (nil t) (t t))

(deftest numeric-types-signed-zero-interval-and-eql
  (values (multiple-value-list (subtypep '(float 0.0 0.0) '(member -0.0 0.0)))
          (multiple-value-list (subtypep '(float 0.0 0.0) '(eql 0.0)))
          (multiple-value-list (subtypep '(eql -0.0) '(float 0.0 0.0))))
  (t t) (nil t) (t t))

(deftest numeric-types-adjacent-floats-have-no-interior
  (values (multiple-value-list (subtypep '(float (1.0d0) (1.0000000000000002d0)) nil))
          (multiple-value-list (subtypep '(float (1.0d0) 1.0000000000000002d0)
                                        '(eql 1.0000000000000002d0))))
  (t t) (t t))

(deftest numeric-types-rational-singletons-and-dense-intervals
  (values (multiple-value-list (subtypep '(rational 2 2) 'integer))
          (multiple-value-list (subtypep '(rational (2) (3)) 'ratio))
          (multiple-value-list (subtypep '(rational 2 3) 'integer))
          (multiple-value-list (subtypep '(and ratio (real 2 2)) nil)))
  (t t) (t t) (nil t) (t t))

(deftest numeric-types-intersection-and-union-coverage
  (values (multiple-value-list (subtypep '(and (real 0 8) (real 3 10)) '(real 3 8)))
          (multiple-value-list (subtypep '(real 0 10) '(or (real 0 (5)) (real 5 10))))
          (multiple-value-list (subtypep '(real 0 10) '(or (real 0 (5)) (real (5) 10)))))
  (t t) (t t) (nil t))

(deftest numeric-types-integer-rounding-and-empty-range
  (values (multiple-value-list (subtypep '(integer -1 1) '(real -3/2 3/2)))
          (multiple-value-list (subtypep '(and integer (real (1) (2))) nil))
          (multiple-value-list (subtypep '(and integer (real -3/2 3/2)) '(integer -1 1))))
  (t t) (t t) (t t))

(deftest numeric-types-bignum-and-unbounded-domain
  (values (multiple-value-list (subtypep 'bignum 'integer))
          (multiple-value-list (subtypep 'bignum 'fixnum))
          (multiple-value-list (subtypep '(or fixnum bignum) 'integer))
          (multiple-value-list (subtypep 'integer '(or fixnum bignum))))
  (t t) (nil t) (t t) (t t))

(deftest numeric-types-exact-rational-float-boundaries
  (values (multiple-value-list (subtypep '(float 1/10 1/10) nil))
          (multiple-value-list (subtypep '(eql 0.1d0) '(real 1/10 1/10)))
          (multiple-value-list (subtypep '(eql 0.5d0) '(real 1/2 1/2))))
  (t t) (nil t) (t t))
