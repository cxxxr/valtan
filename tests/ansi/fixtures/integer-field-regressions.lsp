(in-package :cl-test)

(deftest byte-descriptors-and-extraction
  (let ((b (byte 100 80)))
    (values (byte-size b) (byte-position b) (ldb b -1)
            (ldb-test b 0) (ldb-test b (ash 1 81))
            (mask-field (byte 4 4) -1)))
  100 80 1267650600228229401496703205375 nil t 240)

(deftest byte-deposit-preserves-other-bits
  (values (dpb -1 (byte 4 4) 3) (deposit-field 16 (byte 4 4) -1)
          (dpb 100 (byte 0 200) -7) (deposit-field 100 (byte 0 200) -7))
  243 -225 -7 -7)

(deftest byte-place-order-and-return
  (let ((events nil) (data (vector 255)))
    (values (setf (ldb (progn (push :spec events) (byte 4 2))
                      (aref (progn (push :array events) data)
                            (progn (push :index events) 0)))
                  (progn (push :value events) 0))
            (aref data 0) (reverse events)))
  0 195 (:spec :array :index :value))

(deftest mask-field-place-and-local-macro
  (let ((x -1))
    (macrolet ((place () 'x))
      (values (setf (mask-field (byte 3 4) (place)) 16) x)))
  16 -97)

(deftest integer-square-root-exact-boundaries
  (let* ((n (+ (ash 1 500) 12345)) (square (* n n)))
    (values (= (isqrt square) n) (= (isqrt (1- square)) (1- n))
            (= (isqrt (1- (* (1+ n) (1+ n)))) n)
            (isqrt 0) (isqrt 1)
            (handler-case (isqrt -1) (type-error () :rejected))
            (handler-case (byte 1 -1) (type-error () :rejected))))
  t t t 0 1 :rejected :rejected)
