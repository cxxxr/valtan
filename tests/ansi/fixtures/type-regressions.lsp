(in-package :cl-test)

(deftest condition-subtypes
  (values (multiple-value-list (subtypep 'type-error 'condition))
          (multiple-value-list (subtypep 'condition 'type-error)))
  (t t) (nil t))
(deftest class-object-subtypes
  (subtypep (find-class 'type-error) (find-class 'condition))
  t t)
(deftype positive-small-integer () '(integer 1 5))
(deftest deftype-recursion (mapcar (lambda (x) (typep x 'positive-small-integer)) '(0 1 5 6)) (nil t t nil))
(deftest interval-bounds (mapcar (lambda (x) (typep x '(integer (0) (3)))) '(0 1 2 3)) (nil t t nil))
(deftest structured-cons (values (typep '(7) '(cons (integer 1) null)) (typep '(0) '(cons (integer 1) null))) t nil)
(deftest signed-width (mapcar (lambda (x) (typep x '(signed-byte 3))) '(-5 -4 3 4)) (nil t t nil))
(deftest unsigned-width (mapcar (lambda (x) (typep x '(unsigned-byte 3))) '(-1 0 7 8)) (nil t t nil))
(deftest array-rank-and-dimensions
  (let ((a (make-array '(2 3))))
    (values (typep a '(array * (2 3))) (typep a '(array * (2 4)))
            (typep a '(array * 2)) (typep a '(array * 1))))
  t nil t nil)
(deftest vector-dimension-includes-fill-pointer-tail
  (let ((a (make-array 5 :fill-pointer 2)))
    (values (typep a '(vector * 5)) (typep a '(vector * 2))))
  t nil)
(deftest array-element-upgrading
  (let ((a (make-array 3 :element-type '(unsigned-byte 1))))
    (values (typep a '(vector bit 3)) (typep a '(vector (unsigned-byte 1) 3))
            (typep a '(vector t 3))))
  t t nil)
(deftest float-alias (typep 1.0 'single-float) t)
(deftest class-type (typep 1 (find-class 'integer)) t)

(deftest numeric-subtype-ranges-and-float-aliases
  (values (multiple-value-list (subtypep '(signed-byte 16) 'integer))
          (multiple-value-list (subtypep '(unsigned-byte 8) '(integer 0 300)))
          (multiple-value-list (subtypep '(integer 0 301) '(unsigned-byte 8)))
          (multiple-value-list (subtypep 'short-float 'double-float)))
  (t t) (t t) (nil t) (t t))
