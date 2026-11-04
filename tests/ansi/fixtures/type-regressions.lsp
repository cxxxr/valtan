(in-package :cl-test)

(deftest cons-subtypes-preserve-empty-components
  (values (multiple-value-list (subtypep '(cons nil t) 'symbol))
          (multiple-value-list (subtypep 'cons '(cons * *)))
          (multiple-value-list (subtypep '(cons) 'cons)))
  (t t) (t t) (t t))

(deftest member-type-list-values-are-compared-by-identity
  (let ((left (list :same)) (right (list :same)))
    (values (multiple-value-list (subtypep `(member ,left) `(member ,right)))
            (subtypep `(or (eql ,left)) `(or (eql ,right)))))
  (nil t) nil)

(deftest array-class-and-array-type-have-the-same-extension
  (values (multiple-value-list (subtypep (find-class 'array) '(array * *)))
          (multiple-value-list (subtypep '(array * *) (find-class 'array)))
          (typep #c(-1 1/2) (type-of #c(-1 1/2))))
  (t t) (t t) t)

(deftype implicit-block-type () (return-from implicit-block-type 'integer))
(deftest type-of-retains-an-unregistered-class-object
  (let* ((name (gensym)) (class (eval `(defclass ,name () ())))
         (object (make-instance class)))
    (setf (find-class name) nil)
    (values (eq (type-of object) class) (typep 17 'implicit-block-type)))
  t t)

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
