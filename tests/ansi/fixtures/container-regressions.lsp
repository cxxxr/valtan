(in-package :cl-test)

(deftest equalp-hash-tables-compare-entries-with-the-key-test
  (let ((left (make-hash-table :test 'equal :rehash-size 2))
        (right (make-hash-table :test 'equal :rehash-size 3)))
    (setf (gethash "a" left) "first" (gethash "b" left) "second"
          (gethash "b" right) "SECOND" (gethash "a" right) "FIRST")
    (let ((same (equalp left right)))
      (remhash "a" right)
      (setf (gethash "A" right) "first")
      (values same (equalp left right))))
  t nil)

(defclass equalp-instance-regression () ((value :initarg :value)))
(deftest equalp-standard-instances-use-identity
  (let ((left (make-instance 'equalp-instance-regression :value 1))
        (right (make-instance 'equalp-instance-regression :value 1)))
    (values (equalp left left) (equalp left right)))
  t nil)
(deftest multidimensional-character-array-is-not-a-string
  (let ((x (make-array '(2 2) :element-type 'character :initial-element #\a)))
    (values (stringp x) (simple-string-p x) (arrayp x)))
  nil nil t)
(deftest structure-accessor-rejects-wrong-structure
  (handler-case (hash-table-count (make-random-state))
    (type-error (c) (not (typep (type-error-datum c) (type-error-expected-type c)))))
  t)
(deftest dotted-equal-hash-key
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash (cons 'a 'b) h) 9)
    (gethash (cons 'a 'b) h))
  9 t)
(deftest long-list-to-character-array
  (let* ((characters (loop for i below 65536 collect (code-char i)))
         (string (coerce characters 'string))
         (array (make-array 65536 :element-type 'character :initial-contents characters)))
    (values (length string) (char-code (char string 65535)) (string= string array)
            (map 'list #'+ '(1 2 3) #(10 20))))
  65536 65535 t (11 22))

(deftest sxhash-is-bounded-and-stable-for-equal-objects
  (let* ((a (vector 1 2)) (hash (sxhash a))
         (bits (make-array 5 :element-type 'bit :initial-contents #*01011 :fill-pointer 3)))
    (setf (aref a 0) 'changed)
    (values (= hash (sxhash a)) (= (sxhash #*010) (sxhash bits))
            (typep (sxhash (ash 1 500)) '(and fixnum unsigned-byte))
            (= (sxhash #c(0.0 0.0)) (sxhash #c(-0.0 -0.0)))))
  t t t t)
