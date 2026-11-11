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
(deftype character-alias () 'character)
(deftest arrays-expand-element-type-aliases
  (let ((s (make-array 2 :element-type 'character-alias :initial-element #\a)))
    (values (array-element-type s) (stringp s) (string= s "aa")))
  character t t)

(deftype octet-alias () '(unsigned-byte 8))
(deftest octet-arrays-preserve-specialization
  (let ((bytes (make-array 4 :element-type 'octet-alias :initial-contents '(0 1 128 255))))
    (values (array-element-type bytes) (typep bytes '(vector t))
            (typep bytes '(vector (integer 0 255))) (coerce bytes 'list)
            (upgraded-array-element-type '(unsigned-byte 7))
            (upgraded-array-element-type '(integer 0 1))))
  (unsigned-byte 8) nil t (0 1 128 255) (unsigned-byte 8) bit)

(deftest octet-array-writes-and-initialization-check-bounds
  (let ((bytes (make-array 3 :element-type '(unsigned-byte 8))))
    (values (coerce bytes 'list)
            (handler-case (setf (aref bytes 0) 256) (type-error () :bad))
            (handler-case (make-array 1 :element-type '(unsigned-byte 8) :initial-element -1)
              (type-error () :bad))
            (handler-case (make-array '(1 2) :element-type '(unsigned-byte 8) :initial-contents '((0 256)))
              (type-error () :bad))))
  (0 0 0) :bad :bad :bad)

(deftest octet-arrays-adjust-and-displace
  (let* ((bytes (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(65 255)
                              :adjustable t :fill-pointer 2))
         (view (make-array 1 :element-type '(unsigned-byte 8) :displaced-to bytes :displaced-index-offset 1)))
    (vector-push-extend 128 bytes)
    (setf (aref view 0) 0)
    (values (array-element-type bytes) (coerce bytes 'list) (coerce view 'list)))
  (unsigned-byte 8) (65 0 128) (0))
