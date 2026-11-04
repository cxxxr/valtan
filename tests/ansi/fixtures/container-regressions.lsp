(in-package :cl-test)
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
