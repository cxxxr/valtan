(in-package :cl-test)

(deftest stable-sort-list-keys-and-equal-key-order
  (stable-sort (list (cons 2 :a) (cons 1 :b) (cons 2 :c) (cons 1 :d)) #'< :key #'car)
  ((1 . :b) (1 . :d) (2 . :a) (2 . :c)))

(deftest sort-symbol-names-applies-key-before-predicate
  (sort (list 'zebra 'apple 'middle) #'string-lessp :key #'symbol-name)
  (apple middle zebra))

(deftest stable-sort-short-vectors-retain-contents
  (values (stable-sort (vector 9) #'<)
          (stable-sort (copy-seq "a") #'char<)
          (stable-sort (vector) #'<))
  #(9) "a" #())

(deftest stable-sort-vector-keeps-equal-key-order
  (stable-sort (vector (cons 2 :a) (cons 1 :b) (cons 2 :c) (cons 1 :d)) #'< :key #'car)
  #((1 . :b) (1 . :d) (2 . :a) (2 . :c)))

(deftest some-returns-predicate-value-and-stops
  (let ((visited nil))
    (values (some (lambda (x) (push x visited) (when (> x 1) (list x))) '(1 2 3))
            (reverse visited)
            (some (lambda (x y) (when (= x y) :match)) '(1 2 3) #(0 2))
            (multiple-value-list (some (lambda (x) (declare (ignore x)) (values :found :extra)) '(1)))))
  (2) (1 2) :match (:found))

(deftest notevery-uses-shortest-sequence-and-empty-identity
  (let ((calls 0))
    (values (notevery #'= '(1 2) #(1 2 9))
            (notevery #'= '(1 2) #(1 3))
            (notevery (lambda (x) (incf calls) x) nil)
            calls))
  nil t nil 0)

(deftest elt-update-returns-element-and-honors-fill-pointer
  (let ((list (list :old :tail))
        (vector (make-array 4 :fill-pointer 1 :initial-element 0)))
    (values (setf (elt list 0) :new) list
            (handler-case (elt vector 1)
              (type-error (c) (not (typep (type-error-datum c) (type-error-expected-type c)))))
            (handler-case (setf (elt vector 1) 9) (type-error () :bounds))))
  :new (:new :tail) t :bounds)

(deftest sequence-construction-retains-element-and-length-constraints
  (values (make-sequence (find-class 'bit-vector) 3 :initial-element 1)
          (array-element-type (make-sequence 'bit-vector 3))
          (handler-case (make-sequence '(vector t 4) 3) (type-error () :length))
          (handler-case (map '(simple-string 2) #'identity "abc") (type-error () :length))
          (handler-case (merge '(vector t 4) '(1 3) '(2) #'<) (type-error () :length)))
  #*111 bit :length :length :length)

(deftest sequence-conversions-produce-specialized-bit-vectors
  (values (coerce '(1 0 1) 'bit-vector)
          (concatenate 'bit-vector #*10 #*011)
          (merge 'bit-vector #*01 #*01 #'<))
  #*101 #*10011 #*0011)

(deftest map-into-with-no-input-calls-nullary-function
  (let ((calls 0) (list (list nil nil nil))
        (vector (make-array 3 :fill-pointer 0 :initial-element 0)))
    (map-into list (lambda () (incf calls)))
    (map-into vector (lambda () (incf calls)))
    (values list vector (fill-pointer vector) calls))
  (1 2 3) #(4 5 6) 3 6)

(deftest finite-member-subtypes-use-eql-and-include-characters
  (let ((a (copy-seq "same")) (b (copy-seq "same")))
    (values (multiple-value-list (subtypep `(member ,a) `(member ,b)))
            (multiple-value-list (subtypep '(member #\a #\b) 'character))
            (coerce 3 'complex)))
  (nil t) (t t) 3)

(deftest bit-vector-length-constraints-apply-to-merge
  (values (typep #*000000 '(bit-vector 3))
          (handler-case (make-sequence '(bit-vector 3) 6) (type-error () :length))
          (handler-case (merge '(bit-vector 3) (list 0 0 0) (list 1 1 1) #'<)
            (type-error () :length)))
  nil :length :length)
