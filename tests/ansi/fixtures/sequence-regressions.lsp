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
