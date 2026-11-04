(in-package :cl-test)
(deftest nunion-combines-both-lists
  (sort (nunion (list 1 2 3) (list 2 4)) #'<)
  (1 2 3 4))
(deftest asymmetric-set-exclusive-or
  (values (sort (set-exclusive-or '(1 2 3) '(4 5 6) :test #'<) #'<)
          (sort (nset-exclusive-or (list 1 2 3) (list 4 5 6) :test #'<) #'<))
  nil nil)
(deftest setf-rest-returns-stored-value
  (let ((x (list 1 2 3))) (values (setf (rest x) 9) x))
  9 (1 . 9))
(deftest rotatef-evaluates-places-once
  (let ((x (vector 1 2 3)) (i -1))
    (values (rotatef (aref x (incf i)) (aref x (incf i)) (aref x (incf i)))
            x i))
  nil #(2 3 1) 2)
(deftest rotatef-multiple-values
  (let ((a 1) (b 2) (c 3) (d 4))
    (rotatef (values a b) (values c d))
    (values a b c d))
  3 4 1 2)
