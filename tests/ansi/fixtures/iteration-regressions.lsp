(in-package :cl-test)

(deftest loop-captures-arithmetic-limits-and-step-once
  (let ((limit 3) (step 1))
    (loop for i from 0 to limit by step
          collect (prog1 i (incf limit) (incf step))))
  (0 1 2 3))

(deftest loop-ignored-variables-and-dotted-accumulation
  (values (loop for nil below 3 collect :item)
          (loop with nil = (return :evaluated) return nil)
          (loop for x in '(a b c) nconc (cons x 'tail)))
  (:item :item :item) :evaluated (a b c . tail))

(deftest loop-floating-counters-and-package-default
  (values (loop repeat 3 count t of-type double-float)
          (loop for x in nil sum x of-type single-float)
          (let ((*package* (find-package :keyword)))
            (not (null (member :iteration-probe
                              (loop for x being each external-symbol collect x))))))
  3.0 0.0 t)

(deftest iteration-preserves-free-and-bound-special-declarations
  (let ((x :outer))
    (declare (special x))
    (let ((x :inner))
      (values (do* () (t x) (declare (special x)))
              (dolist (i nil x) (declare (special x)))
              (block nil (dotimes (i (return :done))) :continued))))
  :outer :outer :continued)

;; CL-PPCRE uses FIRST = T THEN NIL while scanning a character class.
(deftest loop-explicit-nil-step
  (loop for first = t then nil repeat 3 collect first)
  (t nil nil))

(deftest loop-explicit-nil-destructuring-step
  (loop for (a b) = '(1 2) then nil repeat 3 collect (list a b))
  ((1 2) (nil nil) (nil nil)))

(deftest loop-omitted-step-reevaluates-initial-form
  (let ((n 0)) (loop for value = (incf n) repeat 3 collect value))
  (1 2 3))

(deftest loop-sequential-equals-then
  (loop for x = 1 then (1+ x)
        for y = x then (+ y x)
        repeat 4 collect (list x y))
  ((1 1) (2 3) (3 6) (4 10)))

(deftest loop-parallel-equals-then
  (loop for x = 1 then (1+ x) and y = 0 then x
        repeat 4 collect (list x y))
  ((1 0) (2 1) (3 2) (4 3)))

(deftest loop-sequence-before-dependent-step
  (values (loop for x in '(1 2 3) for y = x then (+ x y) collect y)
          (loop for x across #(1 2 3) for y = x then (+ x y) collect y)
          (loop for x from 1 to 3 for y = x then (+ x y) collect y))
  (1 3 6) (1 3 6) (1 3 6))

(deftest loop-parallel-sequence-step
  (values (loop for x in '(1 2 3) and y = :initial then x collect (list x y))
          (loop for x from 0 below 3 and y = 5 then x collect (list x y)))
  ((1 :initial) (2 1) (3 2)) ((0 5) (1 0) (2 1)))

(deftest loop-final-step-and-empty-iteration
  (values (loop for x = 0 then (1+ x) repeat 3 finally (return x))
          (loop for x = 5 then (1+ x) repeat 0 finally (return x))
          (loop for x in nil for y = (error "Not reached") collect y))
  3 5 nil)

(deftest loop-hash-before-dependent-step
  (let ((table (make-hash-table)))
    (setf (gethash :a table) '(10 20))
    (loop for key being each hash-key of table using (hash-value (x y))
          for total = (+ x y) collect (list key total)))
  ((:a 30)))

(deftest loop-repeat-at-body-position
  (let ((n 0))
    (loop do (incf n) repeat 3 finally (return n)))
  4)
(deftest loop-repeat-between-iteration-clauses
  (let ((input '(1 2 3)) (count 2))
    (values (loop for result = 0 then (+ (* result 10) digit)
                  repeat count for digit = (pop input)
                  finally (return result))
            input))
  12 (3))
(deftest loop-repeat-before-iteration-clauses
  (let ((called nil))
    (values (loop repeat 0 for x = (push t called) collect x) called))
  nil nil)

(deftest loop-while-before-destructuring-iteration
  (loop with points = '((1 :a) (2 :b))
        while points
        for (point chain) = (pop points)
        collect (list point chain))
  ((1 :a) (2 :b)))

(deftest loop-while-skips-later-assignment-on-empty-input
  (let ((called nil))
    (values (loop while nil for x = (push t called) collect x) called))
  nil nil)

(deftest loop-until-between-iteration-clauses
  (let ((input '(1 2)))
    (loop for result = 0 then (+ (* result 10) digit)
          until (null input) for digit = (pop input)
          finally (return result)))
  12)
