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
