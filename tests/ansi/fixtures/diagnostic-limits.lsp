(in-package :cl-test)
(defparameter *diagnostic-controls*
  (list *print-length* *print-level* *print-circle* *print-readably* *print-pretty*))

(deftest large-failure-remains-a-failure
  (loop repeat 10000 collect (list :unexpected :value :details))
  nil)

(deftest circular-failure-remains-a-failure
  (let ((object (list :unexpected)))
    (setf (cdr object) object)
    object)
  nil)

(deftest circular-condition-remains-a-failure
  (let ((object (list :unexpected)))
    (setf (cdr object) object)
    (error 'type-error :datum object :expected-type 'null))
  t)

(deftest reporting-restores-printer-controls-and-runs-next-test
  (equal *diagnostic-controls*
         (list *print-length* *print-level* *print-circle* *print-readably* *print-pretty*))
  t)
