;; Unchanged test bodies from Bordeaux Threads 0.9.4 test/tests-v2.lisp.
;; Its platform guard predates Valtan; register these three tests explicitly.
;; License: library/portability/bordeaux-threads/upstream/LICENSE.
(in-package :bordeaux-threads-2/test)
(in-suite :bordeaux-threads-2)

(test atomic-integer-incf-decf.return-value
  (let ((aint (make-atomic-integer :value 0)))
    (is (= 5 (atomic-integer-incf aint 5)))
    (is (= 4 (atomic-integer-decf aint 1)))))

(test atomic-integer-compare-and-swap.return-value
  (let ((aint (make-atomic-integer :value 4)))
    (is (null (atomic-integer-compare-and-swap aint 0 100)))
    (is (eql t (atomic-integer-compare-and-swap aint 4 7)))))

(test atomic-integer.concurrency
  (let* ((aint (make-atomic-integer :value 1000000))
         (thread-inc
           (make-thread (lambda ()
                          (dotimes (i 1000000)
                            (atomic-integer-incf aint)))))
         (thread-dec
           (make-thread (lambda ()
                          (dotimes (i 1000000)
                            (atomic-integer-decf aint))))))
    (join-thread thread-inc)
    (join-thread thread-dec)
    (is (= 1000000 (atomic-integer-value aint)))))
