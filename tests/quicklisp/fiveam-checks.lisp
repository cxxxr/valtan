;; Load with the upstream fiveam/test system. A successful load alone does not
;; prove the runner calls its tests (a compiler bug once silently skipped them).
(defpackage :valtan-fiveam-checks (:use :cl :fiveam))
(in-package :valtan-fiveam-checks)
(def-suite :valtan-fiveam-checks)
(in-suite :valtan-fiveam-checks)
(defparameter *executed* 0)
(test assertions
  (incf *executed*)
  (is (= (+ 20 22) 42))
  (is (equal (mapcar #'1+ '(1 2 3)) '(2 3 4))))
(let ((results (run :valtan-fiveam-checks)))
  (assert (= *executed* 1))
  (assert (= (length results) 2))
  (assert (every #'fiveam::test-passed-p results)))
(let ((results (run :it.bese.fiveam)))
  (explain! results)
  ;; FiveAM 1.4.3's upstream suite produces 57 results, including assertions
  ;; which verify intentionally failing, skipped and dependent nested tests.
  (assert (= (length results) 57))
  (assert (every #'fiveam::test-passed-p results))
  (format t "~&FIVEAM-UPSTREAM-PASS ~D~%" (length results)))
