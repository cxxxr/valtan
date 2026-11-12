(defpackage #:valtan-fiveam-execution-checks (:use #:cl #:fiveam))
(in-package #:valtan-fiveam-execution-checks)

(def-suite nested-suite)
(in-suite nested-suite)
(defvar *inner* nil)
(defvar *hidden-failure* nil)
(defvar *hidden-skip* nil)

(test before
  (is (= 1 1))
  (is (= 2 2))
  (is (not (and *inner* *hidden-failure*))))

(test recurse
  (unless *inner*
    (let ((*inner* t)) (run 'nested-suite :print-names nil))
    ;; Deliberately ignore the inner outcomes: the observer must still see them.
    (is-true t)))

(test after
  (if (and *inner* *hidden-skip*)
      (skip "Nested skip must remain visible")
      (progn (is (= 4 4)) (is (= 5 5)))))

(defun check-run (hidden-failure hidden-skip expected-results)
  (let ((*hidden-failure* hidden-failure) (*hidden-skip* hidden-skip)
        (*on-error* nil) (*on-failure* nil)
        (*test-dribble* (make-broadcast-stream)))
    (multiple-value-bind (outer executions)
        (valtan-fiveam-executions:collect-executions
         (lambda () (run 'nested-suite :print-names nil)))
      (let ((latest (valtan-fiveam-executions:latest-results executions))
            (all (valtan-fiveam-executions:all-results executions)))
        (assert (= 4 (length outer)))
        (assert (every #'fiveam::test-passed-p outer))
        (assert (= 5 (length executions)))
        (assert (= 3 (count 2 executions :key #'second)))
        (assert (= expected-results (length latest)))
        (assert (= (+ expected-results 3) (length all)))
        (assert (= (if hidden-failure 1 0)
                   (count-if #'fiveam::test-failure-p all)))
        (assert (= (if hidden-skip 1 0)
                   (count-if #'fiveam::test-skipped-p all)))
        (assert (= (if hidden-skip 1 2)
                   (count 'after latest
                          :key (lambda (result)
                                 (fiveam::name (fiveam::test-case result))))))))))

(defun run-checks ()
  (check-run nil nil 6)
  (check-run t nil 6)
  (check-run nil t 5)
  (assert (not valtan-fiveam-executions::*collecting*))
  (write-line "FIVEAM-NESTED-EXECUTIONS-PASS")
  t)

(run-checks)
#+valtan
(ffi:set (ffi:ref "globalThis.valtanFiveamExecutionKernel") (ffi:ref "lisp"))
