;; Record completed test invocations without changing FiveAM's execution order
;; or dependency/status handling. A recursive RUN resets statuses and can cause
;; later outer cases to be executed only by the inner RUN.
(defpackage #:valtan-fiveam-executions
  (:use #:cl)
  (:export #:collect-executions #:latest-results #:all-results))
(in-package #:valtan-fiveam-executions)

(defvar *collecting* nil)
(defvar *executions* nil)
(defvar *depth* 0)

(defmethod fiveam::run-test-lambda :around ((test fiveam::test-case))
  (if (not *collecting*)
      (call-next-method)
      (let ((tail (last (symbol-value 'fiveam::result-list)))
            (*depth* (1+ *depth*)))
        (multiple-value-prog1 (call-next-method)
          ;; The primary method appends this invocation's results to its
          ;; caller's list. Copy the suffix before later siblings append.
          (push (list test *depth*
                      (copy-list (if tail (cdr tail)
                                     (symbol-value 'fiveam::result-list))))
                *executions*)))))

(defun collect-executions (function)
  "Return FUNCTION's result and completed invocations in completion order."
  (let ((*collecting* t) (*executions* nil) (*depth* 0))
    (let ((result (funcall function)))
      (values result (nreverse *executions*)))))

(defun latest-results (executions)
  "Return the last invocation's results for each test, including empty results."
  (let ((latest (make-hash-table :test #'eq)))
    (dolist (execution executions)
      (setf (gethash (fiveam::name (first execution)) latest) (third execution)))
    (loop for results being the hash-values of latest
          append (copy-list results))))

(defun all-results (executions)
  "Retain every outcome, including failures/skips before a later successful run."
  (loop for execution in executions append (copy-list (third execution))))
