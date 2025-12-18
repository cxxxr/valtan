;;;; test-runner.lisp - Modular test runner for valtan SACLA tests
;;;;
;;;; Usage (from JavaScript after compilation):
;;;;   (run-tests)                        ; Run all tests
;;;;   (run-tests :categories '(:cons))   ; Run only cons tests
;;;;   (run-tests :files '("must-cons.lisp"))
;;;;   (run-tests :verbose nil)           ; Quiet mode
;;;;   (run-tests :stop-on-error t)       ; Stop at first failure

(defvar *test-results* nil
  "Accumulated test results")

(defvar *test-verbose* t
  "When true, print each test result")

(defvar *test-stop-on-error* nil
  "When true, stop testing at first error")

(defvar *current-test-file* nil
  "Currently running test file")

;;; Test categories mapping
(defparameter *test-categories*
  '((:cons       . ("must-cons.lisp" "should-cons.lisp"))
    (:array      . ("must-array.lisp" "should-array.lisp"))
    (:sequence   . ("must-sequence.lisp" "should-sequence.lisp" "x-sequence.lisp"))
    (:string     . ("must-string.lisp" "should-string.lisp"))
    (:character  . ("must-character.lisp" "should-character.lisp"))
    (:symbol     . ("must-symbol.lisp" "should-symbol.lisp"))
    (:hash-table . ("must-hash-table.lisp" "should-hash-table.lisp"))
    (:loop       . ("must-loop.lisp"))
    (:reader     . ("must-reader.lisp"))
    (:printer    . ("desirable-printer.lisp"))
    (:condition  . ("must-condition.lisp"))
    (:control    . ("must-data-and-control.lisp" "should-data-and-control.lisp"))
    (:do         . ("must-do.lisp"))
    (:eval       . ("must-eval.lisp" "should-eval.lisp"))
    (:package    . ("should-package.lisp"))))

;;; Default test files (stable tests first)
(defparameter *default-test-files*
  '("must-cons.lisp"           ; Usually 100% pass
    "must-character.lisp"
    "must-do.lisp"
    "must-eval.lisp"
    "must-condition.lisp"
    "must-array.lisp"
    "must-hash-table.lisp"
    "must-data-and-control.lisp"
    "must-loop.lisp"
    "must-symbol.lisp"
    "must-string.lisp"
    "desirable-printer.lisp"
    ;; These have known issues:
    "must-reader.lisp"
    "must-sequence.lisp"
    ;; Should tests (error handling)
    "should-cons.lisp"
    "should-character.lisp"
    "should-array.lisp"
    "should-hash-table.lisp"
    "should-data-and-control.lisp"
    "should-eval.lisp"
    "should-package.lisp"
    "should-sequence.lisp"
    "should-string.lisp"
    "should-symbol.lisp"))

;;; Result structure
(defstruct test-result
  file
  index
  form
  passed
  error-message
  time-ms)

(defstruct file-result
  file
  pass-count
  fail-count
  results
  time-ms)

;;; Core test runner
;; NOTE: Tests that use SIGNAL with error-type conditions (like type-error)
;; may fail because the test runner's handler-case for catching errors will
;; also catch signaled conditions when inner handlers decline. This is
;; correct behavior per ANSI CL but causes some signal-return-value tests to fail.
(defun run-single-test (form index)
  "Run a single test form and return test-result"
  (let ((start-time (js:-date.now))
        (result (make-test-result :file *current-test-file*
                                  :index index
                                  :form form)))
    (handler-case
        (let ((value (eval form)))
          (setf (test-result-passed result) (if value t nil))
          (setf (test-result-time-ms result) (- (js:-date.now) start-time))
          (unless value
            (setf (test-result-error-message result) "Test returned NIL")))
      (error (e)
        (setf (test-result-passed result) nil)
        (setf (test-result-error-message result) (format nil "~A" e))
        (setf (test-result-time-ms result) (- (js:-date.now) start-time))))
    result))

(defun run-test-file (filename &key (verbose *test-verbose*))
  "Run all tests in a file, return file-result"
  (let* ((filepath (format nil "sacla-tests/~A" filename))
         (*current-test-file* filename)
         (start-time (js:-date.now))
         (pass 0)
         (fail 0)
         (results '()))
    (when verbose
      (format t "~%=== Testing: ~A ===~%" filename))
    (handler-case
        (with-open-file (in filepath)
          (let ((eof-value (gensym))
                (cl-user-pkg (find-package :cl-user)))
            (do ((n 0 (1+ n)))
                (nil)
              (let* ((*package* cl-user-pkg)
                     (form (read in nil eof-value)))
                (when (eq form eof-value)
                  (return))
                (let ((result (run-single-test form n)))
                  (push result results)
                  (if (test-result-passed result)
                      (progn
                        (incf pass)
                        (when verbose
                          (format t "~D PASS: ~S~%" n form)))
                      (progn
                        (incf fail)
                        (when verbose
                          (format t "~D FAIL: ~S~%   Error: ~A~%"
                                  n form
                                  (or (test-result-error-message result) "NIL")))
                        (when *test-stop-on-error*
                          (return)))))))))
      (error (e)
        (format t "Error loading test file ~A: ~A~%" filename e)))
    (let ((total-time (- (js:-date.now) start-time)))
      (when verbose
        (format t "--- ~A: Pass=~D, Fail=~D (~Dms) ---~%"
                filename pass fail total-time))
      (make-file-result :file filename
                        :pass-count pass
                        :fail-count fail
                        :results (nreverse results)
                        :time-ms total-time))))

;;; High-level API
(defun get-files-for-categories (categories)
  "Get list of test files for given categories"
  (let ((files '()))
    (dolist (cat categories)
      (let ((entry (assoc cat *test-categories*)))
        (when entry
          (dolist (file (cdr entry))
            (pushnew file files :test #'string=)))))
    (nreverse files)))

(defun run-tests (&key categories files (verbose t) stop-on-error)
  "Main entry point for running tests.

   Keywords:
     :categories - List of category keywords (e.g., '(:cons :array))
     :files      - List of specific files to run
     :verbose    - Print detailed output (default t)
     :stop-on-error - Stop at first failure (default nil)

   Returns: plist with :total-pass :total-fail :file-results :time-ms"
  (let* ((*test-verbose* verbose)
         (*test-stop-on-error* stop-on-error)
         (test-files (cond
                       (files files)
                       (categories (get-files-for-categories categories))
                       (t *default-test-files*)))
         (start-time (js:-date.now))
         (total-pass 0)
         (total-fail 0)
         (file-results '()))

    (format t "~%========================================~%")
    (format t "Valtan Test Runner~%")
    (format t "Files to test: ~D~%" (length test-files))
    (format t "========================================~%")

    (dolist (file test-files)
      (let ((result (run-test-file file :verbose verbose)))
        (push result file-results)
        (incf total-pass (file-result-pass-count result))
        (incf total-fail (file-result-fail-count result))
        (when (and stop-on-error (> (file-result-fail-count result) 0))
          (format t "~%Stopping due to failures in ~A~%" file)
          (return))))

    (let ((total-time (- (js:-date.now) start-time)))
      (format t "~%========================================~%")
      (format t "TOTAL: Pass=~D, Fail=~D (~Dms)~%" total-pass total-fail total-time)
      (let* ((total (+ total-pass total-fail))
             (rate (if (> total 0)
                       (floor (* 100 total-pass) total)
                       0)))
        (format t "Pass Rate: ~D%~%" rate))
      (format t "========================================~%")

      ;; Return summary
      (list :total-pass total-pass
            :total-fail total-fail
            :file-results (nreverse file-results)
            :time-ms total-time))))

;;; Convenience functions
(defun test-category (category &key (verbose t))
  "Test a single category"
  (run-tests :categories (list category) :verbose verbose))

(defun test-file (filename &key (verbose t))
  "Test a single file"
  (run-tests :files (list filename) :verbose verbose))

(defun quick-test ()
  "Run only the most stable tests (fast feedback)"
  (run-tests :categories '(:cons :do :eval) :verbose t))

(defun list-categories ()
  "Print available test categories"
  (format t "~%Available test categories:~%")
  (dolist (entry *test-categories*)
    (format t "  ~A: ~{~A~^, ~}~%" (car entry) (cdr entry))))

;;; Export results as JSON-like format for CI
(defun results-to-json (results)
  "Convert test results to JSON string for CI integration"
  (format nil "{\"pass\":~D,\"fail\":~D,\"time_ms\":~D,\"files\":[~{~A~^,~}]}"
          (getf results :total-pass)
          (getf results :total-fail)
          (getf results :time-ms)
          (mapcar (lambda (fr)
                    (format nil "{\"file\":\"~A\",\"pass\":~D,\"fail\":~D}"
                            (file-result-file fr)
                            (file-result-pass-count fr)
                            (file-result-fail-count fr)))
                  (getf results :file-results))))
