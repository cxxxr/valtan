;; A diagnostic of the complete upstream test-source plan. Source load errors,
;; skipped checks and unexecuted test cases prevent success; nothing is omitted.
(defpackage #:mcclim-upstream-checks (:use #:cl))
(in-package #:mcclim-upstream-checks)
;; MCCLIM is announced by its ASDF definition. Without it the upstream test
;; reader silently omits implementation-specific assertions within test cases.
(assert (member :mcclim *features*) () "McCLIM's ASDF feature is missing in the target")
(defvar *files* nil)
(defvar *report-progress* nil)

(defmethod fiveam::run-test-lambda :before ((test fiveam::test-case))
  (when *report-progress*
    (format t "~&MCCLIM-UPSTREAM-TEST ~S~%" (fiveam::name test))))

(defun test-cases ()
  (loop for name in (fiveam::test-names)
        for test = (fiveam:get-test name)
        when (typep test 'fiveam::test-case) collect test))

(defun js-array (items &optional (convert #'identity))
  (let ((array (ffi:array)))
    (dolist (item items array)
      ((ffi:ref array "push") (funcall convert item)))))

(defun name-string (test) (prin1-to-string (fiveam::name test)))

(defun load-test-source (file source)
  (format t "~&MCCLIM-UPSTREAM-LOAD ~A~%" file)
  (let ((before (test-cases)) (failure nil))
    (handler-case
        (let ((*load-verbose* nil))
          (system::load-embedded-source source file *features*))
      (error (condition) (setf failure (princ-to-string condition))))
    (push (list file failure (set-difference (test-cases) before :test #'eq)) *files*)
    (when failure (format t "~&MCCLIM-UPSTREAM-LOAD-FAIL ~A: ~A~%" file failure))))

(defun run (&optional (suite :mcclim))
  (let* ((cases (remove-duplicates
                (mapcan (lambda (entry) (copy-list (third entry))) *files*) :test #'eq))
         (*report-progress* t)
         (clim:*application-frame* climi::*default-application-frame*)
         (fiveam:*test-dribble* (make-string-output-stream))
         (fiveam:*on-error* nil) (fiveam:*on-failure* nil)
         (observation (multiple-value-list
                       (valtan-fiveam-executions:collect-executions
                        (lambda () (fiveam:run suite :print-names nil)))))
         (outer-results (first observation))
         (executions (second observation))
         (results (valtan-fiveam-executions:latest-results executions))
         (all-results (valtan-fiveam-executions:all-results executions))
         (execution-failed (count-if #'fiveam::test-failure-p all-results))
         (execution-skipped (count-if #'fiveam::test-skipped-p all-results))
         (counts (make-hash-table :test #'eq))
         (passed (count-if #'fiveam::test-passed-p results))
         (skipped (count-if (lambda (result) (typep result 'fiveam::test-skipped)) results))
         (failed (- (length results) passed skipped))
         (unexecuted (remove-if-not (lambda (test) (eq (fiveam::status test) :unknown)) cases))
         (load-errors (count-if #'second *files*))
         (success (and (plusp (length cases)) (plusp (length results))
                       (zerop load-errors) (zerop failed) (zerop skipped) (null unexecuted)
                       (zerop execution-failed) (zerop execution-skipped))))
    (dolist (result results)
      (incf (gethash (fiveam::test-case result) counts 0)))
    (ffi:js->cl
     ((ffi:ref "JSON" "stringify")
      (ffi:object
       :status (ffi:cl->js (if success "passed" "failed"))
       :features (js-array *features* (lambda (feature) (ffi:cl->js (symbol-name feature))))
       :outer-results (length outer-results)
       :execution-results (length all-results)
       :execution-failed execution-failed :execution-skipped execution-skipped
       :executions (js-array executions
                    (lambda (entry)
                      (let* ((outcomes (third entry))
                             (passed (count-if #'fiveam::test-passed-p outcomes))
                             (skipped (count-if #'fiveam::test-skipped-p outcomes)))
                        (ffi:object :test (ffi:cl->js (name-string (first entry)))
                                    :depth (second entry) :results (length outcomes)
                                    :passed passed :skipped skipped
                                    :failed (- (length outcomes) passed skipped)))))
       :execution-nonpassed
       (js-array (remove-if #'fiveam::test-passed-p all-results)
                 (lambda (result)
                   (ffi:object :test (ffi:cl->js (name-string (fiveam::test-case result)))
                               :kind (ffi:cl->js (symbol-name (type-of result)))
                               :reason (ffi:cl->js (fiveam::reason result)))))
       :files (js-array (reverse *files*)
                        (lambda (entry)
                          (ffi:object :file (ffi:cl->js (first entry))
                                      :error (if (second entry) (ffi:cl->js (second entry)) (ffi:ref "null"))
                                      :registered (js-array (third entry)
                                                            (lambda (test) (ffi:cl->js (name-string test)))))))
       :registration-entries (reduce #'+ *files* :key (lambda (entry) (length (third entry))) :initial-value 0)
       :result-counts (js-array
                       (loop for test being the hash-keys of counts using (hash-value count)
                             collect (cons test count))
                       (lambda (entry)
                         (ffi:object :test (ffi:cl->js (name-string (car entry)))
                                     :count (cdr entry))))
       :registered (length cases) :results (length results) :passed passed :failed failed :skipped skipped
       :unexecuted (js-array unexecuted (lambda (test) (ffi:cl->js (name-string test))))
       :nonpassed (js-array (remove-if #'fiveam::test-passed-p results)
                            (lambda (result)
                              (ffi:object :test (ffi:cl->js (name-string (fiveam::test-case result)))
                                          :kind (ffi:cl->js (symbol-name (type-of result)))
                                          :reason (ffi:cl->js (fiveam::reason result)))))
       :output (ffi:cl->js (get-output-stream-string fiveam:*test-dribble*)))))))
