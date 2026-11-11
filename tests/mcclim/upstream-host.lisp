;; Native baseline for the installed upstream McCLIM test system. This does
;; not substitute host results for Valtan results.
(require :asdf)
(let ((setup (or (uiop:getenv "QUICKLISP_SETUP")
                 (merge-pathnames #p"quicklisp/setup.lisp" (user-homedir-pathname)))))
  (when (probe-file setup) (load setup)))
(pushnew :clim-without-swank *features*)
(asdf:load-system "st-json")
(asdf:load-system "mcclim/test")

(defmethod fiveam::run-test-lambda :before ((test fiveam::test-case))
  (format t "~&MCCLIM-HOST-TEST ~S~%" (fiveam::name test))
  (finish-output))

(let* ((output (or (first (uiop:command-line-arguments))
                   "/tmp/valtan-mcclim-upstream-host.json"))
       (cases (loop for name in (fiveam::test-names)
                    for test = (fiveam:get-test name)
                    when (typep test 'fiveam::test-case) collect test))
       (fiveam:*on-error* nil) (fiveam:*on-failure* nil)
       (results (fiveam:run :mcclim :print-names nil))
       (passed (count-if #'fiveam::test-passed-p results))
       (skipped (count-if (lambda (result) (typep result 'fiveam::test-skipped)) results))
       (failed (- (length results) passed skipped))
       (unexecuted (remove-if-not (lambda (test) (eq (fiveam::status test) :unknown)) cases))
       (success (and cases results (zerop failed) (zerop skipped) (null unexecuted))))
  (with-open-file (out output :direction :output :if-exists :supersede)
    (st-json:write-json
     (st-json:jso
      "status" (if success "passed" "failed") "complete" :true
      "scope" "Native host mcclim/test with its normal dependencies; not a Valtan result"
      "implementation" (lisp-implementation-type)
      "implementationVersion" (lisp-implementation-version)
      "asdfVersion" (asdf:asdf-version)
      "upstreamDefinition" (namestring (asdf:system-source-file "mcclim/test"))
      "features" (mapcar #'symbol-name *features*)
      "registered" (length cases) "results" (length results)
      "passed" passed "failed" failed "skipped" skipped
      "unexecuted" (mapcar (lambda (test) (prin1-to-string (fiveam::name test))) unexecuted)
      "nonpassed" (loop for result in results unless (fiveam::test-passed-p result)
                        collect (st-json:jso
                                 "test" (prin1-to-string (fiveam::name (fiveam::test-case result)))
                                 "kind" (symbol-name (type-of result))
                                 "reason" (fiveam::reason result)))) out))
  (fiveam:explain! results)
  (unless success (uiop:quit 1)))
