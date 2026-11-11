(in-package :cl-user)
(let ((failures nil) (tests (reverse hu.dwim.stefil::*tests*)))
  (dolist (test tests)
    (format t "BABEL-TEST ~A~%" test) (force-output)
    (handler-case (funcall test)
      (error (c) (push test failures) (format t "BABEL-FAIL ~A: ~A~%" test c))))
  (format t "PORTABILITY-SUITE babel ~A ~D ~D~%"
          (if failures "failed" "passed") (length tests) hu.dwim.stefil::*assertions*)
  (force-output)
  ;; The outer runner checks this summary and continues with the other suites.
  (null failures))
