(in-package :cl-user)
(load (merge-pathnames "tests/tests.lisp" *babel-test-source*))
(load *babel-test-main*)
(load (merge-pathnames "test/packages.lisp" *flexi-test-source*))
(load (merge-pathnames "test/test.lisp" *flexi-test-source*))
(let ((failures nil))
  (dolist (suite '(flexi-streams-test::compare-files flexi-streams-test::string-tests
                   flexi-streams-test::sequence-tests flexi-streams-test::error-handling-tests
                   flexi-streams-test::unread-char-tests flexi-streams-test::column-tests
                   flexi-streams-test::make-external-format-tests flexi-streams-test::peek-byte-tests
                   flexi-streams-test::in-memory-stream-tests))
    (let ((passed (handler-case (funcall suite)
                    (error (c) (format t "FLEXI-ERROR ~A: ~A~%" suite c) nil))))
      (format t "~&PORTABILITY-SUITE flexi-~A ~A~%" suite (if passed "passed" "failed"))
      (finish-output)
      (unless passed (push suite failures))))
  (null failures))
(write-line "PORTABILITY-UPSTREAM-COMPLETE") (finish-output)
