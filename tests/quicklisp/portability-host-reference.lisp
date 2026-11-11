;;; Diagnostic baseline only: these results do not establish Valtan compatibility.
(require :asdf)
(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file setup) (load setup)))
(defparameter *reference-harness-directory* (uiop:pathname-directory-pathname *load-truename*))
(asdf:load-system "babel")
(load (merge-pathnames "babel-test-adapter.lisp" *reference-harness-directory*))
;; Match target test selection while retaining the independent host reader.
(let ((*features* '(:valtan :node)))
  (load (asdf:system-relative-pathname "babel" "tests/tests.lisp")))
(load (merge-pathnames "babel-test-main.lisp" *reference-harness-directory*))
(asdf:load-system "flexi-streams-test")
(let ((flexi-streams-test::*tmp-dir*
        (merge-pathnames (format nil "valtan-flexi-reference-~A/" (gensym)) (uiop:temporary-directory))))
  (unwind-protect
      (dolist (suite '(flexi-streams-test::compare-files flexi-streams-test::string-tests
                      flexi-streams-test::sequence-tests flexi-streams-test::error-handling-tests
                      flexi-streams-test::unread-char-tests flexi-streams-test::column-tests
                      flexi-streams-test::make-external-format-tests flexi-streams-test::peek-byte-tests
                      flexi-streams-test::in-memory-stream-tests))
        (let ((passed (handler-case (funcall suite)
                        (error (c) (format t "HOST-FLEXI-ERROR ~A: ~A~%" suite c) nil))))
          (format t "~&PORTABILITY-SUITE flexi-~A ~A~%" suite (if passed "passed" "failed"))))
    (when (probe-file flexi-streams-test::*tmp-dir*)
      (uiop:delete-directory-tree flexi-streams-test::*tmp-dir* :validate t))))
(write-line "PORTABILITY-UPSTREAM-COMPLETE")
