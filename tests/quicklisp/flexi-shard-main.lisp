(in-package :cl-user)
(load (merge-pathnames "test/packages.lisp" *flexi-test-source*))
(load (merge-pathnames "test/test.lisp" *flexi-test-source*))
(let* ((argv (ffi:ref "process" "argv"))
       (index (parse-integer (ffi:js->cl (ffi:aget argv 2))))
       (count (parse-integer (ffi:js->cl (ffi:aget argv 3))))
       (flexi-streams-test::*tmp-dir* (pathname (ffi:js->cl (ffi:aget argv 4))))
       (original (symbol-function 'flexi-streams-test::create-test-combinations))
       (cases 0) (selected 0))
  ;; Each original comparison scenario exercises two copy implementations and
  ;; all four input/output direction pairs. Partition scenarios, not assertions.
  (unwind-protect
       (progn
         (setf (symbol-function 'flexi-streams-test::create-test-combinations)
               (lambda (&rest args)
                 (let ((result nil))
                   (dolist (scenario (apply original args) (nreverse result))
                     (when (= (mod cases count) index)
                       (let ((*print-pretty* nil))
                         (format t "~&FLEXI-CASE ~D ~S~%" cases scenario))
                       (incf selected)
                       (push scenario result))
                     (incf cases)))))
         (let ((passed (handler-case (flexi-streams-test::compare-files)
                         (error (c) (format t "~&FLEXI-ERROR COMPARE-FILES: ~A~%" c) nil))))
           (format t "~&PORTABILITY-SUITE flexi-COMPARE-FILES ~A~%" (if passed "passed" "failed"))))
    (setf (symbol-function 'flexi-streams-test::create-test-combinations) original))
  (format t "~&FLEXI-SHARD ~D ~D ~D ~D~%" index count cases selected)
  ;; The remaining eight original suites run once, with their full inputs.
  (when (zerop index)
    (dolist (suite '(flexi-streams-test::string-tests flexi-streams-test::sequence-tests
                    flexi-streams-test::error-handling-tests flexi-streams-test::unread-char-tests
                    flexi-streams-test::column-tests flexi-streams-test::make-external-format-tests
                    flexi-streams-test::peek-byte-tests flexi-streams-test::in-memory-stream-tests))
      (let ((passed (handler-case (funcall suite)
                      (error (c) (format t "~&FLEXI-ERROR ~A: ~A~%" suite c) nil))))
        (format t "~&PORTABILITY-SUITE flexi-~A ~A~%" suite (if passed "passed" "failed"))
        (finish-output))))
  (format t "~&FLEXI-SHARD-COMPLETE ~D~%" index) (finish-output))
