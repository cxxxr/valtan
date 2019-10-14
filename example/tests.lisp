(ffi:require js:fs "fs")

(defun test (filename)
  (with-open-file (in filename)
    (let ((eof-value '#:eof))
      (do ((form (read in nil eof-value) (read in nil eof-value)))
          ((eq form eof-value))
        (prin1 form)
        (terpri)
        (handler-case
            (eval form)
          (error (e)
            (format t "*** ERROR ***: ~A~%" e))
          (:no-error (ok)
            (if ok
                (write-line "OK")
                (write-line "FAIL"))))))))

;; (test "example/sacla-tests/must-cons.lisp")
;; (test "example/sacla-tests/must-character.lisp")
;; (test "example/sacla-tests/must-string.lisp")
(test "example/sacla-tests/must-sequence.lisp")
