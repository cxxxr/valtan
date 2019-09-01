(ffi:require js:fs "fs")

(with-open-file (in "example/sacla-tests/must-sequence.lisp")
  (let ((eof-value '#:eof))
    (do ((form (read in nil eof-value) (read in nil eof-value)))
        ((eq form eof-value))
      (prin1 form)
      (terpri)
      (cond ((eval form)
             (write-line "OK"))
            (t
             (write-line "FAIL"))))))
