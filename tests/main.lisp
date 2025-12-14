;;;; main.lisp - Test entry point with CLI argument support
;;;;
;;;; Usage:
;;;;   node dist/tests.js                    # Run all tests
;;;;   node dist/tests.js --category cons    # Run cons tests only
;;;;   node dist/tests.js --category array sequence  # Multiple categories
;;;;   node dist/tests.js --file must-cons.lisp
;;;;   node dist/tests.js --quick            # Quick smoke test
;;;;   node dist/tests.js --list             # List available categories
;;;;   node dist/tests.js --quiet            # Less verbose output
;;;;   node dist/tests.js --json             # Output JSON for CI

(defun parse-cli-args ()
  "Parse command line arguments from Node.js process.argv"
  ;; For now, return empty list - CLI arg parsing has issues with FFI
  '())

(defun args-get-values (args flag)
  "Get all values following a flag until next flag or end"
  (let ((collecting nil)
        (values '()))
    (dolist (arg args)
      (cond
        ((string= arg flag)
         (setq collecting t))
        ((and collecting (char= (char arg 0) #\-))
         (setq collecting nil))
        (collecting
         (push arg values))))
    (nreverse values)))

(defun args-has-flag (args flag)
  "Check if flag is present in args"
  (member flag args :test #'string=))

(defun string-to-keyword (s)
  "Convert string to keyword"
  (intern (string-upcase s) :keyword))

(defun main ()
  "Main entry point"
  (let* ((args (parse-cli-args))
         (categories (args-get-values args "--category"))
         (files (args-get-values args "--file"))
         (quick (args-has-flag args "--quick"))
         (list-cats (args-has-flag args "--list"))
         (quiet (args-has-flag args "--quiet"))
         (json-output (args-has-flag args "--json"))
         (help (or (args-has-flag args "--help")
                   (args-has-flag args "-h"))))

    (cond
      ;; Help
      (help
       (format t "~%Valtan Test Runner~%")
       (format t "==================~%~%")
       (format t "Usage: node dist/tests.js [options]~%~%")
       (format t "Options:~%")
       (format t "  --category CAT [CAT...]  Run specific category tests~%")
       (format t "  --file FILE [FILE...]    Run specific test files~%")
       (format t "  --quick                  Run quick smoke tests only~%")
       (format t "  --list                   List available categories~%")
       (format t "  --quiet                  Less verbose output~%")
       (format t "  --json                   Output results as JSON~%")
       (format t "  --help, -h               Show this help~%~%")
       (format t "Examples:~%")
       (format t "  node dist/tests.js --category cons array~%")
       (format t "  node dist/tests.js --file must-cons.lisp --quiet~%")
       (format t "  node dist/tests.js --quick~%"))

      ;; List categories
      (list-cats
       (list-categories))

      ;; Quick test
      (quick
       (let ((results (quick-test)))
         (when json-output
           (format t "~%~A~%" (results-to-json results)))))

      ;; Run with specific categories
      (categories
       (let ((cat-keywords (mapcar #'string-to-keyword categories)))
         (let ((results (run-tests :categories cat-keywords
                                   :verbose (not quiet))))
           (when json-output
             (format t "~%~A~%" (results-to-json results))))))

      ;; Run specific files
      (files
       (let ((results (run-tests :files files :verbose (not quiet))))
         (when json-output
           (format t "~%~A~%" (results-to-json results)))))

      ;; Default: run all tests
      (t
       (let ((results (run-tests :verbose (not quiet))))
         (when json-output
           (format t "~%~A~%" (results-to-json results))))))))

;; Run main
(main)
