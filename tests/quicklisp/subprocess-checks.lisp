(in-package :cl-user)
(portability-check (string= (uiop:escape-sh-token "$literal `text`") "\"\\$literal \\`text\\`\""))
(portability-check (equal (with-input-from-string (s (format nil "one~%two~%"))
                           (uiop:slurp-input-stream :lines s)) '("one" "two")))

#-node
(portability-check
 (handler-case (progn (uiop:run-program '("does-not-run") :output :string) nil)
   (error (c) (not (null (search "requires Node.js" (princ-to-string c)))))))

#+node
(let ((node (ffi:js->cl (ffi:ref "process" "execPath"))))
  (flet ((command (source &rest arguments) (append (list node "-e" source "--") arguments)))
    (let* ((literal "a b ' \" $HOME $(printf forbidden) `echo forbidden` 東京😀")
           (cmd (command "process.stdout.write(process.argv[1])" literal)))
      (dolist (shell '(nil t))
        (multiple-value-bind (output errors status)
            (uiop:run-program cmd :force-shell shell :output :string :error-output :string)
          (portability-check (string= output literal))
          (portability-check (string= errors ""))
          (portability-check (= status 0)))))
    (multiple-value-bind (output errors status)
        (uiop:run-program (command "process.stdout.write('out'); process.stderr.write('err'); process.exitCode=7")
                          :output :string :error-output :string :ignore-error-status t)
      (portability-check (equal (list output errors status) '("out" "err" 7))))
    (portability-check
     (handler-case (progn (uiop:run-program (command "process.exit(9)")) nil)
       (uiop:subprocess-error (c) (= (uiop:subprocess-error-code c) 9))))
    (multiple-value-bind (output errors status)
        (uiop:run-program (command "process.stdout.write('out'); process.stderr.write('err')")
                          :output :string :error-output :output)
      (portability-check (equal (list output errors status) '("outerr" nil 0))))
    (portability-check
     (string= (with-input-from-string (input "stdin 東京😀")
                (uiop:run-program (command "process.stdout.write(require('fs').readFileSync(0))")
                                  :input input :output :string))
              "stdin 東京😀"))
    (portability-check
     (string= (with-output-to-string (output)
                (uiop:run-program (command "process.stdout.write('stream output')") :output output))
              "stream output"))
    (portability-check
     (string= (uiop:run-program (command "console.log('trimmed')") :output '(:string :stripped t))
              "trimmed"))
    (portability-check
     (equal (uiop:run-program (command "console.log('one');console.log('two')") :output :lines)
            '("one" "two")))
    (portability-check
     (= (uiop:run-program (command "process.stdout.write('42')") :output #'read) 42))
    (uiop:with-temporary-file (:pathname file)
      (uiop:run-program (command "process.stdout.write('first')") :output file)
      (uiop:run-program (command "process.stdout.write('+second')") :output file :if-output-exists :append)
      (portability-check (string= (uiop:read-file-string file) "first+second"))
      (portability-check
       (handler-case (progn (uiop:run-program (command "process.stdout.write('bad')")
                                            :output file :if-output-exists :error) nil)
         (file-error () t)))
      (portability-check (string= (uiop:read-file-string file) "first+second"))
      (uiop:run-program (command "process.stdout.write('new')") :output file)
      (portability-check (string= (uiop:read-file-string file) "new"))
      (portability-check
       (string= (uiop:run-program (command "process.stdout.write(require('fs').readFileSync(0))")
                                 :input file :output :string) "new")))
    (let ((directory (uiop:temporary-directory)) (previous (uiop:getcwd)))
      (portability-check
       (equal (truename (uiop:ensure-directory-pathname
                         (uiop:run-program (command "process.stdout.write(process.cwd())")
                                           :directory directory :output :string)))
              (truename directory)))
      (portability-check (equal previous (uiop:getcwd))))
    (portability-check
     (handler-case (progn (uiop:run-program '("valtan-nonexistent-test-command-183729")) nil)
       (error () t)))))
(write-line "PORTABILITY-SUBPROCESS-PASS")
