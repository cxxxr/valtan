(defun generate-p2-builtin-function-table ()
  (let ((file (asdf:system-relative-pathname :valtan "./kernel/lisp.js")))
    (with-open-file (in file)
      (let ((*print-case* :downcase))
        (pprint
         `(defparameter compiler::*builtin-function-table*
            (let ((table (make-hash-table)))
              ,@(loop :for line := (read-line in nil nil)
                      :while line
                      :when (ppcre:register-groups-bind (package-name symbol-name fn-name args)
                                ("^registerFunction\\((\\w+),\\s*'([^']*)',\\s*(\\w+),\\s*(.*)"
                                 line)
                              `(setf (gethash
                                      (read-from-string
                                       ,(format nil "~A:~A"
                                                (cond ((string= package-name "cl_package")
                                                       "CL")
                                                      ((string= package-name "system_package")
                                                       "SYSTEM:")
                                                      ((string= package-name "ffi_package")
                                                       "FFI:"))
                                                symbol-name))
                                      table)
                                     (list ,(format nil "lisp.~A" fn-name)
                                           ,(let ((tokens '()))
                                              (ppcre:do-register-groups (token) ("(\\w+)" args)
                                                (push (if (string= token "null")
                                                          nil
                                                          (parse-integer token))
                                                      tokens))
                                              `(list ,@(nreverse tokens))))))
                      :collect :it)
              table)))))))
