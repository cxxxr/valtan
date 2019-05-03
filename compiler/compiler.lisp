(in-package :compiler)

(defmacro do-forms ((var stream) &body body)
  (let ((g-eof-value (gensym))
        (g-stream (gensym)))
    `(loop :with ,g-eof-value := '#:eof-value
           :and ,g-stream := ,stream
           :for ,var := (read ,g-stream nil ,g-eof-value)
           :until (eq ,var ,g-eof-value)
           :do (progn ,@body))))

(defun call-with-compile (function)
  (let ((*require-modules* '())
        (*defined-function-names* '())
        (*called-function-names* '()))
    (let ((ir-forms (funcall function)))
      (write-line "import * as lisp from 'lisp';")
      (dolist (module *require-modules*)
        (format t "require('~A.lisp');~%" module))
      (pass2-toplevel-forms ir-forms))
    (values)))

(defmacro with-compile (() &body body)
  `(call-with-compile (lambda () ,@body)))

(defun compile-stdin ()
  (with-compile ()
    (let ((ir-forms '()))
      (do-forms (form *standard-input*)
        (push (pass1-toplevel form) ir-forms))
      (nreverse ir-forms))))

(defun compile-files (files)
  (unless (listp files) (setf files (list files)))
  (with-compile ()
    (let ((ir-forms '()))
      (dolist (file files)
        (with-open-file (in file)
          (do-forms (form in)
            (push (pass1-toplevel form) ir-forms))))
      (nreverse ir-forms))))

(defun compile-toplevel (form)
  (with-compile ()
    (list (pass1-toplevel form))))

(defmacro with-js-beautify (&body body)
  `(let ((output
           (with-output-to-string (*standard-output*)
             (progn ,@body))))
     (with-input-from-string (in output)
       (uiop:run-program "js-beautify"
                         :input in
                         :output t))))

(defun get-lisp-files ()
  (let ((base-path (asdf:system-relative-pathname :clscript "./lisp/")))
    (mapcar (lambda (name)
              (make-pathname :name name :type "lisp" :defaults base-path))
            '("control" "condition" "print" "cons"))))

(defun build ()
  (compile-files (get-lisp-files)))
