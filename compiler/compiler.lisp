(in-package :compiler)

(defparameter *known-function-names*
  '("VALUES"
    "MULTIPLE-VALUE-CALL"
    "EQ"
    "EQL"
    "FUNCALL"
    "APPLY"
    "SYMBOL-PLIST"
    "BOUNDP"
    "SYMBOL-FUNCTION"
    "CONSP"
    "CONS"
    "CAR"
    "CDR"
    "RPLACA"
    "RPLACD"
    "+"
    "-"
    "*"
    "="
    ">"
    "<"
    ">="
    "<="
    "INTERN"
    "COPY-STRUCTURE"))

(defvar *quasiquote-readtable*
  (let ((*readtable* (copy-readtable)))
    (set-macro-character
     #\`
     (lambda (s c)
       (declare (ignore c))
       (list 'system::quasiquote
             (read s t nil t))))
    (set-macro-character
     #\,
     (lambda (s c)
       (declare (ignore c))
       (cond ((eql #\@ (peek-char nil s t nil t))
              (read-char s t nil t)
              (list 'system::unquote-splicing (read s t nil t)))
             (t
              (list 'system::unquote
                    (read s t nil t))))))
    *readtable*))

(defmacro do-forms ((var stream) &body body)
  (let ((g-eof-value (gensym))
        (g-stream (gensym)))
    `(let ((*package* (find-package :cl-user)))
       (loop :with ,g-eof-value := '#:eof-value
             :and ,g-stream := ,stream
             :for ,var := (let ((*readtable* *quasiquote-readtable*))
                            (read ,g-stream nil ,g-eof-value))
             :until (eq ,var ,g-eof-value)
             :do (progn ,@body)))))

(defun call-with-compile (function)
  (let ((*require-modules* '())
        (*defined-function-names* '())
        (*called-function-names* '()))
    (let ((ir-forms (funcall function)))
      (dolist (name (set-difference
                     (set-difference *called-function-names* *defined-function-names*)
                     *known-function-names*
                     :test #'string=))
        (unless (or (eq (symbol-package name) (find-package :system))
                    (eq (symbol-package name) (find-package :ffi)))
          (warn "undefined function: ~S" name)))
      (write-line "import * as lisp from 'lisp';")
      (loop :for (var . module) :in *require-modules*
            :do (format t "var ~A = require('~A');~%" (to-js-identier var) module))
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

(defun directory-files (base-dir files)
  (let ((base-path (asdf:system-relative-pathname :clscript base-dir)))
    (mapcar (lambda (name)
              (make-pathname :name name :type "lisp" :defaults base-path))
            files)))

(defun get-lisp-files ()
  (directory-files "./lisp/"
                   '("ffi"
                     "control"
                     "condition"
                     "print"
                     "cons"
                     "symbol"
                     "type"
                     "number"
                     "struct"
                     "array"
                     "hashtable")))

(defun build (pathnames &optional output)
  (with-open-stream (*standard-output*
                     (or output
                         (make-string-output-stream)))
    (compile-files pathnames)
    (unless output
      (get-output-stream-string *standard-output*))))

(defun build-self (&optional output)
  (build (append (get-lisp-files)
                 (directory-files "./compiler/"
                                  '("util"
                                    "error"
                                    "ir"
                                    "pass1"
                                    "pass2")))
         output))

(defparameter *module-table* (make-hash-table))
(defvar *module-directory*)

(defmacro clscript-system::module (name file-names)
  `(setf (gethash ',name *module-table*)
         (mapcar (lambda (file-name)
                   (make-pathname :name file-name
                                  :type "lisp"
                                  :directory *module-directory*))
                 ',file-names)))

(defun load-module (pathname)
  (let ((*module-directory* (pathname-directory (probe-file pathname)))
        (*package* (find-package :clscript-system)))
    (load pathname)))

(defun build-system (pathname &key output-file)
  (unless (probe-file pathname)
    (error "~A does not exist" pathname))
  (let ((pathname (probe-file pathname))
        (*module-table* (make-hash-table)))
    (load-module pathname)
    (let ((output-file (or output-file
                           (make-pathname :name (pathname-name pathname)
                                          :type "js"
                                          :defaults pathname))))
      (with-open-file (output output-file
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede)
        ;; XXX: *module-table*には要素が一つしか入っていないことを想定
        (maphash (lambda (module-name pathnames)
                   (declare (ignore module-name))
                   (build (append (get-lisp-files) pathnames) output))
                 *module-table*)))))
