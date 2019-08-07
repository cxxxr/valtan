(in-package :compiler)

(defmacro system::quasiquote (x)
  (expand-quasiquote x))

(defparameter *known-function-names*
  (let ((file (asdf:system-relative-pathname :clscript "./lib/lisp.js")))
    (with-open-file (in file)
      (loop :for line := (read-line in nil nil)
            :while line
            :when (eql 0 (search "registerFunction(" line))
            :collect (let* ((start (position #\' line))
                            (end (position #\' line :start (1+ start))))
                       (subseq line (1+ start) end))))))

(defvar *js-readtable*
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
    (set-dispatch-macro-character
     #\# #\j
     (lambda (s c1 c2)
       (declare (ignore c1 c2))
       (let ((old (readtable-case *readtable*)))
         (unwind-protect
              (progn
                (setf (readtable-case *readtable*) :invert)
                (assert (char= #\: (read-char s t nil t)))
                (loop :with acc := '() :and names = '()
                      :for c := (peek-char nil s t nil t)
                      :while (or (alphanumericp c) (member c '(#\_ #\:)))
                      :do (case c
                            (#\: (push (coerce (nreverse acc) 'string)
                                       names)
                                 (setq acc nil))
                            (otherwise (push c acc)))
                          (read-char s t nil t)
                      :finally (return `(ffi:ref ,@(nreverse
                                                    (cons (coerce (nreverse acc) 'string)
                                                          names))))))
           (setf (readtable-case *readtable*) old)))))
    *readtable*))

(defun !read (&rest args)
  (let ((*readtable* *js-readtable*))
    (handler-bind ((sb-int:simple-reader-package-error
                     (lambda (condition)
                       (let ((name (first (simple-condition-format-arguments condition)))
                             (package (slot-value condition 'package)))
                         (export (intern name package) package)
                         (continue condition)))))
      (apply #'read args))))

(defmacro do-forms ((var stream) &body body)
  (let ((g-eof-value (gensym))
        (g-stream (gensym)))
    `(let ((*package* (find-package :cl-user)))
       (loop :with ,g-eof-value := '#:eof-value
             :and ,g-stream := ,stream
             :for ,var := (!read ,g-stream nil ,g-eof-value)
             :until (eq ,var ,g-eof-value)
             :do (progn ,@body)))))

(defmacro do-file-form ((var file) &body body)
  (let ((in (gensym)))
    `(with-open-file (,in ,file)
       (do-forms (,var ,in)
         ,@body))))

(defun in-pass2 (ir-forms)
  (let ((*require-modules* '())
        (*defined-function-names* '())
        (*called-function-names* '()))
    (dolist (name (set-difference
                   (set-difference *called-function-names* *defined-function-names*)
                   *known-function-names*
                   :test #'string=))
      (unless (or (eq (symbol-package name) (find-package :system))
                  (eq (symbol-package name) (find-package :ffi)))
        (warn "undefined function: ~S" name)))
    (write-line "import * as lisp from 'lisp';")
    (loop :for (var . module) :in *require-modules*
          :do (format t "var ~A = require('~A');~%" (pass2-convert-var var) module))
    (pass2-toplevel-forms ir-forms)
    (values)))

(defun compile-stdin ()
  (let ((ir-forms '()))
    (do-forms (form *standard-input*)
      (push (pass1-toplevel form) ir-forms))
    (in-pass2 (nreverse ir-forms))))

(defun compile-files (files)
  (unless (listp files) (setf files (list files)))
  (let ((ir-forms '()))
    (dolist (file files)
      (with-open-file (in file)
        (do-forms (form in)
          (push (pass1-toplevel form) ir-forms))))
    (in-pass2 (nreverse ir-forms))))

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

(defun get-lisp-files (&key self)
  (append (directory-files "./lisp/"
                           '("control"
                             "setf"
                             "destructuring-bind"
                             "ffi"
                             "condition"
                             "struct"
                             "cons"
                             "symbol"
                             "type"
                             "number"
                             "character"
                             "array"
                             "string"
                             "function"
                             "sequence"
                             "hashtable"
                             "package"
                             "stream"
                             "print"
                             "read"
                             "file"
                             "pkg"))
          (when self
            (directory-files "./compiler/"
                             '("packages"
                               "util"
                               "error"
                               "ir"
                               "pass1"
                               "pass2")))
          (directory-files "./lisp/"
                           '("compilation"))))

(defun build (pathnames output)
  (let ((*standard-output* output))
    (compile-files pathnames)))

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
                   (let ((ir-forms '()))
                     (dolist (file (get-lisp-files :self t))
                       (do-file-form (form file)
                         (push (pass1-toplevel form) ir-forms)))
                     (dolist (ir-form (pass1-dump-macros))
                       (push ir-form ir-forms))
                     (dolist (file pathnames)
                       (do-file-form (form file)
                         (push (pass1-toplevel form) ir-forms)))
                     (let ((*standard-output* output))
                       (in-pass2 (nreverse ir-forms)))))
                 *module-table*)))))
