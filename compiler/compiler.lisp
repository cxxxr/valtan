(in-package :compiler)

(defmacro system::quasiquote (x)
  (expand-quasiquote x))

(defparameter *known-function-names*
  (let ((file (asdf:system-relative-pathname :valtan "./lib/lisp.js")))
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
     (lambda (s c n)
       (declare (ignore c n))
       (case (peek-char nil s)
         (#\:
          (read-char s)
          (let ((tokens (read-js-ident s)))
            `(ffi:ref ,@tokens)))
         (#\[
          (read-js-array s))
         (#\{
          (read-js-object s))
         (otherwise
          (let ((form (read s t nil t)))
            `(ffi:cl->js ,form))))))
    *readtable*))

(defun non-terminate-macro-character-p (c)
  (multiple-value-bind (function non-terminating-p)
      (get-macro-character c)
    (and function (not non-terminating-p))))

(defparameter *whitespaces* '(#\space #\tab #\newline #\linefeed #\page #\return))

(defun whitespacep (c)
  (find c *whitespaces*))

(defun delimiter-p (c)
  (or (null c)
      (whitespacep c)
      (non-terminate-macro-character-p c)
      (char= c #\\)
      (char= c #\|)
      (char= c #\:)))

(defun read-js-ident-1 (in)
  (with-output-to-string (out)
    (loop :for c := (peek-char nil in nil nil)
          :while c
          :do (cond ((or (alphanumericp c) (char= c #\_))
                     (write-char c out)
                     (read-char in))
                    ((delimiter-p c)
                     (return))
                    (t
                     (error "invalid character: ~S" c))))))

(defun read-js-ident (in)
  (loop :for token := (read-js-ident-1 in)
        :if (string/= token "") :collect token :into tokens
        :do (case (peek-char nil in nil nil)
              (#\: (read-char in))
              (otherwise (return tokens)))))

(defun read-js-array (in)
  (flet ((read-js-array (s c)
           (declare (ignore c))
           `(ffi::array ,@(read-delimited-list #\] s t))))
    (let ((*readtable* (copy-readtable *js-readtable*)))
      (set-macro-character #\[ #'read-js-array)
      (set-macro-character #\] (get-macro-character #\)) nil)
      (read in t nil t))))

(defun read-js-object (in)
  (labels ((read-js-object-1 (s c)
             (declare (ignore c))
             (let ((plist '()))
               (loop
                 (when (eq #\} (peek-char t s t nil t))
                   (read-char s t nil t)
                   (return))
                 (let ((key (read s t nil t))
                       (value (read s t nil t)))
                   (push key plist)
                   (push value plist)))
               `(ffi:object ,@(nreverse plist)))))
    (let ((*readtable* (copy-readtable *js-readtable*)))
      (set-macro-character #\{ #'read-js-object-1)
      (set-macro-character #\} (get-macro-character #\)) nil)
      (read in t nil t))))

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
    `(let ((*package* (find-package :valtan-user)))
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
  (let ((*defined-function-names* '())
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
  (let ((base-path (asdf:system-relative-pathname :valtan base-dir)))
    (mapcar (lambda (name)
              (make-pathname :name name :type "lisp" :defaults base-path))
            files)))

(defun get-lisp-files ()
  (append (directory-files "./lisp/"
                           '("control"
                             "setf"
                             "destructuring-bind"
                             "ffi"
                             "cons"
                             "condition"
                             "struct"
                             "symbol"
                             "type"
                             "number"
                             "array"
                             "character"
                             "string"
                             "function"
                             "sequence"
                             "hashtable"
                             "package"
                             "stream"
                             "print"
                             "read"
                             "file"
                             "pkg"
                             "clos"
                             "restart"))
          (directory-files "./compiler/"
                           '("packages"
                             "variables"
                             "util"
                             "error"
                             "ir"
                             "pass1"
                             "pass2"))
          (directory-files "./lisp/"
                           '("compilation"))))

(defun build (pathnames output)
  (let ((*standard-output* output))
    (compile-files pathnames)))

(defparameter *module-table* (make-hash-table))
(defvar *module-directory*)

(defmacro valtan-system::module (name file-names)
  `(setf (gethash ',name *module-table*)
         (mapcar (lambda (file-name)
                   (make-pathname :name file-name
                                  :type "lisp"
                                  :directory *module-directory*))
                 ',file-names)))

(defun load-module (pathname)
  (let ((*module-directory* (pathname-directory (probe-file pathname)))
        (*package* (find-package :valtan-system)))
    (load pathname)))

(defun build-system (pathname &key output-file)
  (unless (probe-file pathname)
    (error "~A does not exist" pathname))
  (let ((pathname (probe-file pathname))
        (*module-table* (make-hash-table))
        (*in-host-runtime* t))
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
                   (let ((ir-forms '())
                         (*require-modules* '()))
                     (dolist (file (get-lisp-files))
                       (do-file-form (form file)
                         (push (pass1-toplevel form) ir-forms)))
                     (dolist (ir-form (pass1-dump-macros))
                       (push ir-form ir-forms))
                     (let ((macro-definitions
                             (let ((*macro-definitions* nil))
                               (dolist (file pathnames)
                                 (do-file-form (form file)
                                   (push (pass1-toplevel form) ir-forms)))
                               *macro-definitions*)))
                       (dolist (ir-form (pass1-dump-macros macro-definitions))
                         (push ir-form ir-forms))
                       (let ((*standard-output* output))
                         (in-pass2 (nreverse ir-forms))))))
                 *module-table*)))))
