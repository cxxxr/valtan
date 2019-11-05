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

#+(or)
(let ((file (asdf:system-relative-pathname :valtan "./lib/lisp.js")))
  (with-open-file (in file)
    (loop :for line := (read-line in nil nil)
          :while line
          :when (ppcre:register-groups-bind (package-name symbol-name fn-name)
                    ("^registerFunction\\((\\w+),\\s*'([^']*)',\\s*(\\w+)," line)
                  (list (format nil "lisp.~A" fn-name)
                        (format nil "~A:~A"
                                (cond ((string= package-name "cl_package")
                                       "CL:")
                                      ((string= package-name "system_package")
                                       "SYSTEM::")
                                      ((string= package-name "ffi_package")
                                       "FFI::"))
                                symbol-name)))
          :collect :it)))

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

(defun read-in-valtan (&rest args)
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
             :for ,var := (read-in-valtan ,g-stream nil ,g-eof-value)
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

(defparameter *system-directories* (list (asdf:system-relative-pathname :valtan "./library/")))

(defstruct system
  name
  (pathnames '())
  (enable-profile nil)
  (depends-on '()))

(defun load-system (pathname)
  (with-open-file (in pathname)
    (let ((plist (let ((*package* (find-package :valtan-system)))
                   (read in)))
          (directory (pathname-directory pathname)))
      (destructuring-bind (&key members enable-profile depends-on) plist
        (make-system :name (pathname-name pathname)
                     :pathnames (mapcar (lambda (name)
                                          (make-pathname :name name
                                                         :type "lisp"
                                                         :directory directory))
                                        members)
                     :enable-profile enable-profile
                     :depends-on depends-on)))))

(defun find-system (system-name &optional (errorp t))
  (labels ((ok (pathname)
             (when (and (equal (pathname-type pathname) "system")
                        (equal (pathname-name pathname) system-name))
               pathname))
           (f (directory)
             (or (some #'ok (uiop:directory-files directory))
                 (some #'f (uiop:subdirectories directory)))))
    (let ((system-pathname (some #'f *system-directories*)))
      (cond (system-pathname
             (load-system system-pathname))
            (errorp
             (error "System ~S is not found" system-name))))))

(defun compile-with-system-1 (system ir-forms)
  (dolist (system-name (system-depends-on system))
    (let ((system (find-system system-name)))
      (setf ir-forms (compile-with-system-1 system ir-forms))))
  (let ((macro-definitions
          (let ((*macro-definitions* nil))
            (dolist (file (system-pathnames system))
              (do-file-form (form file)
                (push (pass1-toplevel form) ir-forms)))
            *macro-definitions*)))
    (dolist (ir-form (pass1-dump-macros macro-definitions))
      (push ir-form ir-forms)))
  ir-forms)

(defun compile-with-system (system)
  (let ((ir-forms
          (if (system-enable-profile system)
              (list (pass1-toplevel '((ffi:ref "lisp" "startProfile"))))
              '())))
    (dolist (file (get-lisp-files))
      (do-file-form (form file)
        (push (pass1-toplevel form) ir-forms)))
    (dolist (ir-form (pass1-dump-macros))
      (push ir-form ir-forms))
    (nreverse (compile-with-system-1 system ir-forms))))

(defun build-system (pathname &key output-file)
  (unless (probe-file pathname)
    (error "~A does not exist" pathname))
  (let* ((pathname (probe-file pathname))
         (system (load-system pathname))
         (output-file (or output-file
                          (make-pathname :name (pathname-name pathname)
                                         :type "js"
                                         :defaults pathname)))
         (*in-host-runtime* t)
         (*require-modules* '()))
    (with-open-file (output output-file
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (let ((ir-forms (compile-with-system system))
            (*standard-output* output))
        (in-pass2 ir-forms)))))
