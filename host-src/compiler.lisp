(in-package :compiler)

(defparameter *known-function-names*
  (let ((file (asdf:system-relative-pathname :valtan "./kernel/lisp.js")))
    (with-open-file (in file)
      (loop :for line := (read-line in nil nil)
            :while line
            :when (eql 0 (search "registerFunction(" line))
            :collect (let* ((start (position #\' line))
                            (end (position #\' line :start (1+ start))))
                       (subseq line (1+ start) end))))))

#+(or)
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
            table))))))

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

(defun in-pass2 (hir-forms)
  (write-line "import * as lisp from 'lisp';")
  (loop :for (var . module) :in *require-modules*
        :do (format t "var ~A = require('~A');~%" (p2-convert-var var) module))
  (p2-toplevel-forms hir-forms *standard-output*)
  (values))

(defun report-undefined-functions ()
  (dolist (name (set-difference
                 (set-difference *called-function-names* *defined-function-names*)
                 *known-function-names*
                 :test #'string=))
    (unless (or (eq (symbol-package name) (find-package :system))
                (eq (symbol-package name) (find-package :ffi)))
      (warn "undefined function: ~S" name))))

(defun js-beautify (text &optional (output *standard-output*))
  (with-input-from-string (in text)
    (uiop:run-program "js-beautify"
                      :input in
                      :output output)))

(defmacro with-js-beautify (&body body)
  `(let ((output
           (with-output-to-string (*standard-output*)
             (progn ,@body))))
     (js-beautify output)))

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
          (directory (pathname-directory pathname))
          (system-name (pathname-name pathname)))
      (destructuring-bind (&key members enable-profile depends-on) plist
        (make-system :name system-name
                     :pathnames (mapcar (lambda (name)
                                          (let ((file
                                                  (probe-file
                                                   (make-pathname :name name
                                                                  :type "lisp"
                                                                  :directory directory))))
                                            (unless file
                                              (error "~S not found for system ~S"
                                                     name
                                                     system-name))
                                            file))
                                        members)
                     :enable-profile enable-profile
                     :depends-on (cons "valtan" depends-on))))))

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

(defun compute-system-precedence-list (system)
  (let ((seen (make-hash-table :test 'equal))
        (systems '()))
    (labels ((f (system)
               (unless (gethash (system-name system) seen)
                 (setf (gethash (system-name system) seen) t)
                 (dolist (system-name (system-depends-on system))
                   (f (find-system system-name)))
                 (push system systems))))
      (f system)
      (nreverse systems))))

(defparameter *hir-optimize* t)

(defun pass1-toplevel-usign-optimize (form)
  (let ((hir (pass1-toplevel form)))
    (if *hir-optimize*
        (hir-optimize hir)
        hir)))

(defun compile-with-system-1 (system hir-forms)
  (let ((macro-definitions
          (let ((*macro-definitions* nil))
            (dolist (file (system-pathnames system))
              (let ((file-hir-forms '())
                    (*export-modules* '()))
                (do-file-form (form file)
                  (push (pass1-toplevel-usign-optimize form) file-hir-forms))
                (push (pass1-module file (nreverse file-hir-forms) *export-modules*)
                      hir-forms)))
            *macro-definitions*)))
    (dolist (hir-form (pass1-dump-macros macro-definitions))
      (push hir-form hir-forms)))
  hir-forms)

(defun compile-with-system (system)
  (let ((hir-forms '()))
    (when *enable-profiling*
      (push (pass1-toplevel '((ffi:ref "lisp" "startProfile"))) hir-forms))
    (dolist (system (compute-system-precedence-list system))
      (setq hir-forms (compile-with-system-1 system hir-forms)))
    (when *enable-profiling*
      (push (pass1-toplevel '((ffi:ref "lisp" "finishProfile"))) hir-forms))
    (nreverse hir-forms)))

(defmacro %with-compilation-unit (options &body body)
  (declare (ignore options))
  `(let ((*require-modules* '())
         (*genvar-counter* 0)
         (*gensym-counter* 0)
         (*defined-function-names* '())
         (*called-function-names* '())
         (*known-function-names* '())
         (*known-toplevel-functions* '()))
     ,@body))

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
         (*enable-profiling* (system-enable-profile system)))
    (%with-compilation-unit ()
      (with-open-file (output output-file
                              :direction :output
                              :if-does-not-exist :create
                              :if-exists :supersede)
        (let ((hir-forms (compile-with-system system))
              (*standard-output* output))
          (report-undefined-functions)
          (in-pass2 hir-forms))))))
