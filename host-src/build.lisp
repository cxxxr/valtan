(defpackage :valtan-host.build
  (:use :cl)
  (:export :build-system))
(in-package :valtan-host.build)

(defparameter *known-function-names*
  (let ((file (asdf:system-relative-pathname :valtan "./kernel/lisp.js")))
    (with-open-file (in file)
      (loop :for line := (read-line in nil nil)
            :while line
            :when (eql 0 (search "registerFunction(" line))
            :collect (let* ((start (position #\' line))
                            (end (position #\' line :start (1+ start))))
                       (subseq line (1+ start) end))))))

(defmacro do-forms ((var stream) &body body)
  (let ((g-eof-value (gensym))
        (g-stream (gensym)))
    `(let ((*package* (find-package :valtan-user)))
       (loop :with ,g-eof-value := '#:eof-value
             :and ,g-stream := ,stream
             :for ,var := (valtan-host.reader:read-in-valtan ,g-stream nil ,g-eof-value)
             :until (eq ,var ,g-eof-value)
             :do (progn ,@body)))))

(defmacro do-file-form ((var file) &body body)
  (let ((in (gensym)))
    `(with-open-file (,in ,file)
       (do-forms (,var ,in)
         ,@body))))

(defun in-pass2 (hir-forms)
  (write-line "import * as lisp from 'lisp';")
  (loop :for (var . module) :in compiler::*require-modules*
        :do (format t "var ~A = require('~A');~%" (compiler::p2-convert-var var) module))
  (compiler::p2-toplevel-forms hir-forms *standard-output*)
  (values))

(defun !compile-file (file hir-forms)
  (let ((file-hir-forms '())
        (compiler::*export-modules* '()))
    (do-file-form (form file)
      (push (compiler::pass1-toplevel-usign-optimize form) file-hir-forms))
    (push (compiler::pass1-module file (nreverse file-hir-forms) compiler::*export-modules*)
          hir-forms)
    hir-forms))

(defun compile-with-system-1 (system hir-forms)
  (let ((compiler::*macro-definitions* nil))
    (dolist (file (valtan-host.system::system-pathnames system))
      (setq hir-forms (!compile-file file hir-forms)))
    (dolist (hir-form (compiler::pass1-dump-macros compiler::*macro-definitions*))
      (push hir-form hir-forms))
    hir-forms))

(defun compile-with-system (system)
  (let ((hir-forms '()))
    (handler-bind ((warning #'muffle-warning))
      (when compiler::*enable-profiling*
        (push (compiler::pass1-toplevel '((ffi:ref "lisp" "startProfile"))) hir-forms))
      (dolist (system (valtan-host.system::compute-system-precedence-list system))
        (setq hir-forms (compile-with-system-1 system hir-forms)))
      (when compiler::*enable-profiling*
        (push (compiler::pass1-toplevel '((ffi:ref "lisp" "finishProfile"))) hir-forms))
      (nreverse hir-forms))))

(defmacro %with-compilation-unit (options &body body)
  (declare (ignore options))
  `(let ((compiler::*require-modules* '())
         (compiler::*genvar-counter* 0)
         (*gensym-counter* 0)
         (*known-function-names* '())
         (compiler::*known-toplevel-functions* '()))
     ,@body))

(defun build-system-using-system (system)
  (let ((output-file (make-pathname :name (pathname-name (valtan-host.system::system-pathname system))
                                    :type "js"
                                    :defaults (valtan-host.system::system-pathname system)))
        (compiler::*in-host-runtime* t)
        (compiler::*enable-profiling* (valtan-host.system::system-enable-profile system)))
    (%with-compilation-unit ()
      (let ((hir-forms (compile-with-system system)))
        (with-open-file (output output-file
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede)
          (let ((*standard-output* output))
            (in-pass2 hir-forms)))))))

(defun ensure-system-file (pathname)
  (let ((pathname* (probe-file pathname)))
    (unless pathname*
      (error "~A does not exist" pathname))
    (unless (equal (pathname-type pathname) "system")
      (error "~A is not a system file" pathname))
    pathname*))

(defun build-system (pathname)
  (let* ((pathname (ensure-system-file pathname))
         (system (valtan-host.system::load-system pathname)))
    (build-system-using-system system)))

(defun all-directories-to-notify (system)
  (let ((systems (valtan-host.system::compute-system-precedence-list system))
        (directories '()))
    (dolist (system systems)
      (dolist (pathname (valtan-host.system::system-pathnames system))
        (pushnew (make-pathname :directory (pathname-directory pathname))
                 directories
                 :test #'uiop:pathname-equal)))
    directories))

#+linux
(defun run-build-server (pathname)
  (let* ((pathname (ensure-system-file pathname))
         (system-directory (make-pathname :directory (pathname-directory pathname)))
         (system (valtan-host.system::load-system pathname)))
    (let* ((directories (all-directories-to-notify system))
           (paths-with-masks
             (loop :for directory :in directories
                   :collect (list directory inotify:in-modify))))
      (flet ((build ()
               (when (ignore-errors (build-system-using-system system) t)
                 (uiop:run-program (list "./node_modules/.bin/webpack")
                                   :directory system-directory
                                   :output t))))
        (build)
        (loop
          (inotify:with-inotify (inot paths-with-masks)
            (inotify:read-events inot)
            (build)))))))
