(defpackage :valtan-host.build
  (:use :cl)
  (:export :build-system))
(in-package :valtan-host.build)

(defvar *cache-directory*)

(defmacro %with-compilation-unit (() &body body)
  `(let ((compiler::*in-host-runtime* t)
         (compiler::*require-modules* '())
         (compiler::*known-toplevel-functions* '())
         (compiler::*genvar-counter* 0)
         (*gensym-counter* 0))
     ,@body))

(defmacro with-write-file ((stream pathname) &body body)
  (let ((g-pathname (gensym)))
    `(let ((,g-pathname ,pathname))
       (format t "creating ~A~%" ,g-pathname)
       (with-open-file (,stream ,g-pathname
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
         ,@body))))

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

(defun in-pass2 (hir-forms &optional (stream *standard-output*))
  (write-line "import * as lisp from 'lisp';" stream)
  (loop :for (var . module) :in compiler::*require-modules*
        :do (format stream "var ~A = require('~A');~%" (compiler::p2-convert-var var) module))
  (compiler::p2-toplevel-forms hir-forms stream)
  (values))

(defun input-file-to-output-file (input-file &optional (*cache-directory* *cache-directory*))
  (make-pathname :directory (append *cache-directory*
                                    (rest (pathname-directory input-file)))
                 :name (pathname-name input-file)
                 :type (format nil "~A.js" (pathname-type input-file))))

(defun !compile-file (input-file)
  (%with-compilation-unit ()
    (let ((module
            (let ((hir-forms '())
                  (compiler::*export-modules* '()))
              (do-file-form (form input-file)
                (push (handler-bind ((warning #'muffle-warning))
                        (compiler::pass1-toplevel-usign-optimize form))
                      hir-forms))
              (compiler::pass1-module input-file (nreverse hir-forms) compiler::*export-modules*))))
      (let ((output-file (input-file-to-output-file input-file)))
        (ensure-directories-exist output-file)
        (with-write-file (out output-file)
          (in-pass2 (list module) out))
        output-file))))

(defun common-directory (pathname1 pathname2)
  (loop :for dir1 := (pathname-directory pathname1) :then (rest dir1)
        :for dir2 := (pathname-directory pathname2) :then (rest dir2)
        :until (or (not (equal (first dir1) (first dir2)))
                   (null dir1)
                   (null dir2))
        :finally (return (values dir1 dir2))))

(defun resolve-path (pathname1 pathname2)
  (multiple-value-bind (directory1 directory2)
      (common-directory pathname1 pathname2)
    (make-pathname :directory
                   `(:relative
                     "."
                     ,@(loop :repeat (length directory1) :collect :up)
                     ,@directory2)
                   :name (pathname-name pathname2)
                   :type (pathname-type pathname2))))

(defun compile-system-file (system)
  (%with-compilation-unit ()
    (let ((output-file (input-file-to-output-file (valtan-host.system:system-pathname system))))
      (with-write-file (out output-file)
        (write-line "import * as lisp from 'lisp';" out)
        (dolist (system-name (valtan-host.system:system-depends-on system))
          (let* ((dependent-system (valtan-host.system:find-system system-name)) ;!!!
                 (path (resolve-path (valtan-host.system:system-pathname system)
                                     (valtan-host.system:system-pathname dependent-system))))
            (format out "require('~A.js');~%" path)))
        (dolist (pathname (valtan-host.system:system-pathnames system))
          (let ((path (resolve-path (valtan-host.system:system-pathname system)
                                    pathname)))
            (format out "require('~A.js');~%" path)))
        (compiler::p2-toplevel-forms (compiler::pass1-dump-macros compiler::*macro-definitions*)
                                     out)))))

(defun create-entry-file (system)
  (let ((output-file (make-pathname :type "js"
                                    :name (valtan-host.system:system-name system)
                                    :directory (pathname-directory (valtan-host.system:system-pathname system)))))
    (with-write-file (out output-file)
      (format out
              "require('~A');~%"
              (input-file-to-output-file (valtan-host.system:system-pathname system))))))

(defun build-system-using-system (system)
  (let ((*cache-directory* (append (pathname-directory
                                    (valtan-host.system:system-pathname system))
                                   (list "valtan-cache"))))
    (dolist (system (valtan-host.system:compute-system-precedence-list system))
      (let ((compiler::*macro-definitions* '()))
        (dolist (pathname (valtan-host.system:system-pathnames system))
          (!compile-file pathname))
        (compile-system-file system)))
    (create-entry-file system)))

(defun ensure-system-file (pathname)
  (let ((probed-pathname (probe-file pathname)))
    (unless probed-pathname
      (error "~A does not exist" pathname))
    (unless (equal (pathname-type pathname) "system")
      (error "~A is not a system file" pathname))
    probed-pathname))

(defun build-system (pathname)
  (let* ((pathname (ensure-system-file pathname))
         (system (valtan-host.system:load-system pathname)))
    (build-system-using-system system)))

(defun all-directories-to-notify (system)
  (let ((systems (valtan-host.system:compute-system-precedence-list system))
        (directories '()))
    (dolist (system systems)
      (dolist (pathname (valtan-host.system:system-pathnames system))
        (pushnew (make-pathname :directory (pathname-directory pathname))
                 directories
                 :test #'uiop:pathname-equal)))
    directories))

#+linux
(defun run-build-server (pathname)
  (let* ((pathname (ensure-system-file pathname))
         (system-directory (make-pathname :directory (pathname-directory pathname)))
         (system (valtan-host.system:load-system pathname)))
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
