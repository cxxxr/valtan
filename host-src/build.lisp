(defpackage :valtan-host.build
  (:use :cl)
  (:export :build-system
           :build-application
           :run-node
           :run-build-server))
(in-package :valtan-host.build)

(defvar *cache-directory*)
(defvar *cache* (make-hash-table :test 'equal))
(defvar *discard-cache* nil)

(defun cache-date (cache)
  (car cache))

(defun cache-output-file (cache)
  (cdr cache))

(defun make-cache (date output-file)
  (cons date output-file))

(defun latest-cache-p (cache file)
  (eql (cache-date cache)
       (file-write-date file)))

(defun invoke-compile-file-with-cache (input-file compile-fn &key (cache-key input-file))
  (flet ((main ()
           (let ((output-file (funcall compile-fn)))
             (setf (gethash cache-key *cache*)
                   (make-cache (file-write-date input-file)
                               output-file))
             output-file)))
    (let ((cache (gethash cache-key *cache*)))
      (cond ((or (null cache) *discard-cache*)
             (main))
            ((not (latest-cache-p cache input-file))
             (setq *discard-cache* t)
             (main))
            (t
             (let ((output-file (cache-output-file cache)))
               (format t "~&cache ~A~%" output-file)
               output-file))))))

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
        :do (if var
                (format stream
                        "var ~A = require('~A');~%"
                        (compiler::p2-convert-var var)
                        module)
                (format stream
                        "require('~A');~%"
                        module)))
  (compiler::p2-toplevel-forms hir-forms stream)
  (values))

(defun input-file-to-output-file (input-file &optional (*cache-directory* *cache-directory*))
  (make-pathname :directory (append *cache-directory*
                                    (rest (pathname-directory input-file)))
                 :name (pathname-name input-file)
                 :type (format nil "~A.js" (pathname-type input-file))))

(defun !compile-file (input-file)
  (%with-compilation-unit ()
    (let ((hir-forms
            (let ((hir-forms '())
                  (compiler::*export-modules* '())
                  (compiler::*macro-definitions* '())
                  (*package* (find-package :valtan-user)))
              (do-file-form (form input-file)
                (push (handler-bind ((warning #'muffle-warning))
                        (compiler::pass1-toplevel-usign-optimize form))
                      hir-forms))
              (list* (compiler::pass1-toplevel '(in-package "CL-USER"))
                     (compiler::pass1-module input-file
                                             (nreverse hir-forms)
                                             compiler::*export-modules*)
                     (compiler::pass1-dump-macros compiler::*macro-definitions*)))))
      (let ((output-file (input-file-to-output-file input-file)))
        (ensure-directories-exist output-file)
        (with-write-file (out output-file)
          (in-pass2 hir-forms out))
        output-file))))

(defun compile-file-with-cache (input-file)
  (invoke-compile-file-with-cache
   input-file
   (lambda ()
     (!compile-file input-file))))

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
    (let ((output-file (input-file-to-output-file (valtan-host.system:system-to-pathname system))))
      (ensure-directories-exist output-file)
      (with-write-file (out output-file)
        (write-line "import * as lisp from 'lisp';" out)
        (dolist (system-name (valtan-host.system:system-depends-on system))
          (let* ((dependent-system (valtan-host.system:find-system system-name)) ;!!!
                 (path (resolve-path (valtan-host.system:system-to-pathname system)
                                     (valtan-host.system:system-to-pathname dependent-system))))
            (format out "require('~A.js');~%" path)))
        (dolist (pathname (valtan-host.system:system-pathnames system))
          (let ((path (resolve-path (valtan-host.system:system-to-pathname system)
                                    pathname)))
            (format out "require('~A.js');~%" path))))
      output-file)))

(defun cache-system-key (system)
  (valtan-host.system:system-name system))

(defun compile-system-file-with-cache (system)
  (let ((input-file (valtan-host.system:system-pathname system)))
    (invoke-compile-file-with-cache
     input-file
     (lambda ()
       (compile-system-file system))
     :cache-key (cache-system-key system))))

(defun check-discarting-cache-system-file (system)
  (let* ((input-file (valtan-host.system:system-pathname system))
         (cache (gethash (cache-system-key system) *cache*)))
    (and cache (not (latest-cache-p cache input-file)))))

(defun create-entry-file (system)
  (let ((output-file (make-pathname :type "js"
                                    :name (valtan-host.system:system-name system)
                                    :directory (pathname-directory
                                                (valtan-host.system:system-pathname system)))))
    (with-write-file (out output-file)
      (format out
              "require('~A');~%"
              (input-file-to-output-file (valtan-host.system:system-to-pathname system)))
      (compiler::p2-toplevel-forms
       (list (compiler::pass1-toplevel '(cl:finish-output) out))
       out))))

(defun prepare-valtan-path (base-directory)
  (with-open-file (out (make-pathname :name ".valtan-path"
                                      :directory base-directory)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ (asdf:system-relative-pathname :valtan "./kernel/") out)))

(defun build-system-using-system (system &key force)
  (let* ((base-directory (pathname-directory
                          (valtan-host.system:system-pathname system)))
         (*cache-directory* (append base-directory
                                    (list ".valtan-cache")))
         (*discard-cache* (if force t nil))
         (*features* *features*))
    (pushnew :valtan *features*)
    (when (eql :node (valtan-host.system:system-target system))
      (pushnew :node *features*))
    (prepare-valtan-path base-directory)
    (dolist (system (valtan-host.system:compute-system-precedence-list system))
      (when (check-discarting-cache-system-file system)
        (setq *discard-cache* t))
      (dolist (pathname (valtan-host.system:system-pathnames system))
        (compile-file-with-cache pathname))
      (compile-system-file-with-cache system))
    (create-entry-file system)))

(defun ensure-system-file (pathname)
  (let ((probed-pathname (probe-file pathname)))
    (unless probed-pathname
      (error "~A does not exist" pathname))
    (unless (valtan-host.system:system-file-p pathname)
      (error "~A is not a system file" pathname))
    probed-pathname))

(defun build-system (pathname &key force)
  (let* ((pathname (ensure-system-file pathname))
         (system (valtan-host.system:load-system-file pathname)))
    (build-system-using-system system :force force)))

(defun webpack (directory)
  (uiop:run-program (list "./node_modules/.bin/webpack")
                    :directory directory
                    :output t
                    :error-output t
                    :ignore-error-status t))

(defun build-application (pathname &key force)
  (let ((directory (make-pathname :directory (pathname-directory pathname))))
    (build-system pathname :force force)
    (webpack directory)))

(defun run-node (pathname)
  (build-system pathname)
  (let ((directory (make-pathname :directory (pathname-directory pathname))))
    (webpack directory)
    (format t "~&==================================================~%")
    (uiop:run-program (list "node" "./dist/main.js")
                      :directory directory
                      :output t
                      :error-output t
                      :ignore-error-status t))
  (values))

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
               (block exit
                 (handler-bind ((error (lambda (condition)
                                         (format t "~&~%~A~%" condition)
                                         (uiop:print-backtrace :condition condition)
                                         (return-from exit))))
                   (build-system-using-system system))
                 (webpack system-directory))))
        (build)
        (loop
          (inotify:with-inotify (inot paths-with-masks)
            (inotify:read-events inot)
            (build)))))))
