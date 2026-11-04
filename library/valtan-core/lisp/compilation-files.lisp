(in-package :common-lisp)

(defvar *compile-file-pathname* nil)
(defvar *compile-file-truename* nil)
(defvar *compile-print* nil)
(defvar *compile-verbose* nil)
(defvar *load-pathname* nil)
(defvar *load-truename* nil)
(defvar *load-print* nil)
(defvar *load-verbose* nil)
(defvar *modules* nil)

(defmacro with-compilation-unit ((&key override) &body body)
  (let ((value (gensym)))
    `(let ((,value ,override)) (declare (ignore ,value)) ,@body)))

(defun provide (module-name)
  (pushnew (string module-name) *modules* :test #'string=)
  t)

(defun require (module-name &optional pathnames)
  (let ((name (string module-name)))
    (unless (member name *modules* :test #'string=)
      (unless pathnames (error "No provider is registered for module ~A" name))
      (dolist (path (if (listp pathnames) pathnames (list pathnames))) (load path))))
  nil)

(defun compile-file-pathname (input-file &key output-file &allow-other-keys)
  (let ((input (merge-pathnames (pathname input-file))))
    (if output-file
        (merge-pathnames (pathname output-file) input)
        (make-pathname :type "fasl" :defaults input))))

(defvar *compiled-file-header* "; Valtan compiled JavaScript, version 1")

(defun %compiled-input-p (stream)
  ;; Probe without consuming the first form of a source stream. Non-positionable
  ;; streams remain ordinary Lisp source streams.
  (let ((position (file-position stream)))
    (when (integerp position)
      (unwind-protect
           (string= (read-line stream nil "") *compiled-file-header*)
        (file-position stream position)))))

(defun %load-stream (stream print)
  (let ((*readtable* (if (%compiled-input-p stream) (copy-readtable nil) *readtable*))
        (eof (gensym)))
    (loop for form = (read stream nil eof) until (eq form eof)
          do (let ((value (eval form)))
               (when print (write value :escape t :readably nil) (terpri)))))
  t)

(defun load (filespec &key (verbose *load-verbose*) (print *load-print*)
                          (if-does-not-exist t) (external-format :default))
  (let* ((supplied-stream (streamp filespec))
         (*package* *package*) (*readtable* *readtable*)
         (*load-pathname* (if supplied-stream
                             (when (file-stream-p filespec) (pathname filespec))
                             (merge-pathnames (pathname filespec))))
         (*load-truename* (when *load-pathname* (probe-file *load-pathname*))))
    (unless (or supplied-stream *load-truename*)
      (if if-does-not-exist
          (error 'file-error :pathname *load-pathname* :message "File does not exist")
          (return-from load nil)))
    (when verbose (format t "; Loading ~A~%" (or *load-pathname* "stream")))
    (if supplied-stream
        (%load-stream filespec print)
        (with-open-file (stream *load-truename* :external-format external-format)
          (%load-stream stream print)))))

(defun %write-compiled-form (code output)
  ;; The artifact contains compiled JavaScript, not the source form or a process
  ;; local function handle. It can be loaded after restarting the runtime.
  (let ((*package* (find-package :cl-user)) (*print-readably* t)
        (*print-escape* t) (*print-pretty* nil) (*print-base* 10)
        (*print-radix* nil) (*print-circle* nil) (*readtable* (copy-readtable nil)))
    (write (list 'ffi:js-eval code) :stream output)
    (terpri output)))

(defun compile-file (input-file &key output-file (verbose *compile-verbose*)
                                     (print *compile-print*) (external-format :default))
  (let* ((*compile-file-pathname* (merge-pathnames (pathname input-file)))
         (*compile-file-truename* (truename *compile-file-pathname*))
         (compiler::*file-compilation-p* t)
         (output-path (compile-file-pathname input-file :output-file output-file))
         (*package* *package*) (*readtable* *readtable*)
         (warnings nil) (failure nil) (eof (gensym)))
    (when verbose (format t "; Compiling ~A~%" *compile-file-truename*))
    (with-open-file (input *compile-file-truename* :external-format external-format)
      (with-open-file (output output-path :direction :output :if-exists :supersede
                                        :if-does-not-exist :create)
        (write-line *compiled-file-header* output)
        (handler-bind ((warning (lambda (condition)
                                  (setq warnings t)
                                  (unless (typep condition 'style-warning) (setq failure t)))))
          (loop for form = (read input nil eof) until (eq form eof)
                do (when print (write form :readably nil) (terpri))
                   (let ((compiler::*macro-definitions* nil))
                     (%write-compiled-form (compiler:compile-toplevel form t nil) output)
                     (when compiler::*macro-definitions*
                       (%write-compiled-form
                        (with-output-to-string (code)
                          (compiler::p2-toplevel-forms (compiler::pass1-dump-macros) code))
                        output)))))))
    (when verbose (format t "; Wrote ~A~%" output-path))
    (values (truename output-path) warnings failure)))
