(defpackage :valtan-host.build
  (:use :cl
        :cl-source-map/source-map-generator
        :cl-source-map/mapping
        :valtan-host.system
        :valtan-host.emitter-stream)
  (:import-from :cl-source-map/mapping
                :mapping-generated-line
                :mapping-generated-column
                :mapping-original-line
                :mapping-original-column
                :mapping-source
                :mapping-name)
  (:export :build-system
           :build-application
           :run-node
           :run-build-server))
(in-package :valtan-host.build)

(defvar *cache-directory*)
(defvar *cache* (make-hash-table :test 'equal))
(defvar *discard-cache* nil)

(defvar *enable-source-map* nil)

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

(defmacro do-file-form ((var file) &body body)
  `(valtan-host.reader:map-file-forms (lambda (,var) ,@body) ,file))

(defun emit-import-lisp (stream)
  (write-line "var lisp = require('lisp');" stream))

(defun in-pass2 (hir-forms stream)
  (emit-import-lisp stream)
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

(defun input-file-to-output-file (input-file &optional (cache-directory *cache-directory*))
  (make-pathname :directory (append cache-directory (rest (pathname-directory input-file)))
                 :name (pathname-name input-file)
                 :type (if (equal (pathname-type input-file) "js")
                           "js"
                           (format nil "~A.js" (pathname-type input-file)))))

(defun to-source-map-pathname (pathname)
  (make-pathname :name (format nil "~A.~A" (pathname-name pathname) (pathname-type pathname))
                 :type "map"
                 :defaults pathname))

(defun write-to-source-map-file (file generator)
  (with-write-file (stream file)
    (to-json generator stream)))

(defun make-emitter-stream (stream input-file output-file)
  (let* ((generator
           (make-instance 'source-map-generator
                          :file (namestring output-file)))
         (stream
           (make-instance 'emitter-stream
                          :source input-file
                          :stream stream
                          :source-map-generator generator)))
    (values stream generator)))

(defun call-with-source-map (input-file output-file function)
  (with-write-file (out output-file)
    (multiple-value-bind (stream generator)
        (make-emitter-stream out input-file output-file)
      (if *enable-source-map*
          (let ((source-map-file (to-source-map-pathname output-file)))
            (format stream "//# sourceMappingURL=~A~%" source-map-file)
            (funcall function stream)
            (write-to-source-map-file source-map-file
                                      generator))
          (funcall function stream)))))

(defmacro with-source-map ((stream input-file output-file) &body body)
  `(call-with-source-map ,input-file ,output-file (lambda (,stream) ,@body)))

(defun !compile-file (input-file &optional (output-file (input-file-to-output-file input-file)))
  (%with-compilation-unit ()
    (let ((hir-forms
            (let ((hir-forms '())
                  (compiler::*export-modules* '())
                  (compiler::*macro-definitions* '())
                  (compiler::*source-info* (compiler::make-source-info))
                  (*package* (find-package :valtan-user)))
              (do-file-form (form input-file)
                (push (handler-bind ((warning #'muffle-warning))
                        (compiler::pass1-toplevel-using-optimize form)
                        #+(or)
                        (compiler::pass1-toplevel `(system::form-time ,form)))
                      hir-forms))
              (list* (compiler::pass1-toplevel '(in-package "CL-USER"))
                     (compiler::pass1-module input-file
                                             (nreverse hir-forms)
                                             compiler::*export-modules*)
                     (compiler::pass1-dump-macros compiler::*macro-definitions*)))))
      (ensure-directories-exist output-file)
      (with-source-map (stream input-file output-file)
        (in-pass2 hir-forms stream))
      output-file)))

(compiler:def-implementation compiler:set-source-map (hir)
  (when *enable-source-map*
    (let ((stream compiler::*p2-emit-stream*))
      (check-type stream emitter-stream)
      (let ((generated-line (emitter-stream-line stream))
            (generated-column (emitter-stream-column stream))
            (source (emitter-stream-source stream)))
        (when (compiler::hir-position hir)
          (destructuring-bind (original-line . original-column)
              (compiler::hir-position hir)
            (add-mapping
             (emitter-stream-source-map-generator stream)
             (mapping
              :generated-line (1- generated-line)
              :generated-column generated-column
              :original-line original-line
              :original-column original-column
              :source (namestring source)))))))))

(compiler:def-implementation compiler:make-emitter-stream (base-stream)
  (if *enable-source-map*
      (make-emitter-stream (make-string-output-stream)
                           (emitter-stream-source base-stream)
                           (cl-source-map/source-map-generator::.file
                            (emitter-stream-source-map-generator base-stream)))
      (make-string-output-stream)))

(defun merge-source-map (generator-1 generator-2 offset-line)
  (dolist (mapping
           (cl-source-map/mapping-list:to-list
            (cl-source-map/source-map-generator::.mappings generator-2)))
    (add-mapping generator-1
                 (mapping :generated-line (+ (mapping-generated-line mapping) offset-line)
                          :generated-column (mapping-generated-column mapping)
                          :original-line (mapping-original-line mapping)
                          :original-column (mapping-original-column mapping)
                          :source (mapping-source mapping)
                          :name (mapping-name mapping)))))

(compiler:def-implementation compiler:join-emitter-stream (base-stream forked-stream)
  (cond
    (*enable-source-map*
     (assert (typep base-stream 'emitter-stream))
     (assert (typep forked-stream 'emitter-stream))
     (let ((offset (1- (emitter-stream-line base-stream)))
           (base-generator (emitter-stream-source-map-generator base-stream))
           (forked-generator (emitter-stream-source-map-generator forked-stream)))
       (merge-source-map base-generator forked-generator offset)
       (write-string (get-output-stream-string (emitter-stream-stream forked-stream)) base-stream)))
    (t
     (write-string (get-output-stream-string forked-stream) base-stream))))

(defun compile-file-with-cache (input-file)
  (invoke-compile-file-with-cache
   input-file
   (lambda ()
     (!compile-file input-file))))

(defmethod compile-component ((component lisp-component))
  (compile-file-with-cache (component-pathname component)))

(defmethod compile-component ((component js-component))
  (let ((dst-file (input-file-to-output-file (component-pathname component))))
    (ensure-directories-exist dst-file)
    (uiop:copy-file (component-pathname component)
                    dst-file)))

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

(defun compute-output-system-pathname (system)
  (input-file-to-output-file (escape-system-pathname system)))

(defun compile-system-file (system)
  (when (system-entry-file system)
    (create-entry-file system))
  (%with-compilation-unit ()
    (let ((output-file (compute-output-system-pathname system)))
      (ensure-directories-exist output-file)
      (with-write-file (out output-file)
        (emit-import-lisp out)
        (dolist (system-name (system-depends-on system))
          (let* ((dependent-system (find-system system-name)) ;!!!
                 (path (resolve-path (escape-system-pathname system)
                                     (escape-system-pathname dependent-system))))
            (format out "require('~A.js');~%" path)))
        (dolist (component (system-components system))
          (let ((path (resolve-path (escape-system-pathname system)
                                    (component-pathname component))))
            (etypecase component
              (js-component
               (format out "require('~A');~%" path))
              (lisp-component
               (format out "require('~A.js');~%" path))))))
      output-file)))

(defun cache-system-key (system)
  (system-name system))

(defun compile-system-file-with-cache (system)
  (let ((input-file (system-pathname system)))
    (invoke-compile-file-with-cache
     input-file
     (lambda ()
       (compile-system-file system))
     :cache-key (cache-system-key system))))

(defun check-discarting-cache-system-file (system)
  (let* ((input-file (system-pathname system))
         (cache (gethash (cache-system-key system) *cache*)))
    (and cache (not (latest-cache-p cache input-file)))))

(defun create-entry-file (system)
  (let ((output-file
          (if (system-entry-file system)
              (merge-pathnames (system-entry-file system)
                               (uiop:pathname-directory-pathname (system-pathname system)))
              (make-pathname :type "js"
                             :name (system-name system)
                             :directory *cache-directory*))))
    (ensure-directories-exist output-file)
    (with-source-map (stream
                      (system-pathname system)
                      output-file)
      (emit-import-lisp stream)
      (format stream
              "require('~A');~%"
              (compute-output-system-pathname system))
      (compiler::p2-toplevel-forms
       (list (compiler::pass1-toplevel '(cl:finish-output) stream))
       stream))))

(defun prepare-valtan-path (base-directory)
  (with-open-file (out (make-pathname :name ".valtan-path"
                                      :directory base-directory)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ (asdf:system-relative-pathname :valtan "./kernel/") out)))

(defun build-system-using-system (system &key force)
  (let* ((base-directory (pathname-directory
                          (system-pathname system)))
         (*enable-source-map* (system-enable-source-map system))
         (*cache-directory* (append base-directory
                                    (list ".valtan-cache")))
         (*discard-cache* (if force t nil))
         (valtan-core::*features* valtan-core::*features*))
    (pushnew :valtan valtan-core::*features*)
    (when (eql :node (system-target system))
      (pushnew :node valtan-core::*features*))
    (prepare-valtan-path base-directory)
    (dolist (system (compute-system-precedence-list system))
      (when (check-discarting-cache-system-file system)
        (setq *discard-cache* t))
      (dolist (component (system-components system))
        (compile-component component))
      (compile-system-file-with-cache system))
    (create-entry-file system)))

(defun ensure-system-file (pathname)
  (let ((probed-pathname (probe-file pathname)))
    (unless probed-pathname
      (error "~A does not exist" pathname))
    (unless (system-file-p pathname)
      (error "~A is not a system file" pathname))
    probed-pathname))

(defun build-system (pathname &key force)
  (let* ((pathname (ensure-system-file pathname))
         (system (load-system-file pathname))
         (*system-directories*
           (cons (uiop:pathname-directory-pathname pathname)
                 *system-directories*)))
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
  (let ((systems (compute-system-precedence-list system))
        (directories '()))
    (dolist (system systems)
      (dolist (component (system-components system))
        (pushnew (make-pathname :directory (pathname-directory (component-pathname component)))
                 directories
                 :test #'uiop:pathname-equal)))
    directories))

#+linux
(defun run-build-server (pathname)
  (let* ((pathname (ensure-system-file pathname))
         (system-directory (make-pathname :directory (pathname-directory pathname)))
         (system (load-system-file pathname)))
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

#+darwin
(defun run-build-server (system-file)
  (let* ((pathname (ensure-system-file system-file))
         (system (load-system-file pathname))
         (directories (all-directories-to-notify system))
         (command (list* "fswatch"
                         "-1"
                         (mapcar (lambda (dir)
                                   (namestring dir))
                                 directories))))
    (build-when-file-modified pathname)
    (loop
      (let ((process (async-process:create-process command)))
        (unwind-protect
             (progn
               (async-process:process-receive-output process)
               (build-when-file-modified pathname))
          (async-process:delete-process process))))))

#+darwin
(defun build-when-file-modified (system-pathname)
  (handler-bind ((error (lambda (condition)
                          (format t "~&~%~A~%" condition)
                          (uiop:print-backtrace :condition condition)
                          (return-from build-when-file-modified))))
    (build-system system-pathname))
  (webpack (uiop:pathname-directory-pathname system-pathname)))
