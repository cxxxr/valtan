(defpackage :valtan-host.system
  (:use :cl)
  (:export :system-name
           :system-pathname
           :system-pathnames
           :system-enable-profile
           :system-depends-on
           :system-target
           :load-system
           :find-system
           :compute-system-precedence-list))
(in-package :valtan-host.system)

(defpackage :valtan-host.system-user (:use :cl))

(defparameter *system-directories* (list (asdf:system-relative-pathname :valtan "./library/")))

(defstruct system
  name
  pathname
  (pathnames '())
  (enable-profile nil)
  (depends-on '())
  (target nil :type (member :node :browser nil)))

(defun ensure-system-package-exist ()
  (find-package :valtan-host.system-user))

(defvar *defined-system*)

(defun parse-components (components system-name directory)
  (loop :for c :in components
        :collect (destructuring-bind (&key ((:file name))) c
                   (unless name
                     (error "Illegal component: ~S" c))
                   (let ((file
                           (probe-file
                            (make-pathname :name name
                                           :type "lisp"
                                           :directory directory))))
                     (unless file
                       (error "~S not found for system ~S" name system-name))
                     file))))

(defmacro valtan-host.system-user::defsystem (name &key serial depends-on components target)
  (check-type target (member :node :browser nil))
  (assert (eq serial t))
  `(setq *defined-system*
         (make-system :name ,(string name)
                      :pathname *load-pathname*
                      :depends-on ',(if (string= (string-downcase name) "valtan")
                                        depends-on
                                        (cons "valtan" depends-on))
                      :pathnames ',(parse-components components
                                                     name
                                                     (pathname-directory
                                                      *load-pathname*))
                      :target ,target)))

(defun load-system (pathname)
  (let ((*package* (ensure-system-package-exist))
        (*defined-system*))
    (load pathname)
    *defined-system*))

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
