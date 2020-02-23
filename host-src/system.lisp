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

(defparameter +valtan-core-system+ "valtan-core")
(defparameter *system-directories* (list (asdf:system-relative-pathname :valtan "./library/")))

(defvar *defined-system*)

(defstruct system
  name
  pathname
  (pathnames '())
  (enable-profile nil)
  (depends-on '())
  (target nil :type (member :node :browser nil)))

(defun ensure-system-package-exist ()
  (find-package :valtan-host.system-user))

(defun featurep (expr)
  (cond ((keywordp expr)
         (member expr *features*))
        ((not (consp expr))
         (error "invalid feature: ~S" expr))
        ((ecase (first expr)
           (:and
            (every #'featurep (rest expr)))
           (:or
            (some #'featurep (rest expr)))
           (:not
            (not (featurep (second expr)))))
         t)))

(defun parse-components (components system-name directory)
  (loop :for c :in components
        :for file :=
           (destructuring-bind (&key ((:file name)) (if-feature nil if-feature-p)) c
             (unless name
               (error "Illegal component: ~S" c))
             (let ((file
                     (probe-file
                      (make-pathname :name name
                                     :type "lisp"
                                     :directory directory))))
               (unless file
                 (error "~S not found for system ~S" name system-name))
               (when (or (not if-feature-p)
                         (featurep if-feature))
                 file)))
        :when file
        :collect file))

(defmacro valtan-host.system-user::defsystem (name &key serial depends-on components target)
  (check-type target (member :node :browser nil))
  (assert (eq serial t))
  `(setq *defined-system*
         (make-system :name ,(string name)
                      :pathname *load-pathname*
                      :depends-on ',(if (string= (string-downcase name) +valtan-core-system+)
                                        depends-on
                                        (cons +valtan-core-system+ depends-on))
                      :pathnames ',(parse-components components
                                                     name
                                                     (pathname-directory
                                                      *load-pathname*))
                      :target ,target)))

(defun load-system (pathname)
  (let ((*package* (ensure-system-package-exist))
        (*defined-system*)
        (*features* (adjoin :valtan *features*)))
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
