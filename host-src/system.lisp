(defpackage :valtan-host.system
  (:use :cl)
  (:export :system-name
           :system-pathname
           :system-pathnames
           :system-enable-profile
           :system-depends-on
           :load-system
           :find-system
           :compute-system-precedence-list))
(in-package :valtan-host.system)

(defparameter *system-directories* (list (asdf:system-relative-pathname :valtan "./library/")))

(defstruct system
  name
  pathname
  (pathnames '())
  (enable-profile nil)
  (depends-on '()))

(defun ensure-system-package-exist ()
  (or (find-package :valtan-system)
      (make-package :valtan-system :use ())))

(defun load-system (pathname)
  (with-open-file (in pathname)
    (let ((plist (let ((*package* (ensure-system-package-exist)))
                   (read in)))
          (directory (pathname-directory pathname))
          (system-name (pathname-name pathname)))
      (destructuring-bind (&key members enable-profile depends-on) plist
        (make-system :name system-name
                     :pathname pathname
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
                     :depends-on (if (string= system-name "valtan")
                                     depends-on
                                     (cons "valtan" depends-on)))))))

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
