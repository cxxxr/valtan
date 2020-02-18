(defpackage :valtan-host.asdf-compat
  (:use :cl))
(in-package :valtan-host.asdf-compat)

(defpackage :valtan-host.asdf-compat-user
  (:use :cl))

(defvar *registry-systems* (make-hash-table :test 'equal))

(defstruct system
  name
  depends-on
  serial
  components)

(defstruct component
  pathname)

(defun register-system (system)
  (setf (gethash (system-name system) *registry-systems*)
        system))

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
                     (make-component :pathname file)))))

(defvar *defined-system*)

(defmacro valtan-host.asdf-compat-user::defsystem (name &key serial depends-on components)
  `(setq *defined-system*
         (register-system
          (make-system :name ,(string name)
                       :serial ,serial
                       :depends-on ',depends-on
                       :components ',(parse-components components
                                                       name
                                                       (pathname-directory *load-pathname*))))))

(defun read-system-file (pathname)
  (let ((*package* (find-package :valtan-host.asdf-compat-user))
        (*defined-system*))
    (load pathname)
    *defined-system*))

(defparameter *system-directories* (list (asdf:system-relative-pathname :valtan "./library/")))

(defun find-system (system-name)
  (setq system-name (string-downcase system-name))
  (or (gethash system-name *registry-systems*)
      (labels ((ok (pathname)
                 (when (and (equal (pathname-type pathname) "system")
                            (equal (pathname-name pathname) system-name))
                   pathname))
               (f (directory)
                 (or (some #'ok (uiop:directory-files directory))
                     (some #'f (uiop:subdirectories directory)))))
        (let ((system-pathname (some #'f *system-directories*)))
          (when system-pathname
            (read-system-file system-pathname))))))

(defun load-system (system-name)
  (let ((system (find-system system-name)))
    (unless system
      (error "system ~A is not defined" system-name))
    ))
