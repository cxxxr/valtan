(defpackage :valtan-host.system
  (:use :cl)
  (:export :*system-directories*
           :lisp-component
           :js-component
           :component-pathname
           :system-name
           :system-pathname
           :system-components
           :system-enable-profile
           :system-enable-source-map
           :system-depends-on
           :system-target
           :system-target-features
           :system-compile-mode
           :system-entry-file
           :system-file-p
           :load-system
           :find-system
           :load-system-file
           :compute-system-precedence-list
           :escape-system-pathname))
(in-package :valtan-host.system)

(defpackage :valtan-host.system-user (:use :cl))

(defparameter +valtan-core-system+ "valtan-core")
(defparameter *system-directories* (list (asdf:system-relative-pathname :valtan "./library/")))

(defvar *system-map* (make-hash-table :test 'equal))
(defvar *prefer-asdf-systems* nil)

(defstruct system
  name
  pathname
  (components '())
  (asdf-metadata nil)
  (asdf-resources nil)
  (enable-profile nil)
  (enable-source-map nil)
  (depends-on '())
  (target nil :type (member :node :browser nil))
  (target-features '())
  (compile-mode :ahead-of-time :type (member :ahead-of-time :runtime))
  (entry-file nil))

(defstruct component
  name
  pathname
  depends-on)

(defstruct (lisp-component (:include component)))
(defstruct (js-component (:include component)))

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

(defun parse-components (&key components system-name directory serial)
  (declare (ignore serial))
  (let ((components
          (loop :for component :in components
                :when (cond ((getf component :file)
                             (destructuring-bind (&key ((:file name))
                                                       (if-feature nil if-feature-p)
                                                       depends-on)
                                 component
                               (unless name
                                 (error "Illegal component: ~S" component))
                               (when (or (not if-feature-p) (featurep if-feature))
                                 (let ((file
                                         (probe-file
                                          (make-pathname :name name
                                                         :type "lisp"
                                                         :directory directory))))
                                   (unless file
                                     (error "~S not found for system ~S" name system-name))
                                   (make-lisp-component :name name
                                                        :pathname file
                                                        :depends-on depends-on)))))
                            ((getf component :js-source-file)
                             (destructuring-bind (&key ((:js-source-file name))
                                                       depends-on)
                                 component
                               (let ((file
                                       (probe-file (make-pathname :name name
                                                                  :type "js"
                                                                  :directory directory))))
                                 (unless file
                                   (error "~S not found for system ~S" name system-name))
                                 (make-js-component :name name
                                                    :pathname file
                                                    :depends-on depends-on)))))
                :collect it)))
    (flet ((find-component (name)
             (find name components :key #'component-name :test #'equal)))
      (dolist (component components)
        (when (component-depends-on component)
          (setf (component-depends-on component)
                (mapcar #'find-component (component-depends-on component)))))
      components)))

(defun compute-components (components)
  (let ((computed-components '()))
    (labels ((resolve-dependencies (component)
               (mapc #'resolve-dependencies (component-depends-on component))
               (pushnew component computed-components)))
      (mapc #'resolve-dependencies components)
      (nreverse computed-components))))

(defmacro valtan-host.system-user::defsystem
    (name &key (serial nil serial-p) depends-on components target source-map entry-file
          (compile-mode :ahead-of-time) target-features &allow-other-keys)
  (check-type target (member :node :browser nil))
  (check-type compile-mode (member :ahead-of-time :runtime))
  (assert (and (listp target-features) (every #'keywordp target-features)))
  (assert (or (not serial-p) (eq serial t)))
  (let* ((*features* (union target-features *features*))
         (components (parse-components :components components
                                      :system-name name
                                      :directory (pathname-directory *load-pathname*)
                                      :serial serial)))
    `(setf (gethash ,(string-downcase name) *system-map*)
           (make-system :name ,(string-downcase name)
                        :pathname *load-pathname*
                        :depends-on ',(if (or (string= (string-downcase name) +valtan-core-system+)
                                             (uiop:string-prefix-p "valtan-core/" (string-downcase name)))
                                          depends-on
                                          (cons +valtan-core-system+ depends-on))
                        :components ',(compute-components components)
                        :target ,target
                        :target-features ',target-features
                        :compile-mode ,compile-mode
                        :enable-source-map ,source-map
                        :entry-file ,entry-file))))

(defmacro with-system-env (() &body body)
  `(let ((*package* (ensure-system-package-exist))
         (*features* (adjoin :valtan *features*)))
     ,@body))

(defun load-system (pathname)
  (if (equal (pathname-type pathname) "asd")
      (load-asdf-system-file pathname)
      (with-system-env ()
        (load pathname))))

(defun system-file-p (pathname)
  (or (equal (pathname-type pathname) "system")
      (equal (pathname-type pathname) "asd")))

(defun defsystem-form-p (form)
  (and (consp form)
       (symbolp (first form))
       (string-equal (first form) "defsystem")
       (<= 2 (length form))))

(defun lookup (system-file system-name)
  (with-open-file (in system-file)
    (with-system-env ()
      (loop :with eof := '#:eof
            :for form := (read in nil eof)
            :until (eq form eof)
            :do (when (and (defsystem-form-p form)
                           (string-equal (second form) system-name))
                  (return t))))))

(defun find-system-file (system-name &optional (directories *system-directories*))
  (labels ((ok (pathname)
             (when (and (equal (pathname-type pathname) "system")
                        (lookup pathname system-name))
               pathname))
           (f (directory)
             (or (some #'ok (uiop:directory-files directory))
                 (some #'f (uiop:subdirectories directory)))))
    (some #'f directories)))

(defun core-system-name-p (name)
  (or (equal name +valtan-core-system+)
      (uiop:string-prefix-p "valtan-core/" name)))

(defun find-system (system-name &optional (errorp t))
  (let ((name (string-downcase system-name)))
    (or (gethash name *system-map*)
        ;; Keep the target runtime and explicit project .system definitions.
        ;; Runtime mode prefers upstream ASDF sources. Preserve bundled ports
        ;; for existing ahead-of-time projects.
        (if (core-system-name-p name)
            (progn
              ;; The runtime .asd also builds the host compiler. Interpret it
              ;; with the native target loader, not ASDF's host build hooks.
              (with-system-env ()
                (load (asdf:system-relative-pathname :valtan "library/valtan-core/valtan-core.asd")))
              (gethash name *system-map*))
            (let* ((bundled (asdf:system-relative-pathname :valtan "library/"))
                   (local (remove-if (lambda (directory) (uiop:subpathp directory bundled))
                                     *system-directories*))
                   (pathname (find-system-file name local)))
              (flet ((bundled-system ()
                       (let ((fallback (find-system-file name (list bundled))))
                         (when fallback
                           (load-system fallback)
                           (gethash name *system-map*)))))
                (or (and pathname
                         (progn (load-system pathname) (gethash name *system-map*)))
                    (and (not *prefer-asdf-systems*) (bundled-system))
                    (find-asdf-system name)
                    (and *prefer-asdf-systems* (bundled-system))))))
        (when errorp
          (error "System ~S is not found. Install its sources with Quicklisp or configure ASDF's source registry."
                 name)))))

(defun load-system-file (pathname)
  (load-system pathname)
  (or (gethash (string-downcase (pathname-name pathname)) *system-map*)
      (error "No system named ~S is defined by ~A" (pathname-name pathname) pathname)))

(defun compute-system-precedence-list (system)
  (let ((seen (make-hash-table :test 'equal))
        (systems '()))
    (labels ((f (system)
               (when (eq :visiting (gethash (system-name system) seen))
                 (error "Circular system dependency involving ~S" (system-name system)))
               (unless (eq :done (gethash (system-name system) seen))
                 (setf (gethash (system-name system) seen) :visiting)
                 (dolist (system-name (system-depends-on system))
                   (f (find-system system-name)))
                 (setf (gethash (system-name system) seen) :done)
                 (push system systems))))
      (f system)
      (nreverse systems))))

(defun escape-system-pathname (system)
  (make-pathname :directory (pathname-directory (system-pathname system))
                 :name (ppcre:regex-replace-all "/" (system-name system) "-slash-")
                 :type (pathname-type (system-pathname system))))
