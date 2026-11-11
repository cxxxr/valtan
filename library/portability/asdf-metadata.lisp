;; Read-only ASDF component metadata for systems embedded by Valtan. Build
;; operations remain on the host; this package does not implement runtime ASDF.
(defpackage :asdf
  (:use :cl)
  (:export :component :system :find-system :find-component
           :component-name :component-pathname :component-parent
           :component-children :component-version :component-system
           :system-source-file :system-source-directory :system-relative-pathname
           :registered-systems :missing-component))
(in-package :asdf)

(defstruct component name pathname parent children version source-file)
(defstruct (system (:include component)))
(defvar *embedded-systems* (make-hash-table :test 'equal))

(defun coerce-name (name)
  (etypecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))))

(define-condition missing-component (error)
  ((name :initarg :name :reader missing-component-name))
  (:report (lambda (condition stream)
             (format stream "No embedded ASDF component named ~S"
                     (missing-component-name condition)))))

(defun find-system (name &optional (error-p t))
  (if (typep name 'system) name
      (or (gethash (coerce-name name) *embedded-systems*)
          (when error-p (error 'missing-component :name name)))))

(defun find-component (base path &key registered)
  (declare (ignore registered))
  (cond ((typep path 'component) path)
        ((consp base) (find-component (find-component (first base) (rest base)) path))
        ((null base)
         (cond ((null path) nil)
               ((consp path)
             (let ((system (find-system (first path) nil)))
               (when system (find-component system (rest path)))))
               (t (find-system path nil))))
        ((not (typep base 'component))
         (let ((system (find-system base nil)))
           (when system (find-component system path))))
        ((null path) base)
        ((consp path)
         (let ((child (find-component base (first path))))
           (when child (find-component child (rest path)))))
        (t (find (coerce-name path) (component-children base)
                 :key #'component-name :test #'string=))))

(defun component-system (component)
  (if (typep component 'system) component
      (component-system (component-parent component))))

(defun system-source-file (system)
  (component-source-file (find-system system)))

(defun system-source-directory (system)
  (make-pathname :name nil :type nil :version nil
                 :defaults (system-source-file system)))

(defun system-relative-pathname (system name &key type)
  (let ((path (merge-pathnames name (system-source-directory system))))
    (if type (make-pathname :type type :defaults path) path)))

(defun registered-systems ()
  (loop for name being the hash-keys of *embedded-systems* collect name))

(defun system::register-asdf-metadata (descriptions)
  (labels ((component (description parent)
             (destructuring-bind (name pathname kind version source children) description
               (let ((value (funcall (if (eq kind :system) #'make-system #'make-component)
                                     :name name :pathname (and pathname (pathname pathname))
                                     :parent parent :version version
                                     :source-file (and source (pathname source)))))
                 (setf (component-children value)
                       (mapcar (lambda (child) (component child value)) children))
                 value))))
    (dolist (description descriptions)
      ;; Shared dependencies preserve component identity across system bundles.
      (unless (find-system (first description) nil)
        (setf (gethash (first description) *embedded-systems*)
              (component description nil)))))
  t)
