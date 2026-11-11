(in-package :valtan-host.system)

(defparameter *host-asdf-systems* '("asdf" "asdf-package-system"))
(defvar *initial-target-asdf-features* nil)

(defun target-asdf-features ()
  ;; ASDF version features describe the definition reader, not the target VM.
  (union valtan-core::*features*
         (remove-if-not (lambda (feature)
                          (uiop:string-prefix-p "ASDF" (symbol-name feature)))
                        *features*)))

(defun call-with-target-asdf (function)
  ;; System objects already in the compiler image were read with host features.
  ;; A separate registry/session prevents reusing or changing those objects.
  ;; ASDF renamed this registry in older releases; support both spellings.
  (let* ((registry (find-if (lambda (symbol) (and symbol (boundp symbol)))
                            (list (find-symbol "*REGISTERED-SYSTEMS*" :asdf/system-registry)
                                  (find-symbol "*DEFINED-SYSTEMS*" :asdf/system-registry))))
         (builtins (remove-if-not
                    (lambda (system)
                      (member (asdf:component-name system)
                              *host-asdf-systems* :test #'equal))
                    (asdf/system-registry:registered-systems*)))
         (ports-directory (asdf:system-relative-pathname :valtan "library/portability/"))
         (*initial-target-asdf-features* (target-asdf-features))
         (*features* (copy-list *initial-target-asdf-features*))
         (asdf:*central-registry* (cons ports-directory (append *system-directories* asdf:*central-registry*))))
    (unless (and registry (boundp registry))
      (error "Unsupported ASDF registry in ASDF ~A" (asdf:asdf-version)))
    (progv (list registry) (list (make-hash-table :test 'equal))
      (mapc #'asdf/system-registry:register-system builtins)
      (asdf/session:with-asdf-session (:override t)
        ;; Ports are selected in the target registry only. Host ASDF and
        ;; compiler dependencies continue to use their native implementations.
        (dolist (name '("trivial-gray-streams" "trivial-features" "trivial-garbage" "closer-mop" "uiop"
                       "net.didierverna.asdf-flv"))
          (asdf:load-asd (merge-pathnames (format nil "~A.asd" name) ports-directory)))
        (when (member :valtan-resumable *features*)
          (asdf:load-asd (merge-pathnames "bordeaux-threads.asd" ports-directory)))
        (funcall function)))))

(defun import-asdf-system (system)
  ;; A source bundle retains ASDF's order across module and system boundaries.
  ;; Do not PERFORM the plan: host FASLs cannot be executed by the JS runtime.
  (when (member (asdf:component-name system) *host-asdf-systems* :test #'equal)
    (error "Host system ~A has no Valtan runtime implementation" (asdf:component-name system)))
  ;; An already-loaded host system can disappear from a filtered ASDF plan.
  ;; Reject it in the declared dependency graph as well as in emitted actions.
  (let ((visited (make-hash-table :test 'equal)))
    (labels ((dependency-name (dependency)
               (cond ((or (stringp dependency) (symbolp dependency)) dependency)
                     ((eq (first dependency) :version) (second dependency))
                     ((eq (first dependency) :feature)
                      (when (uiop:featurep (second dependency))
                        (dependency-name (third dependency))))))
             (visit (component)
               (let ((name (asdf:component-name component)))
                 (unless (gethash name visited)
                   (setf (gethash name visited) t)
                   (when (member name *host-asdf-systems* :test #'equal)
                     (error "Host system ~A has no Valtan runtime implementation" name))
                   (dolist (dependency (asdf:system-depends-on component))
                     (let ((dependency (dependency-name dependency)))
                       (when dependency (visit (asdf:find-system dependency)))))))))
      (visit system)))
  (let ((components '())
        (resources '())
        (metadata-systems '())
        (seen (make-hash-table :test 'equal))
        (plan (asdf:make-plan 'asdf/plan:filtered-sequential-plan
                             'asdf:load-op system :other-systems t)))
    (dolist (action (asdf/plan:plan-actions plan))
      (let ((operation (car action)) (component (cdr action)))
        (pushnew (asdf:component-system component) metadata-systems)
        (when (member (asdf:component-name (asdf:component-system component))
                      *host-asdf-systems* :test #'equal)
          (error "Host system ~A has no Valtan runtime implementation"
                 (asdf:component-name (asdf:component-system component))))
        (unless (member (type-of operation)
                        '(asdf:prepare-op asdf:compile-op asdf:load-op
                          asdf/find-system:define-op))
          (error "ASDF operation ~S for ~A is not supported by Valtan"
                 (type-of operation) (asdf:component-name component)))
        (when (typep operation 'asdf:load-op)
          (typecase component
            (asdf:cl-source-file
             (unless (eq (type-of component) 'asdf:cl-source-file)
               (error "Custom ASDF source component ~S for ~A requires a Valtan adapter"
                      (type-of component) (asdf:component-pathname component)))
             (let ((pathname (asdf:component-pathname component)))
               (unless (gethash pathname seen)
                 (unless (probe-file pathname)
                   (error "ASDF source file ~A is missing" pathname))
                 (setf (gethash pathname seen) t)
                 (push (make-lisp-component :name (asdf:component-name component)
                                            :pathname pathname)
                       components))))
            (asdf:static-file
             (pushnew (asdf:component-pathname component) resources :test #'equal))
            (asdf:module)
            (t (error "ASDF component ~S (~A) is not supported by Valtan"
                      (type-of component) (asdf:component-name component)))))))
    (let ((name (asdf:component-name system)))
      (setf (gethash name *system-map*)
            (make-system :name name
                         :pathname (asdf:system-source-file system)
                         ;; Definitions can announce implementation features,
                         ;; e.g. MCCLIM, which their target sources also read.
                         ;; Only additions to the isolated target feature set
                         ;; cross this boundary; host implementation features
                         ;; and ASDF's own reader features do not.
                         :target-features (set-difference *features*
                                                         *initial-target-asdf-features*)
                         :asdf-metadata (mapcar #'describe-asdf-component
                                               (reverse metadata-systems))
                         :asdf-resources (nreverse resources)
                         :depends-on (list +valtan-core-system+)
                         :components (nreverse components))))))

(defun describe-asdf-component (component)
  ;; Only immutable descriptive data crosses into the target; no host ASDF
  ;; objects, compiled functions, or build operations are serialized.
  (list (asdf:component-name component)
        (let ((path (asdf:component-pathname component)))
          (when path (namestring path)))
        (cond ((typep component 'asdf:system) :system)
              ((typep component 'asdf:module) :module)
              (t :file))
        (asdf:component-version component)
        (when (typep component 'asdf:system)
          (let ((path (asdf:system-source-file component)))
            (when path (namestring path))))
        (when (typep component 'asdf:module)
          (mapcar #'describe-asdf-component (asdf:component-children component)))))

(defun find-asdf-system (name)
  (call-with-target-asdf
   (lambda ()
     (let ((system (asdf:find-system name nil)))
       (when system
         (import-asdf-system system))))))

(defun load-asdf-system-file (pathname)
  (call-with-target-asdf
   (lambda ()
     (asdf:load-asd pathname)
     (import-asdf-system (asdf:find-system (pathname-name pathname))))))
