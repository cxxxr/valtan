#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defun find-all-symbols (name)
  (let ((symbols '()) (name (string name)))
    (dolist (package (list-all-packages))
      (multiple-value-bind (symbol status) (find-symbol name package)
        (when status (pushnew symbol symbols :test #'eq))))
    symbols))

(defun find-package (name)
  (cond ((packagep name)
         name)
        ;; Handle raw strings from host environment (SBCL) - check if it's a CL string, not Valtan array
        ((and (cl:stringp name) (not (arrayp name)))
         (cl:find-package name))
        ;; Handle symbols from host environment
        ((and (cl:symbolp name) (not (arrayp name)))
         (cl:find-package (cl:symbol-name name)))
        (t
         (dolist (package (list-all-packages))
           (when (or (string= name (package-name package))
                     (find name (package-nicknames package) :test #'string=))
             (return package))))))

(defun ensure-package (package-designator)
  (if (packagep package-designator)
      package-designator
      (let ((package (cond ((symbolp package-designator)
                            (find-package (symbol-name package-designator)))
                           ((or (stringp package-designator) (characterp package-designator))
                            (find-package (string package-designator)))
                           (t
                            (type-error package-designator 'package)))))
        (unless package
          (error 'package-error :package package-designator))
        package)))

(defun package-name (package)
  (let ((name (*:package-name (ensure-package package))))
    (unless (eq name system:+null+) (*:raw-string-to-array name))))

(defun package-nicknames (package)
  (mapcar #'*:raw-string-to-array
          (*:raw-array-to-list (*:package-nicknames (ensure-package package)))))

(defun intern (name &optional (package *package*))
  ;; Handle raw strings from host environment (SBCL) - check if it's a CL string, not Valtan array
  (if (and (cl:stringp name) (not (arrayp name)))
      (cl:intern name (let ((pkg (find-package package)))
                        (if (cl:packagep pkg) pkg (cl:find-package (cl:string pkg)))))
      (*:intern (*:array-to-raw-string name)
                (find-package package))))

(defun find-symbol (name &optional (package *package*))
  ;; Handle raw strings from host environment (SBCL) - check if it's a CL string, not Valtan array
  (if (and (cl:stringp name) (not (arrayp name)))
      (let* ((pkg (find-package package))
             (real-pkg (if (cl:packagep pkg) pkg (cl:find-package (cl:string pkg)))))
        (cl:find-symbol name real-pkg))
      (*:find-symbol (*:array-to-raw-string name)
                     (find-package package))))

(defun make-package (package-name &key nicknames use)
  (*:make-package (*:array-to-raw-string (string package-name))
                  (*:list-to-raw-array
                   (mapcar (lambda (nickname)
                             (*:array-to-raw-string (string nickname)))
                           nicknames))
                  (*:list-to-raw-array (mapcar #'ensure-package use))))


(defun export (symbols &optional (package *package*))
  (unless (or (listp symbols) (symbolp symbols))
    (type-error symbols '(or cons symbol)))
  (let ((symbols
          (cond ((null symbols) nil)
                ((symbolp symbols)
                 (list symbols))
                (t symbols)))
        (package (ensure-package package)))
    (dolist (symbol symbols)
      (unless (symbolp symbol)
        (type-error symbol 'symbol)))
    (system:export symbols package)
    t))

(defun delete-package (package-designator)
  (let ((package (find-package package-designator)))
    (cond ((null package)
           (cerror "Ignore the missing package." 'package-error :package package-designator)
           nil)
          ((null (package-name package)) nil)
          (t
           (when (package-used-by-list package)
             (cerror "Remove this package from the use lists." 'package-error :package package))
           (*:%delete-package package)))))

(defun package-used-by-list (package-designator)
  (*:package-used-by-list (ensure-package package-designator)))

(defun package-use-list (package-designator)
  (*:package-use-list (ensure-package package-designator)))

(defun package-shadowing-symbols (package-designator)
  (system:package-shadowing-symbols (ensure-package package-designator)))

(defun collect-package-symbols (package external-only)
  (let ((symbols nil))
    (funcall (if external-only #'system:map-package-external-symbols
                 #'system:map-package-all-symbols)
             (ensure-package package) (lambda (symbol) (push symbol symbols)))
    symbols))

(defmacro do-symbols ((var &optional (package '*package*) result) &body body)
  `(dolist (,var (collect-package-symbols ,package nil) ,result) ,@body))

(defmacro do-external-symbols ((var &optional (package '*package*) result) &body body)
  `(dolist (,var (collect-package-symbols ,package t) ,result) ,@body))

(defun collect-all-symbols ()
  (let ((symbols nil))
    (dolist (package (list-all-packages))
      (system:map-package-symbols package (lambda (symbol) (push symbol symbols))))
    symbols))

(defmacro do-all-symbols ((var &optional result) &body body)
  `(dolist (,var (collect-all-symbols) ,result) ,@body))

(defun make-package-iterator (packages symbol-types)
  (let ((entries nil))
    (dolist (designator (if (listp packages) packages (list packages)))
      (let ((package (ensure-package designator)))
        (system:map-package-all-symbols
         package
         (lambda (symbol)
           (multiple-value-bind (accessible status) (find-symbol (symbol-name symbol) package)
             (declare (ignore accessible))
             (when (member status symbol-types)
               (push (list symbol status package) entries)))))))
    (lambda ()
      (if entries
          (let ((entry (pop entries))) (values t (first entry) (second entry) (third entry)))
          (values nil)))))

(defmacro with-package-iterator ((name package-list &rest symbol-types) &body body)
  (cl:unless (cl:and symbol-types
                    (cl:every (lambda (type) (cl:member type '(:internal :external :inherited))) symbol-types))
    (cl:error 'cl:program-error))
  (let ((iterator (cl:gensym)))
    `(let ((,iterator (make-package-iterator ,package-list ',symbol-types)))
       (macrolet ((,name () '(funcall ,iterator)))
         ,@body))))

(defun package-operation-error (package raw-message)
  (error 'package-error
         :package (if (packagep package) package (*:raw-string-to-array package))
         :format-control (*:raw-string-to-array raw-message)))

(defun use-package (packages-to-use &optional (package *package*))
  (system:use-package
   (mapcar #'ensure-package (if (listp packages-to-use) packages-to-use (list packages-to-use)))
   (ensure-package package)))

(defun unuse-package (packages-to-unuse &optional (package *package*))
  (system:unuse-package
   (mapcar #'ensure-package (if (listp packages-to-unuse) packages-to-unuse (list packages-to-unuse)))
   (ensure-package package)))

(defun import (symbols &optional (package *package*))
  (system:import symbols (ensure-package package)))

(defun shadowing-import (symbols &optional (package *package*))
  (system:shadowing-import symbols (ensure-package package)))

(defun shadow (names &optional (package *package*))
  (system:shadow
   (*:list-to-raw-array
    (mapcar (lambda (name) (*:array-to-raw-string (string name)))
            (if (listp names) names (list names))))
   (ensure-package package)))

(defun unintern (symbol &optional (package *package*))
  (system:unintern symbol (ensure-package package)))

(defun unexport (symbols &optional (package *package*))
  (system:unexport symbols (ensure-package package)))

(defun rename-package (package name &optional nicknames)
  (system:rename-package
   (ensure-package package) (*:array-to-raw-string (string name))
   (*:list-to-raw-array
    (mapcar (lambda (nickname) (*:array-to-raw-string (string nickname))) nicknames))))

(defvar *package-documentation* (make-hash-table :test 'eq))

(defun ensure-defined-package (name options)
  (let ((names nil) (interned nil) (exported nil) (nicknames nil)
        (documentation nil) (documentation-p nil) (size-p nil))
    ;; Validate before mutating the package system.
    (dolist (option options)
      (unless (consp option) (program-error "Invalid DEFPACKAGE option ~S" option))
      (case (car option)
        ((:shadow :intern :import-from :shadowing-import-from)
         (dolist (name (if (member (car option) '(:import-from :shadowing-import-from))
                          (cddr option) (cdr option)))
           (let ((name (string name)))
             (when (member name names :test #'string=)
               (program-error "Overlapping DEFPACKAGE symbol name ~S" name))
             (push name names)
             (when (eq (car option) :intern) (push name interned)))))
        (:export (setq exported (append exported (mapcar #'string (cdr option)))))
        (:nicknames (setq nicknames (append nicknames (mapcar #'string (cdr option)))))
        (:use)
        (:size
         (when (or size-p (/= (length option) 2)
                   (not (typep (second option) '(integer 0 *))))
           (program-error "Invalid DEFPACKAGE :SIZE option"))
         (setq size-p t))
        (:documentation
         (when (or documentation-p (/= (length option) 2) (not (stringp (second option))))
           (program-error "Invalid DEFPACKAGE :DOCUMENTATION option"))
         (setq documentation-p t documentation (second option)))
        (otherwise (program-error "Unknown DEFPACKAGE option ~S" (car option)))))
    (when (intersection interned exported :test #'string=)
      (program-error "DEFPACKAGE :INTERN and :EXPORT names overlap"))
    (let ((package (or (find-package name) (make-package name :use nil))))
      (rename-package package name nicknames)
      (labels ((import-option (option shadowing)
                 (let ((source (ensure-package (second option))))
                   (dolist (name (cddr option))
                     (multiple-value-bind (symbol status) (find-symbol (string name) source)
                       (if status
                           (if shadowing (shadowing-import symbol package) (import symbol package))
                           (cerror "Skip the missing symbol." 'package-error :package source)))))))
        (dolist (option options)
          (case (car option)
            (:shadow (shadow (cdr option) package))
            (:shadowing-import-from (import-option option t))))
        (dolist (option options)
          (when (eq (car option) :use) (use-package (cdr option) package)))
        (dolist (option options)
          (case (car option)
            (:import-from (import-option option nil))
            (:intern (dolist (name (cdr option)) (intern (string name) package)))))
        (dolist (name exported) (export (list (intern name package)) package))
        (when documentation-p (setf (gethash package *package-documentation*) documentation)))
      package)))

(defmacro defpackage (package &body options)
  (when compiler::*in-host-runtime*
    (cl:unless (cl:find-package package)
      (cl:eval `(cl:defpackage ,package ,@options))))
  (if compiler::*in-host-runtime*
      ;; Compiled modules initialize their symbol references before evaluating
      ;; their top-level forms. Reserve the package first, then apply options
      ;; in the normal runtime order.
      `(progn (*:%defpackage ,package)
              (ensure-defined-package ',package ',options))
      `(ensure-defined-package ',package ',options)))
