#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defun find-all-symbols (name)
  (let ((symbols '()))
    (dolist (package (list-all-packages))
      (*:map-package-symbols
       package
       (lambda (symbol)
         (when (string= name symbol)
           (push symbol symbols)))))
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
                           ((stringp package-designator)
                            (find-package package-designator))
                           (t
                            (type-error package-designator 'package)))))
        (unless package
          (error "The Package ~A is undefined" package-designator))
        package)))

(defun package-name (package)
  (*:raw-string-to-array (*:package-name (ensure-package package))))

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
                  (*:list-to-raw-array
                   (mapcar (lambda (nickname)
                             (*:array-to-raw-string (string nickname)))
                           use))))

(defmacro defpackage (package &body options)
  (when compiler::*in-host-runtime*
    ;; ホスト側のsbclで定義しないとin-packageもエラーになる
    (cl:eval `(defpackage ,package ,@options)))
  (let ((export (cdr (assoc :export options)))
        (use (cdr (assoc :use options)))
        (nicknames (cdr (assoc :nicknames options))))
    `(*:%defpackage ,package :export ,export :use ,use :nicknames ,nicknames)))

(defun export (symbols &optional (package *package*))
  (unless (or (listp symbols) (symbolp symbols))
    (type-error symbols '(or cons symbol)))
  (let ((symbols
          (cond ((null symbols))
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
    (when package
      (*:%delete-package package))))

(defun package-used-by-list (package-designator)
  (*:package-used-by-list (ensure-package package-designator)))

(defun package-use-list (package-designator)
  (*:package-use-list (ensure-package package-designator)))

#|
(defvar *packages* '())

(defstruct (package (:constructor %make-package)
                    (:copier nil)
                    (:predicate packagep))
  name
  nicknames
  use-list
  internal-symbols
  external-symbols)

(defun find-package (name)
  (cond ((packagep name)
         name)
        (t
         (dolist (package *packages*)
           (when (or (package-name package)
                     (find name (package-nicknames package) :test #'string=))
             (return package))))))

(defun package-designator-to-package (package-designator)
  (let ((package (find-package package-designator)))
    (unless package
      ;;(error "The name ~S does not designate any package." package-designator)
      (error "error"))
    package))

(defun make-package (package-name &key nicknames use)
  (when (or (find-package package-name)
            (find-if #'find-package nicknames))
    (error "error"))
  (let ((use-list (mapcar #'package-designator-to-package
                          (if (listp use) use (list use)))))
    (let ((package (%make-package :name package-name
                                  :nicknames nicknames
                                  :use-list use-list
                                  :internal-symbols '()
                                  :external-symbols '())))
      (push package *packages*)
      package)))

(defun find-symbol (string &optional (package-designator *package*))
  (unless (stringp string)
    (error "type error"))
  (let ((package (package-designator-to-package package-designator)))
    (dolist (symbol (package-external-symbols package))
      (when (string= string (symbol-name symbol))
        (return-from find-symbol (values symbol :external))))
    (dolist (symbol (package-internal-symbols package))
      (when (equal string (symbol-name symbol))
        (return-from find-symbol (values symbol :internal))))
    (dolist (p (package-use-list package))
      (dolist (symbol (package-external-symbols p))
        (when (equal string (symbol-name symbol))
          (return-from find-symbol (values symbol :inherited)))))))

(defun intern (string &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator)))
    (multiple-value-bind (symbol found)
        (find-symbol string package)
      (if found
          (values symbol found)
          (let ((symbol (make-symbol string)))
            (push symbol (package-internal-symbols package))
            (values symbol nil))))))
|#
