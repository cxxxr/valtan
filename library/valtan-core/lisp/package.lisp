(in-package :common-lisp)

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
  (*:js-string-to-array (*:package-name (ensure-package package))))

(defun package-nicknames (package)
  (mapcar #'*:js-string-to-array
          (*:js-array-to-list (*:package-nicknames (ensure-package package)))))

(defun intern (name &optional (package *package*))
  (*:intern (*:array-to-js-string name)
            (find-package package)))

(defun find-symbol (name &optional (package *package*))
  (*:find-symbol (*:array-to-js-string name)
                 (find-package package)))

(defun make-package (package-name &key nicknames use)
  (*:make-package (*:array-to-js-string (string package-name))
                  (*:list-to-js-array
                   (mapcar (lambda (nickname)
                             (*:array-to-js-string (string nickname)))
                           nicknames))
                  (*:list-to-js-array
                   (mapcar (lambda (nickname)
                             (*:array-to-js-string (string nickname)))
                           use))))

(defmacro defpackage (package &body options)
  (when compiler::*in-host-runtime*
    ;; ホスト側のsbclで定義しないとin-packageもエラーになる
    (eval `(defpackage ,package ,@options)))
  (let ((export (cdr (assoc :export options)))
        (use (cdr (assoc :use options)))
        (nicknames (cdr (assoc :nicknames options))))
    `(*:%defpackage ,package :export ,export :use ,use :nicknames ,nicknames)))

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
