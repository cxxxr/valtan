(in-package :common-lisp)

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
                            (error "type error")))))
        (unless package
          (error "The Package ~A is undefined" package-designator))
        package)))

(defun package-name (package)
  (system::js-string-to-array (system::%package-name (ensure-package package))))

(defun package-nicknames (package)
  (mapcar #'system::js-string-to-array
          (system::js-array-to-list (system::%package-nicknames (ensure-package package)))))

(defun intern (name &optional (package *package*))
  (system::intern (system::array-to-js-string name)
                  (find-package package)))

(defun make-package (package-name &key nicknames use)
  (system::make-package (system::array-to-js-string (string package-name))
                        (system::list-to-js-array
                         (mapcar (lambda (nickname)
                                   (system::array-to-js-string (string nickname)))
                                 nicknames))
                        (system::list-to-js-array
                         (mapcar (lambda (nickname)
                                   (system::array-to-js-string (string nickname)))
                                 use))))

(defmacro defpackage (package &body options)
  (declare (ignore options))
  (let ((g-package (gensym)))
    `(let ((,g-package ,package))
       (unless (find-package ,g-package)
         (make-package ,g-package :use '("CL"))))))

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
