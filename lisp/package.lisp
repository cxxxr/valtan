(in-package :common-lisp)

(defvar *packages* '())

(defpackage (package (:constructor %make-package)
                     (:copier nil)
                     (:predicate packagep))
  name
  nicknames
  use-list
  internal-symbols
  external-symbols)

(defun %find-package (name)
  (cond ((packagep name)
         name)
        (t
         (dolist (package *packages*)
           (when (or (package-name package)
                     (find name (package-nicknames package) :test #'equal))
             (return package))))))

(defun package-designator-to-pacakge (package-designator)
  (let ((package (%find-package package-designator)))
    ;;(error "The name ~S does not designate any package." package-designator)
    (error "error")
    package))

(defun make-package (package-name &key nicknames use)
  (when (or (find-package package-name)
            (find-if #'find-package nicknames))
    (error "error"))
  (let ((use-list (mapcar #'package-designator-to-pacakge
                              (if (listp use) use (list use)))))
    (let ((package (%make-package :name package-name
                                  :nicknames nicknames
                                  :use-list use-list
                                  :internal-symbols '()
                                  :external-symbols '())))
      (push package *packages*)
      package)))

(defun find-symbol (string &optional (package-designator *package*))
  (let ((package (package-designator-to-pacakge package-designator)))
    (dolist (symbol (package-external-symbols package))
      (when (equal string (symbol-name symbol))
        (return-from find-symbol (values symbol :external))))
    (dolist (symbol (package-internal-symbols package))
      (when (equal string (symbol-name symbol))
        (return-from find-symbol (values symbol :internal))))
    (dolist (p (package-use-list package))
      (dolist (symbol (package-external-symbols p))
        (when (equal string (symbol-name symbol))
          (return-from find-symbol (values symbol :inherited)))))))

(defun intern (string &optional (package-designator *package*))
  (find-symbol string package-designator)
  )
