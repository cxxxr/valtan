(in-package :common-lisp)

(defparameter *class-table* (make-hash-table))

(defstruct standard-object
  class
  slots)

(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (let ((class (gethash symbol *class-table*)))
    (when (and (null class) errorp)
      (error "There is no class named ~S." symbol))
    class))

(defun (setf find-class) (class symbol &optional errorp environment)
  (declare (ignore errorp environment))
  (setf (gethash symbol *class-table*) class))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun canonicalize-direct-slot (direct-slot-spec)
    (let ((result `(:name ,(if (consp direct-slot-spec)
                               (car direct-slot-spec)
                               direct-slot-spec)))
          (others '()))
      (do ((plist (if (consp direct-slot-spec) (cdr direct-slot-spec) nil)
                  (cddr plist)))
          ((null plist))
        (let ((key (car plist))
              (value (cadr plist)))
          (case key
            (:initform
             (setq result
                   (append result `(:initform ,value
                                    :initfunction (lambda () ,value)))))
            (:initarg
             (setf (getf result :initargs)
                   (nconc (getf result :initargs)
                          (list value))))
            ((:reader :writer :accessor)
             (case key
               ((:accessor :reader)
                (setf (getf result :readers)
                      (nconc (getf result :readers)
                             (list value)))))
             (case key
               (:writer
                (setf (getf result :writers)
                      (nconc (getf result :writers)
                             (list value))))
               (:accessor
                (setf (getf result :writers)
                      (nconc (getf result :writers)
                             (list `(setf ,value)))))))
            (:documentation
             (setf (getf result :documentation) value))
            (otherwise
             (setf (getf others key)
                   (nconc (getf others key) (list value)))))))
      (do ((plist others (cddr plist)))
          ((null plist))
        (let ((k (car plist))
              (v (cadr plist)))
          (setf (getf result k)
                (if (null (cdr v))
                    (car v)
                    v))))
      (do ((plist result (cddr plist)))
          ((null plist))
        (setf (car plist) `(quote ,(car plist)))
        (setf (cadr plist) `(quote ,(cadr plist))))
      `(list ,@result)))

  (defun canonicalize-direct-slot-specs (direct-slot-specs)
    `(list ,@(mapcar #'canonicalize-direct-slot direct-slot-specs)))

  (defun canonicalize-defclass-options (options)
    (mapcan (lambda (elt)
              (let ((key (car elt))
                    (rest (cdr elt)))
                (when (eq key :default-initargs)
                  (setq key :direct-default-initargs))
                (cons key
                      (case key
                        (:direct-default-initargs
                         (let ((initargs '()))
                           (do ((plist rest (cddr plist)))
                               ((null plist))
                             (push `(list ,(car plist)
                                          ,(cadr plist)
                                          (lambda () ,(cadr plist)))
                                   initargs))
                           (nreverse initargs)))
                        ((:metaclass :documentation)
                         `(quote ,(car rest)))
                        (otherwise
                         `(quote ,rest))))))
            options)))

(defun ensure-class-using-class (class name &key direct-default-initargs direct-slots
                                                 direct-superclasses #|name|# metaclass
                                            &allow-other-keys)
  (assert (null class))
  (let ((class (make-standard-object :class metaclass
                                     :slots (list (cons :name name)
                                                  (cons :direct-default-initargs
                                                        direct-default-initargs)
                                                  (cons :direct-slots
                                                        direct-slots)
                                                  (cons :direct-superclasses
                                                        direct-superclasses)))))
    (setf (find-class name) class)
    class))

(defun ensure-class (name &rest args)
  (apply #'ensure-class-using-class (find-class name nil) name args))

(defmacro defclass (name direct-super-classes direct-slot-specs &rest options)
  `(ensure-class ',name
                 :direct-super-classes ',direct-super-classes
                 :direct-slots ,(canonicalize-direct-slot-specs direct-slot-specs)
                 ,@(canonicalize-defclass-options options)))
