(in-package :common-lisp)

(defun list-remove-duplicates (list)
  (let ((new-list '()))
    (do ((list list (cdr list)))
        ((null list))
      (unless (member (car list) (cdr list))
        (push (car list) new-list)))
    new-list))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mappend (function &rest lists)
    (apply #'append (apply #'mapcar function lists))))

(defvar +standard-class+)

(defstruct (standard-instance (:print-function print-standard-instance))
  class
  slots)

(defstruct slot-definition
  name
  initargs
  initform
  initfunction
  readers
  writers
  allocation)

(defun print-standard-instance (standard-instance stream depth)
  (declare (ignore depth))
  (print-unreadable-object (standard-instance stream)
    (format stream "~S ~S"
            (class-name (standard-instance-class standard-instance))
            (class-name standard-instance))))

(defun %slot-value (class slot-name)
  (let ((elt (assoc slot-name (standard-instance-slots class))))
    (unless elt
      (error "The slot ~S is unbound in the object ~S."
             slot-name class))
    (cdr elt)))

(defun (setf %slot-value) (value class slot-name)
  (let ((elt (assoc slot-name (standard-instance-slots class))))
    (if elt
        (setf (cdr elt) value)
        (push (cons slot-name value) (standard-instance-slots class)))
    value))

(defun class-name (class)
  (%slot-value class 'name))

(defun (setf class-name) (name class)
  (setf (%slot-value class 'name) name))

(defun class-precedence-list (class)
  (%slot-value class 'precedence-list))

(defun (setf class-precedence-list) (precedence-list class)
  (setf (%slot-value class 'precedence-list) precedence-list))

(defun class-direct-superclasses (class)
  (%slot-value class 'direct-superclasses))

(defun (setf class-direct-superclasses) (direct-superclasses class)
  (setf (%slot-value class 'direct-superclasses) direct-superclasses))

(defun class-direct-slots (class)
  (%slot-value class 'direct-slots))

(defun (setf class-direct-slots) (direct-slots class)
  (setf (%slot-value class 'direct-slots) direct-slots))

(defun class-direct-subclasses (class)
  (%slot-value class 'direct-subclasses))

(defun (setf class-direct-subclasses) (direct-subclasses class)
  (setf (%slot-value class 'direct-subclasses) direct-subclasses))

(defun class-direct-methods (class)
  (%slot-value class 'direct-methods))

(defun (setf class-direct-methods) (direct-methods class)
  (setf (%slot-value class 'direct-methods) direct-methods))

(defun class-direct-default-initargs (class)
  (%slot-value class 'direct-default-initargs))

(defun (setf class-direct-default-initargs) (direct-default-initargs class)
  (setf (%slot-value class 'direct-default-initargs) direct-default-initargs))

(defun class-slots (class)
  (%slot-value class 'slots))

(defun (setf class-slots) (slots class)
  (setf (%slot-value class 'slots) slots))

(defun class-default-initargs (class)
  (%slot-value class 'default-initargs))

(defun (setf class-default-initargs) (default-initargs class)
  (setf (%slot-value class 'default-initargs) default-initargs))

(defun class-of (x)
  (if (standard-instance-p x)
      (standard-instance-class x)
      (error "trap")))

(let ((class-table (make-hash-table)))
  (defun find-class (symbol &optional (errorp t) environment)
    (declare (ignore environment))
    (let ((class (gethash symbol class-table)))
      (when (and (null class) errorp)
        (error "There is no class named ~S." symbol))
      class))

  (defun (setf find-class) (class symbol &optional errorp environment)
    (declare (ignore errorp environment))
    (setf (gethash symbol class-table) class)))

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
                (list key
                      (case key
                        (:direct-default-initargs
                         (let ((initargs '()))
                           (do ((plist rest (cddr plist)))
                               ((null plist))
                             (push `(list ,(car plist)
                                          ,(cadr plist)
                                          (lambda () ,(cadr plist)))
                                   initargs))
                           `(list ,@(nreverse initargs))))
                        ((:metaclass :documentation)
                         `(quote ,(car rest)))
                        (otherwise
                         `(quote ,rest))))))
            options)))

(defun canonicalize-class (class &optional (errorp t))
  (if (symbolp class) (find-class class errorp) class))

(defun check-duplicate-direct-slots (direct-slots)
  (flet ((name (direct-slot) (getf direct-slot :name)))
    (do ((direct-slots direct-slots (cdr direct-slots)))
        ((null direct-slots))
      (let ((direct-slot (car direct-slots)))
        (when (member (name direct-slot)
                      (cdr direct-slots)
                      :key #'name)
          (error "Duplicate slot ~S" (name direct-slot)))))))

(defun check-duplicate-direct-default-initargs (direct-default-initargs class-name)
  (do ((direct-default-initargs direct-default-initargs (cdr direct-default-initargs)))
      ((null direct-default-initargs))
    (when (member (caar direct-default-initargs) (cdr direct-default-initargs) :key #'car)
      (error "Duplicate initialization argument name ~S in :DEFAULT-INITARGS DEFCLASS ~S."
             (caar direct-default-initargs)
             class-name))))

(defmacro defclass (name direct-superclasses direct-slot-specs &rest options)
  `(ensure-class ',name
                 :direct-superclasses ',direct-superclasses
                 :direct-slots ,(canonicalize-direct-slot-specs direct-slot-specs)
                 ,@(canonicalize-defclass-options options)))

(defun ensure-class (name &rest args)
  (apply #'ensure-class-using-class (find-class name nil) name args))

(defun ensure-class-using-class (class name &key direct-default-initargs direct-slots
                                                 direct-superclasses #|name|#
                                                 (metaclass 'standard-class)
                                            &allow-other-keys)
  (check-duplicate-direct-slots direct-slots)
  (check-duplicate-direct-default-initargs direct-default-initargs name)
  (setq direct-superclasses (mapcar #'find-class direct-superclasses))
  (cond (class
         (error "trap"))
        (t
         (setq metaclass (canonicalize-class metaclass))
         (setf (find-class name)
               (funcall (if (eq metaclass +standard-class+)
                            #'make-instance-standard-class
                            (error "make-instance trap"))
                        metaclass
                        :name name
                        :direct-default-initargs direct-default-initargs
                        :direct-slots direct-slots
                        :direct-superclasses direct-superclasses)))))

(defun make-instance-standard-class (metaclass &key name
                                                    direct-superclasses
                                                    direct-slots
                                                    direct-default-initargs
                                               &allow-other-keys)
  (declare (ignore metaclass))
  (let ((class (make-standard-instance :class +standard-class+)))
    (setf (class-name class) name)
    (setf (class-direct-subclasses class) '())
    (setf (class-direct-methods class) '())
    (setf (class-direct-default-initargs class) direct-default-initargs)
    (std-after-initialization-for-classes
     class
     :direct-superclasses direct-superclasses
     :direct-slots direct-slots)
    class))

(defun std-after-initialization-for-classes (class &key direct-superclasses
                                                        direct-slots
                                                   &allow-other-keys)
  (let ((supers (or direct-superclasses (list (find-class 'standard-object)))))
    (setf (class-direct-superclasses class) supers)
    (dolist (superclass supers)
      (push class (class-direct-subclasses superclass))))
  (let ((slots
          (mapcar (lambda (slot-plist)
                    (apply #'make-direct-slot-definition slot-plist))
                  direct-slots)))
    (setf (class-direct-slots class) slots)
    (dolist (direct-slot slots)
      (dolist (reader (slot-definition-readers direct-slot))
        (add-reader-method class reader (slot-definition-name direct-slot)))
      (dolist (writer (slot-definition-writers direct-slot))
        (add-writer-method class writer (slot-definition-name direct-slot))))
    (if (eq (class-of class) +standard-class+)
        (std-finalize-inheritance class)
        (error "finalize-inheritance trap"))))

(defun make-direct-slot-definition (&rest args
                                    &key name initargs initform initfunction readers writers
                                         (allocation :instance)
                                    &allow-other-keys)
  (declare (ignore name initargs initform initfunction readers writers allocation))
  (apply #'make-slot-definition args))

(defun make-effective-slot-definition (&rest args
                                       &key name initargs initform initfunction
                                            (allocation :instance)
                                       &allow-other-keys)
  (declare (ignore name initargs initform initfunction allocation))
  (apply #'make-slot-definition args))

(defun std-finalize-inheritance (class)
  (setf (class-precedence-list class)
        (if (eq (class-of class) +standard-class+)
            (std-compute-class-precedence-list class)
            (error "compute-class-precedence-list trap")))
  (setf (class-slots class)
        (if (eq (class-of class) +standard-class+)
            (std-compute-slots class)
            (error "compute-slots trap")))
  (setf (class-default-initargs class)
        (if (eq (class-of class) +standard-class+)
            (std-compute-default-initargs class)
            (error "compute-default-initargs trap"))))

(defun std-compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
    (topological-sort classes-to-order
                      (list-remove-duplicates
                       (mapcan #'local-precedence-ordering
                               classes-to-order))
                      #'std-tie-breaker-rule)))

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (dolist (element minimal-elements)
      (when (find element (class-direct-superclasses cpl-constituent))
        (return-from std-tie-breaker-rule element)))))

(defun local-precedence-ordering (class)
  (let ((supers (class-direct-superclasses class)))
    (mapcar #'list
            (cons class supers)
            supers)))

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (do () (nil)
      (let ((minimal-elements
              (remove-if
               (lambda (class)
                 (member class remaining-constraints
                         :key #'cadr))
               remaining-elements)))
        (when (null minimal-elements)
          (if (null remaining-elements)
              (return-from topological-sort result)
              (error "Inconsistent precedence graph.")))
        (let ((choice (if (null (cdr minimal-elements))
                          (car minimal-elements)
                          (funcall tie-breaker
                                   minimal-elements
                                   result))))
          (setq result (nconc result (list choice)))
          (setq remaining-elements
                (remove choice remaining-elements)) ; TODO: delete
          (setq remaining-constraints
                (remove choice remaining-constraints ; TODO: delete
                        :test #'member)))))))

(defun collect-superclasses* (class)
  (labels ((f (seen superclasses)
             (let ((class-to-process
                     (dolist (super superclasses nil)
                       (unless (find super seen)
                         (return super)))))
               (if (null class-to-process)
                   superclasses
                   (f (cons class-to-process seen)
                      (union (class-direct-superclasses class-to-process)
                             superclasses))))))
    (f nil (list class))))

(defun std-compute-slots (class)
  (let ((all-slots '()))
    (dolist (class (class-precedence-list class))
      (setq all-slots (append (class-direct-slots class) all-slots)))
    (let ((all-names (list-remove-duplicates
                      (mapcar #'slot-definition-name all-slots))))
      (mapcar (if (eq (class-of class) +standard-class+)
                  (lambda (name)
                    (std-compute-effective-slot-definition
                     class
                     (remove-if-not (lambda (slot)
                                      (eq name (slot-definition-name slot)))
                                    all-slots)))
                  (error "compute-effective-slot-definition trap"))
              all-names))))

(defun std-compute-effective-slot-definition (class direct-slots)
  (let ((initer (find-if #'identity direct-slots :key #'slot-definition-initfunction)))
    (make-effective-slot-definition
     :name (slot-definition-name (car direct-slots))
     :initform (if initer
                   (slot-definition-initform initer)
                   nil)
     :initfunction (if initer
                       (slot-definition-initfunction initer)
                       nil)
     :initargs (list-remove-duplicates
                (mappend #'slot-definition-initargs direct-slots))
     :allocation (slot-definition-allocation (car direct-slots)))))

(defun std-compute-default-initargs (class)
  (let ((names '())
        (initargs '()))
    (dolist (initarg (mappend #'class-direct-default-initargs (class-precedence-list class)))
      (let ((name (car initarg)))
        (unless (member name names)
          (push name names)
          (push initarg initargs))))
    (nreverse initargs)))

(defvar +standard-generic-function+)

(defun generic-function-name (gf)
  (%slot-value gf 'name))

(defun (setf generic-function-name) (name gf)
  (setf (%slot-value gf 'name) name))

(defun generic-function-methods (gf)
  (%slot-value gf 'methods))

(defun (setf generic-function-methods) (methods gf)
  (setf (%slot-value gf 'methods) methods))

(defun generic-function-lambda-list (gf)
  (%slot-value gf 'lambda-list))

(defun (setf generic-function-lambda-list) (lambda-list gf)
  (setf (%slot-value gf 'lambda-list) lambda-list))

(defun generic-function-method-class (gf)
  (%slot-value gf 'method-class))

(defun (setf generic-function-method-class) (method-class gf)
  (setf (%slot-value gf 'method-class) method-class))

(defun classes-to-emf-table (gf)
  (%slot-value gf 'classes-to-emf-table))

(defun (setf classes-to-emf-table) (classes-to-emf-table gf)
  (setf (%slot-value gf 'classes-to-emf-table) classes-to-emf-table))

(defun generic-function-min-argc (gf)
  (%slot-value gf 'min-argc))

(defun (setf generic-function-min-argc) (min-argc gf)
  (setf (%slot-value gf 'min-argc) min-argc))

(defun set-funcallable-instance-function (gf function)
  (setf (%slot-value gf 'funcallable-instance) function))

(defvar +standard-method+)

(defun method-function (method)
  (%slot-value method 'function))

(defun (setf method-function) (function method)
  (setf (%slot-value method 'function) function))

(defun method-generic-function (method)
  (%slot-value method 'generic-function))

(defun (setf method-generic-function) (generic-function method)
  (setf (%slot-value method 'generic-function) generic-function))

(defun method-lambda-list (method)
  (%slot-value method 'lambda-list))

(defun (setf method-lambda-list) (lambda-list method)
  (setf (%slot-value method 'lambda-list) lambda-list))

(defun method-specializers (method)
  (%slot-value method 'specializers))

(defun (setf method-specializers) (specializers method)
  (setf (%slot-value method 'specializers) specializers))

(defun method-qualifiers (method)
  (%slot-value method 'qualifiers))

(defun (setf method-qualifiers) (qualifiers method)
  (setf (%slot-value method 'qualifiers) qualifiers))

(defmacro defgeneric (function-name lambda-list &body options)
  `(ensure-generic-function ',function-name
                            :lambda-list ',lambda-list
                            ,@(canonicalize-defgeneric-options options)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun canonicalize-defgeneric-option (option)
    (case (car option)
      (:method-function-class
       (list :generic-function-class
             `(find-class ',(cadr option))))
      (:method-class
       (list :method-class
             `(find-class ',(cadr option))))
      (otherwise
       (list `',(car option) `',(cadr option)))))

  (defun canonicalize-defgeneric-options (options)
    (mappend #'canonicalize-defgeneric-option options)))

(let ((generic-function-table (make-hash-table)))
  (defun find-generic-function (function-name)
    (gethash function-name generic-function-table))
  (defun (setf find-generic-function) (gf function-name)
    (setf (gethash function-name generic-function-table) gf)))

(defun ensure-generic-function (function-name
                                &rest all-keys
                                &key lambda-list
                                     (generic-function-class +standard-generic-function+)
                                     (method-class +standard-method+)
                                &allow-other-keys)
  (apply #'ensure-generic-function-using-class
         (find-generic-function function-name)
         function-name
         all-keys))

(defun ensure-generic-function-using-class
    (generic-function-or-null function-name
     &rest all-keys
     &key lambda-list
          (generic-function-class +standard-generic-function+)
          (method-class +standard-method+)
     &allow-other-keys)
  (or generic-function-or-null
      (let ((gf (apply (if (eq generic-function-class +standard-generic-function+)
                           #'make-instance-standard-generic-function
                           (error "make-instance trap"))
                       generic-function-class
                       :name function-name
                       :method-class method-class
                       all-keys)))
        (setf (find-generic-function function-name) gf)
        gf)))

(defun make-instance-standard-generic-function
    (generic-function-class &key name lambda-list method-class &allow-other-keys)
  (declare (ignore generic-function-class))
  (let ((gf (make-standard-instance :class +standard-generic-function+)))
    (setf (generic-function-name gf) name)
    (setf (generic-function-lambda-list gf) lambda-list)
    (setf (generic-function-method-class gf) method-class)
    (setf (generic-function-methods gf) '())
    (setf (classes-to-emf-table gf) (make-hash-table :test 'equal))
    (destructuring-bind (&key required-args &allow-other-keys)
        (analyze-lambda-list lambda-list)
      (setf (generic-function-min-argc gf) (length required-args)))
    (finalize-generic-function gf)
    gf))

(defun finalize-generic-function (gf)
  (set-funcallable-instance-function gf (if (eq (class-of gf) +standard-generic-function+)
                                            (std-compute-discriminating-function gf)
                                            (error "compute-discriminating-function trap")))
  (setf (fdefinition (generic-function-name gf))
        gf) ;TODO: funcallable
  (clrhash (classes-to-emf-table gf)))

(defun required-classes (gf args)
  (let ((argc (length args)))
    (when (< argc (generic-function-min-argc gf))
      (error "Too few arguments to generic function ~S." gf))
    (let ((classes '()))
      (dotimes (i argc)
        (push (class-of (pop args)) classes))
      (nreverse classes))))

(defun std-compute-discriminating-function (gf)
  (lambda (&rest args)
    (let ((classes (required-classes gf args))
          (emfun (gethash classes (classes-to-emf-table gf))))
      (if emfun
          (funcall emfun args)
          (slow-method-lookup gf args classes)))))

(defun slow-method-lookup (gf args classes)
  (let* ((applicable-methods (%compute-applicable-methods gf args classes))
         (emfun (if (eq (class-of gf) +standard-generic-function+)
                    (std-compute-effective-method-function gf applicable-methods)
                    (error "compute-effective-method-function trap"))))
    (setf (gethash classes (classes-to-emf-table gf)) emfun)
    (funcall emfun args)))

(defun compute-applicable-methods (gf args)
  (%compute-applicable-methods gf args (required-classes gf args)))

(defun %compute-applicable-methods (gf args required-classes)
  (sort (remove-if-not (lambda (method)
                         (every #'subclassp
                                required-classes
                                (method-specializers method)))
                       (generic-function-methods gf))
        (if (eq (class-of gf) +standard-method+)
            (lambda (m1 m2)
              (std-method-more-specific-p gf m1 m2 required-classes))
            #+(or)
            (lambda (m1 m2)
              (method-more-specific-p gf m1 m2 required-classes))
            (error "method-more-specific-p trap"))))

(defun std-method-more-specific-p (gf m1 m2 required-classes)
  (mapc (lambda (spec1 spec2 required-class)
          (unless (eq spec1 spec2)
            (return-from std-method-more-specific-p
              (sub-specializer-p spec1 spec2 required-class))))
        (method-specializers m1)
        (method-specializers m2)
        required-classes)
  nil)

(defun subclassp (c1 c2)
  (not (null (find c2 (class-precedence-list c1)))))

(defun sub-specializer-p (c1 c2 arg)
  (let ((cpl (class-precedence-list arg)))
    (not (null (find c2 (cdr (member c1 cpl)))))))

(defun primary-method-p (method)
  (null (method-qualifiers method)))

(defun before-method-p (method)
  (equal '(:before) (method-qualifiers method)))

(defun after-method-p (method)
  (equal '(:after) (method-qualifiers method)))

(defun around-method-p (method)
  (equal '(:around) (method-qualifiers method)))

(defun std-compute-effective-method-function (gf methods)
  (let ((primaries (remove-if-not #'primary-method-p methods))
        (around (find-if #'around-method-p methods)))
    (if around
        (let ((next-fmfun
                (if (eq (class-of gf) +standard-generic-function+)
                    (std-compute-effective-method-function gf (remove around methods))
                    (compute-effective-method-function gf (remove around methods)))))
          (lambda (args)
            (funcall (method-function around) args next-emfun)))
        (let ((next-emfun (compute-primary-emfun (cdr primaries)))
              (befores (remove-if-not #'before-method-p methods))
              (afters (nreverse (remove-if-not #'after-method-p methods))))
          (lambda (args)
            (dolist (before befores)
              (funcall (method-function before) args nil))
            (multiple-value-prog1
                (funcall (method-function (car primaries)) args next-emfun)
              (dolist (after afters)
                (funcall (method-function after) args nil))))))))

(defun compute-primary-emfun (methods)
  (if (null methods)
      nil
      (let ((next-emfun (compute-primary-emfun (cdr methods))))
        (lambda (args)
          (funcall (method-function (car methods)) args next-emfun)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun analyze-lambda-list (lambda-list)
    (labels ((make-keyword (symbol)
               (intern (symbol-name symbol)
                       (find-package 'keyword)))
             (get-keyword-from-arg (arg)
               (if (listp arg)
                   (if (listp (car arg))
                       (caar arg)
                       (make-keyword (car arg)))
                   (make-keyword arg))))
      (let ((keys ())
            (key-args ())
            (required-names ())
            (required-args ())
            (specializers ())
            (rest-var nil)
            (optionals ())
            (auxs ())
            (allow-other-keys nil)
            (state :parsing-required))
        (dolist (arg lambda-list)
          (if (member arg lambda-list-keywords)
              (ecase arg
                (&optional
                 (setq state :parsing-optional))
                (&rest
                 (setq state :parsing-rest))
                (&key
                 (setq state :parsing-key))
                (&allow-other-keys
                 (setq allow-other-keys 't))
                (&aux
                 (setq state :parsing-aux)))
              (case state
                (:parsing-required
                 (push arg required-args)
                 (cond ((listp arg)
                        (push (car arg) required-names)
                        (push (cadr arg) specializers))
                       (t
                        (push arg required-names)
                        (push 't specializers))))
                (:parsing-optional (push arg optionals))
                (:parsing-rest (setq rest-var arg))
                (:parsing-key
                 (push (get-keyword-from-arg arg) keys)
                 (push arg key-args))
                (:parsing-aux (push arg auxs)))))
        (list  :required-names (nreverse required-names)
               :required-args (nreverse required-args)
               :specializers (nreverse specializers)
               :rest-var rest-var
               :keywords (nreverse keys)
               :key-args (nreverse key-args)
               :auxiliary-args (nreverse auxs)
               :optional-args (nreverse optionals)
               :allow-other-keys allow-other-keys))))

  (defun extract-lambda-list (analyzed-lambda-list)
    (destructuring-bind (&key required-names
                         rest-var
                         key-args
                         allow-other-keys
                         optional-args
                         auxiliary-args
                         &allow-other-keys)
        analyzed-lambda-list
      `(,@required-names
        ,@(when rest-var `(&rest ,rest-var))
        ,@(when (or key-args allow-other-keys) `(&key ,@key-args))
        ,@(when allow-other-keys '(&allow-other-keys))
        ,@(when optional-args `(&optional ,@optional-args))
        ,@(when auxiliary-args `(&aux ,@auxiliary-args)))))

  (defun extract-specializers (analyzed-lambda-list)
    (destructuring-bind (&key specializers &allow-other-keys)
        analyzed-lambda-list
      specializers))

  (defun parse-defmethod (args)
    (let ((function-name (pop args))
          (method-qualifiers '())
          (specialized-lambda-list))
      (do ()
          (nil)
        (let ((arg (car args)))
          (if (and arg (atom arg))
              (push (pop args) method-qualifiers)
              (progn
                (setq method-qualifiers (nreverse method-qualifiers))
                (return)))))
      (setq specialized-lambda-list (pop args))
      (let ((body args))
        (let ((analyzed-lambda-list (analyze-lambda-list specialized-lambda-list)))
          (values function-name
                  method-qualifiers
                  (extract-lambda-list analyzed-lambda-list)
                  (extract-specializers analyzed-lambda-list)
                  (list* 'block
                         (if (consp function-name)
                             (cadr function-name)
                             function-name)
                         body)))))))

(defmacro defmethod (&rest args)
  (multiple-value-bind (function-name
                        qualifiers
                        lambda-list
                        specializers
                        body)
      (parse-defmethod args)
    `(ensure-method ',function-name
                    :lambda-list ',lambda-list
                    :qualifiers ',qualifiers
                    :specializers ',specializers
                    :body ',body)))

(defun ensure-method (function-name &rest args &key lambda-list qualifiers specializers body)
  (declare (ignore qualifiers specializers body))
  (let* ((gf (ensure-generic-function function-name :lambda-list lambda-list))
         (method-class (generic-function-method-class gf))
         (method (apply (if (eq method-class +standard-method+)
                            #'make-instance-standard-method
                            (error "make-instance trap"))
                        method-class
                        args)))
    (add-method gf method)
    method))

(defun make-instance-standard-method (method-class &key lambda-list qualifiers specializers body)
  (declare (ignore method-class))
  (let ((method (make-standard-instance :class +standard-method+)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (setf (method-specializers method) specializers)
    (setf (method-generic-function method) nil)
    (setf (method-function method)
          (%make-method-lambda lambda-list body))
    method))

(defun %make-method-lambda (lambda-list body)
  (let ((g-args (gensym))
        (g-next-emfun (gensym))
        (g-call-next-method-args (gensym)))
    `(lambda (,g-args ,g-next-emfun)
       (flet ((call-next-method (&rest ,g-call-next-method-args)
                (if (null ,g-next-emfun)
                    (error "No next method")
                    (funcall ,g-next-emfun (or ,g-call-next-method-args ,g-args))))
              (next-method-p ()
                (not (null ,g-next-emfun))))
         (apply (lambda ,lambda-list ,body)
                ,g-args)))))

(defun add-method (gf method)
  )


(defun allocate-instance (class)
  (make-standard-instance :class class))

(defun initialize-instance (instance &rest initargs)
  )

(defun make-instance (class &rest initargs)
  (setq class (canonicalize-class class))
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun add-reader-method (class fn-name slot-name)
  )

(defun add-writer-method (class fn-name slot-name)
  )


(setq +standard-class+
      (let ((standard-class (make-standard-instance)))
        (setf (standard-instance-class standard-class) standard-class)
        (setf (class-name standard-class) 'standard-class)
        standard-class))

(setf (find-class 'standard-class) +standard-class+)

(setf (find-class 't)
      (let ((class (allocate-instance +standard-class+)))
        (setf (class-name class) 't)
        (setf (class-direct-subclasses class) ())
        (setf (class-direct-superclasses class) ())
        (setf (class-direct-methods class) ())
        (setf (class-direct-slots class) ())
        (setf (class-precedence-list class) (list class))
        (setf (class-slots class) ())
        (setf (class-direct-default-initargs class) ())
        class))

(defclass standard-object (t) ())

(setq +standard-generic-function+
      (defclass standard-generic-function ()
        ((name)
         (methods)
         (lambda-list)
         (method-class)
         (emf-table)
         (funcallable-instance))))

(setq +standard-method+
      (defclass standard-method ()
        ((function)
         (generic-function)
         (lambda-list)
         (specializers)
         (qualifiers))))
