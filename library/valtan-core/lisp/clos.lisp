#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

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
  allocation
  location
  type
  documentation)

(defun print-standard-instance (standard-instance stream depth)
  (declare (ignore depth))
  (print-object standard-instance stream))

(defun std-slot-boundp (instance slot-name)
  (if (assoc slot-name (standard-instance-slots instance))
      t
      nil))

(defun slot-boundp (object slot-name)
  (when (and (*:structure-p object) (not (standard-instance-p object)))
    (return-from slot-boundp (slot-exists-p object slot-name)))
  (let ((slot (%instance-slot-definition object slot-name)))
    (if slot
        (let ((location (slot-definition-location slot)))
          (if location (not (null (second location))) (std-slot-boundp object slot-name)))
        (not (null (slot-missing (class-of object) object slot-name 'slot-boundp))))))

(defun %instance-slot-definition (object slot-name)
  (unless (symbolp slot-name) (type-error slot-name 'symbol))
  (unless (standard-instance-p object) (type-error object 'standard-object))
  (find slot-name (class-slots (class-of object)) :key #'slot-definition-name))

(defun %slot-value (class slot-name)
  (let ((elt (assoc slot-name (standard-instance-slots class))))
    (unless elt
      (error "The slot ~S is unbound in the object ~S."
             slot-name class))
    (cdr elt)))

(defun (cl:setf %slot-value) (value class slot-name)
  (let ((elt (assoc slot-name (standard-instance-slots class))))
    (if elt
        (setf (cdr elt) value)
        (push (cons slot-name value) (standard-instance-slots class)))
    value))

(defun slot-value (object slot-name)
  (when (and (*:structure-p object) (not (standard-instance-p object)))
    (let ((index (position slot-name (get (*:%structure-name object) 'structure-slots))))
      (unless index (error "No slot named ~S." slot-name))
      (return-from slot-value (*:%structure-ref object index))))
  (let ((slot (%instance-slot-definition object slot-name)))
    (unless slot
      (return-from slot-value (values (slot-missing (class-of object) object slot-name 'slot-value))))
    (unless (slot-boundp object slot-name)
      (return-from slot-value (values (slot-unbound (class-of object) object slot-name))))
    (let ((location (slot-definition-location slot)))
      (if location (third location) (%slot-value object slot-name)))))

(defun (cl:setf slot-value) (value object slot-name)
  (when (and (*:structure-p object) (not (standard-instance-p object)))
    (let ((index (position slot-name (get (*:%structure-name object) 'structure-slots))))
      (unless index (error "No slot named ~S." slot-name))
      (return-from slot-value (*:%structure-set object index value))))
  (let ((slot (%instance-slot-definition object slot-name)))
    (if slot
        (let ((location (slot-definition-location slot)))
          (if location
              (setf (second location) t (third location) value)
              (setf (%slot-value object slot-name) value)))
        (slot-missing (class-of object) object slot-name 'setf value)))
  value)

(defun slot-makunbound (object slot-name)
  (let ((slot (%instance-slot-definition object slot-name)))
    (if slot
        (let ((location (slot-definition-location slot)))
          (if location
              (setf (second location) nil (third location) nil)
              (setf (standard-instance-slots object)
                    (delete slot-name (standard-instance-slots object) :key #'car))))
        (slot-missing (class-of object) object slot-name 'slot-makunbound)))
  object)

(defun class-name (class)
  (%slot-value class 'name))

(defmacro with-slots (slots instance-form &body body)
  (let ((instance (gensym "INSTANCE")))
    `(let ((,instance ,instance-form))
       (declare (ignorable ,instance))
       (symbol-macrolet
           ,(mapcar (lambda (slot)
                      (if (symbolp slot)
                          `(,slot (slot-value ,instance ',slot))
                          (destructuring-bind (variable name) slot
                            `(,variable (slot-value ,instance ',name))))) slots)
         ,@body))))

(defmacro with-accessors (accessors instance-form &body body)
  (let ((instance (gensym "INSTANCE")))
    `(let ((,instance ,instance-form))
       (declare (ignorable ,instance))
       (symbol-macrolet
           ,(mapcar (lambda (binding)
                      (destructuring-bind (variable accessor) binding
                        `(,variable (,accessor ,instance)))) accessors)
         ,@body))))

(defun (cl:setf class-name) (name class)
  (setf (%slot-value class 'name) name))

(defun class-precedence-list (class)
  (%slot-value class 'precedence-list))

(defun (cl:setf class-precedence-list) (precedence-list class)
  (setf (%slot-value class 'precedence-list) precedence-list))

(defun class-direct-superclasses (class)
  (%slot-value class 'direct-superclasses))

(defun (cl:setf class-direct-superclasses) (direct-superclasses class)
  (setf (%slot-value class 'direct-superclasses) direct-superclasses))

(defun class-direct-slots (class)
  (%slot-value class 'direct-slots))

(defun (cl:setf class-direct-slots) (direct-slots class)
  (setf (%slot-value class 'direct-slots) direct-slots))

(defun class-direct-subclasses (class)
  (%slot-value class 'direct-subclasses))

(defun (cl:setf class-direct-subclasses) (direct-subclasses class)
  (setf (%slot-value class 'direct-subclasses) direct-subclasses))

;;; XXX:
;;; class-direct-methodsは今のところ使っていない、
;;; eql-specializerはdirect-methodsを持たないのでエラーが出る
;;; という理由でとりあえずコメントアウト

;; (defun class-direct-methods (class)
;;   (%slot-value class 'direct-methods))

;; (defun (cl:setf class-direct-methods) (direct-methods class)
;;   (setf (%slot-value class 'direct-methods) direct-methods))

(defun class-direct-default-initargs (class)
  (%slot-value class 'direct-default-initargs))

(defun (cl:setf class-direct-default-initargs) (direct-default-initargs class)
  (setf (%slot-value class 'direct-default-initargs) direct-default-initargs))

(defun class-slots (class)
  (%slot-value class 'slots))

(defun (cl:setf class-slots) (slots class)
  (setf (%slot-value class 'slots) slots))

(defun class-default-initargs (class)
  (%slot-value class 'default-initargs))

(defun (cl:setf class-default-initargs) (default-initargs class)
  (setf (%slot-value class 'default-initargs) default-initargs))

(defun class-of (x)
  (cond ((standard-instance-p x)
         (standard-instance-class x))
        ((null x)
         (find-class 'null))
        ((symbolp x)
         (find-class 'symbol))
        ((integerp x)
         (find-class 'integer))
        ((floatp x)
         (find-class 'float))
        ((rationalp x)
         (find-class 'ratio))
        ((cl:complexp x)
         (find-class 'complex))
        ((numberp x)
         (find-class 'number))
        ((consp x)
         (find-class 'cons))
        ((characterp x)
         (find-class 'character))
        ((hash-table-p x)
         (find-class 'hash-table))
        ((pathnamep x)
         (find-class 'pathname))
        ((packagep x)
         (find-class 'package))
        ((random-state-p x)
         (find-class 'random-state))
        ((readtablep x)
         (find-class 'readtable))
        ((streamp x)
         (find-class
           (case (*:%structure-name x)
             ((string-input-stream string-output-stream) 'string-stream)
             ((file-input-stream file-stream) 'file-stream)
             ((broadcast-stream concatenated-stream echo-stream synonym-stream two-way-stream)
              (*:%structure-name x))
             (otherwise 'stream))))
        ((stringp x)
         (find-class 'string))
        ((bit-vector-p x)
         (find-class 'bit-vector))
        ((vectorp x)
         (find-class 'vector))
        ((arrayp x)
         (find-class 'array))
        ((functionp x)
         (if (and (boundp '*generic-function-objects*)
                  (gethash x *generic-function-objects*))
             (find-class 'standard-generic-function)
             (find-class 'function)))
        ((*:structure-p x)
         (or (find-class (*:%structure-name x) nil) (find-class t)))
        (t
         (find-class t))))

(let ((class-table (make-hash-table)))
  (defun find-class (symbol &optional (errorp t) environment)
    (declare (ignore environment))
    (let ((class (or (gethash symbol class-table)
                     (and (get symbol 'structure-defined)
                          (fboundp '%ensure-structure-class)
                          (%ensure-structure-class symbol)))))
      (when (and (null class) errorp)
        (error "There is no class named ~S." symbol))
      class))

  (defun (cl:setf find-class) (class symbol &optional errorp environment)
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
        (unless (eq (car plist) :initfunction)
          (setf (cadr plist) `(quote ,(cadr plist))))
        (setf (car plist) `(quote ,(car plist))))
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
                             (push `(list ',(car plist)
                                          ',(cadr plist)
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
  `(ensure-class-using-class (find-class ',name nil)
                             ',name
                             :direct-superclasses ',direct-superclasses
                             :direct-slots ,(canonicalize-direct-slot-specs direct-slot-specs)
                             ,@(canonicalize-defclass-options options)))

(defun ensure-class (name &rest args)
  (apply #'ensure-class-using-class (find-class name nil) name args))

(defun ensure-class-using-class (class name &rest args
                                            &key direct-default-initargs direct-slots
                                                 direct-superclasses #|name|#
                                                 (metaclass 'standard-class)
                                            &allow-other-keys)
  (declare (ignore args))
  (check-duplicate-direct-slots direct-slots)
  (check-duplicate-direct-default-initargs direct-default-initargs name)
  (setq direct-superclasses (mapcar #'find-class direct-superclasses))
  (cond (class
         (std-after-initialization-for-classes class
                                               :direct-superclasses direct-superclasses
                                               :direct-slots direct-slots)
         class)
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
    ;; (setf (class-direct-methods class) '())
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
  (declare (ignore name initargs initform initfunction readers writers))
  (apply #'make-slot-definition
         :allocation allocation
         :location (when (eq allocation :class) (list nil nil nil)) args))

(defun make-effective-slot-definition (&rest args
                                       &key name initargs initform initfunction
                                            (allocation :instance)
                                       &allow-other-keys)
  (declare (ignore name initargs initform initfunction))
  (apply #'make-slot-definition :allocation allocation args))

(defun std-finalize-inheritance (class)
  (setf (class-precedence-list class)
        (if (eq (class-of class) +standard-class+)
            (std-compute-class-precedence-list class)
            (error "compute-class-precedence-list trap")))
  (setf (class-slots class)
        (if (eq (class-of class) +standard-class+)
            (std-compute-slots class)
            (error "compute-slots trap")))
  (dolist (slot (class-slots class))
    (let ((location (slot-definition-location slot)))
      (when (and location (not (first location)))
        (setf (first location) t)
        (when (slot-definition-initfunction slot)
          (setf (third location) (funcall (slot-definition-initfunction slot))
                (second location) t)))))
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
      (when (member element (class-direct-superclasses cpl-constituent))
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
                (delete choice remaining-elements))
          (setq remaining-constraints
                (delete choice remaining-constraints
                        :test #'member)))))))

(defun collect-superclasses* (class)
  (labels ((f (seen superclasses)
             (let ((class-to-process
                     (dolist (super superclasses nil)
                       (unless (member super seen)
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
      (setq all-slots (append all-slots (class-direct-slots class))))
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
  (declare (ignore class))
  (let ((initer (find-if (lambda (slot) (slot-definition-initfunction slot))
                         direct-slots)))
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
     :allocation (slot-definition-allocation (car direct-slots))
     :location (slot-definition-location (car direct-slots)))))

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

(defun (cl:setf generic-function-name) (name gf)
  (setf (%slot-value gf 'name) name))

(defun generic-function-methods (gf)
  (%slot-value gf 'methods))

(defun (cl:setf generic-function-methods) (methods gf)
  (setf (%slot-value gf 'methods) methods))

(defun generic-function-lambda-list (gf)
  (%slot-value gf 'lambda-list))

(defun (cl:setf generic-function-lambda-list) (lambda-list gf)
  (setf (%slot-value gf 'lambda-list) lambda-list))

(defun generic-function-method-class (gf)
  (%slot-value gf 'method-class))

(defun (cl:setf generic-function-method-class) (method-class gf)
  (setf (%slot-value gf 'method-class) method-class))

(defun classes-to-emf-table (gf)
  (%slot-value gf 'classes-to-emf-table))

(defun (cl:setf classes-to-emf-table) (classes-to-emf-table gf)
  (setf (%slot-value gf 'classes-to-emf-table) classes-to-emf-table))

(defun generic-function-min-argc (gf)
  (%slot-value gf 'min-argc))

(defun (cl:setf generic-function-min-argc) (min-argc gf)
  (setf (%slot-value gf 'min-argc) min-argc))

(defun set-funcallable-instance-function (gf function)
  (setf (%slot-value gf 'funcallable-instance) function))

(defvar +standard-method+)

(defun method-function (method)
  (%slot-value method 'function))

(defun (cl:setf method-function) (function method)
  (setf (%slot-value method 'function) function))

(defun method-generic-function (method)
  (%slot-value method 'generic-function))

(defun (cl:setf method-generic-function) (generic-function method)
  (setf (%slot-value method 'generic-function) generic-function))

(defun method-lambda-list (method)
  (%slot-value method 'lambda-list))

(defun (cl:setf method-lambda-list) (lambda-list method)
  (setf (%slot-value method 'lambda-list) lambda-list))

(defun method-specializers (method)
  (%slot-value method 'specializers))

(defun (cl:setf method-specializers) (specializers method)
  (setf (%slot-value method 'specializers) specializers))

(defun method-qualifiers (method)
  (%slot-value method 'qualifiers))

(defun (cl:setf method-qualifiers) (qualifiers method)
  (setf (%slot-value method 'qualifiers) qualifiers))

(defvar *generic-function-objects* (make-hash-table :test 'eq))
(defvar *generic-function-combinations* (make-hash-table :test 'eq))
(defvar *method-combination-definitions* (make-hash-table :test 'eq))
(defvar *traced-functions*)

(defmacro defgeneric (function-name lambda-list &body options)
  `(progn
     (ensure-generic-function ',function-name
                              :lambda-list ',lambda-list
                              ,@(canonicalize-defgeneric-options
                                 (remove :method options :key #'car)))
     ,@(mapcar (lambda (option) `(defmethod ,function-name ,@(cdr option)))
               (remove-if-not (lambda (option) (eq (car option) :method)) options))
     (fdefinition ',function-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun canonicalize-defgeneric-option (option)
    (case (car option)
      (:method-combination (list :method-combination (list 'quote (cdr option))))
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

(let ((generic-function-table (make-hash-table))
      (setf-generic-function-table (make-hash-table)))

  (defun find-generic-function (function-name)
    (if (consp function-name)
        (gethash (cadr function-name) setf-generic-function-table)
        (gethash function-name generic-function-table)))

  (defun (cl:setf find-generic-function) (gf function-name)
    (if (consp function-name)
        (setf (gethash (cadr function-name) setf-generic-function-table)
              gf)
        (setf (gethash function-name generic-function-table)
              gf))))

(defun %ensure-generic-function (function-name
                                &rest all-keys
                                &key lambda-list
                                     (generic-function-class +standard-generic-function+)
                                     (method-class +standard-method+)
                                &allow-other-keys)
  (declare (ignore lambda-list generic-function-class method-class))
  (unless (find-generic-function function-name)
    (when (or (if (consp function-name)
                  (get (cadr function-name) '*:fdefinition-setf)
                  (fboundp function-name))
              (and (symbolp function-name)
                   (or (and (fboundp 'special-operator-p) (special-operator-p function-name))
                       (and (fboundp 'macro-function) (macro-function function-name)))))
      (error "~S already names an ordinary function, macro or special operator." function-name)))
  (apply #'ensure-generic-function-using-class
         (find-generic-function function-name)
         function-name
         all-keys))

(defun ensure-generic-function (function-name &rest all-keys)
  (apply #'%ensure-generic-function function-name all-keys)
  (fdefinition function-name))

(defun ensure-generic-function-using-class
    (generic-function-or-null function-name
     &rest all-keys
     &key lambda-list
          (generic-function-class +standard-generic-function+)
          (method-class +standard-method+)
     &allow-other-keys)
  (declare (ignore lambda-list))
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
    (generic-function-class &key name lambda-list method-class
                                 (method-combination '(standard)) &allow-other-keys)
  (declare (ignore generic-function-class))
  (let ((gf (make-standard-instance :class +standard-generic-function+)))
    (setf (gethash gf *generic-function-combinations*) method-combination)
    (setf (generic-function-name gf) name)
    (setf (generic-function-lambda-list gf) lambda-list)
    (setf (generic-function-method-class gf) method-class)
    (setf (generic-function-methods gf) '())
    (setf (classes-to-emf-table gf) (make-hash-table :test 'equal))
    (destructuring-bind (&key required-args optional-args rest-var &allow-other-keys)
        (analyze-lambda-list lambda-list)
      (setf (generic-function-min-argc gf) (length required-args)
            (%slot-value gf 'max-argc)
            (unless (or rest-var (member '&key lambda-list))
              (+ (length required-args) (length optional-args)))))
    (finalize-generic-function gf)
    gf))

(defun finalize-generic-function (gf)
  (setf (%slot-value gf 'eql-specializers) (generic-function-eql-specializers gf))
  (let ((discriminating-function
          (if (std-slot-boundp gf 'funcallable-instance)
              (%slot-value gf 'funcallable-instance)
              (if (eq (class-of gf) +standard-generic-function+)
              (std-compute-discriminating-function gf)
              (error "compute-discriminating-function trap")))))
    (set-funcallable-instance-function gf discriminating-function)
    (setf (gethash discriminating-function *generic-function-objects*) gf)
    ;; (setf (fdefinition (generic-function-name gf)) gf)
    (let ((trace-entry (and (boundp '*traced-functions*)
                            (assoc (generic-function-name gf) *traced-functions* :test #'equal))))
      (setf (fdefinition (generic-function-name gf))
            (if (and trace-entry (eq (second trace-entry) discriminating-function))
                (third trace-entry) discriminating-function)))
    (clrhash (classes-to-emf-table gf))))

(defun required-classes (gf args)
  (let ((argc (length args)))
    (when (< argc (generic-function-min-argc gf))
      (error 'program-error))
    (let ((maximum (%slot-value gf 'max-argc)))
      (when (and maximum (> argc maximum)) (error 'program-error)))
    (let ((classes '()))
      (dotimes (i argc)
        (push (class-of (pop args)) classes))
      (nreverse classes))))

(defun std-compute-discriminating-function (gf)
  (lambda (&rest args)
    (let* ((classes (required-classes gf args))
           (key (method-cache-key gf args classes))
           (emfun (gethash key (classes-to-emf-table gf))))
      (if emfun
          (funcall emfun args)
          (slow-method-lookup gf args classes key)))))

(defun generic-function-eql-specializers (gf)
  (let ((specializers nil))
    (dolist (method (generic-function-methods gf))
      (let ((position 0))
        (dolist (specializer (method-specializers method))
          (when (typep specializer 'eql-specializer)
            (pushnew (cons position specializer) specializers :test #'equal))
          (incf position))))
    specializers))

(defun method-cache-key (gf args classes)
  ;; Classes alone cannot distinguish an EQL method from other calls with the
  ;; same argument class. Include the identities of the matching specializers.
  (let ((matches nil))
    (dolist (entry (%slot-value gf 'eql-specializers))
      (when (eql (nth (car entry) args) (eql-specializer-object (cdr entry)))
        (push entry matches)))
    (if matches (cons classes matches) classes)))

(defun slow-method-lookup (gf args classes key)
  (let ((applicable-methods (%compute-applicable-methods gf args classes)))
    (unless applicable-methods
      (return-from slow-method-lookup
        (apply #'no-applicable-method (%slot-value gf 'funcallable-instance) args)))
    (let ((emfun (if (eq (class-of gf) +standard-generic-function+)
                     (std-compute-effective-method-function gf applicable-methods)
                     (error "compute-effective-method-function trap"))))
      (setf (gethash key (classes-to-emf-table gf)) emfun)
      (funcall emfun args))))

(defun compute-applicable-methods (gf args)
  (when (functionp gf)
    (setq gf (or (gethash gf *generic-function-objects*) (type-error gf 'generic-function))))
  (%compute-applicable-methods gf args (required-classes gf args)))

(defun %compute-applicable-methods (gf args required-classes)
  (stable-sort (remove-if-not (lambda (method)
                                (do ((required-classes required-classes (cdr required-classes))
                                     (specializers (method-specializers method) (cdr specializers))
                                     (args args (cdr args)))
                                    ((null specializers) t)
                                  (let ((required-class (car required-classes))
                                        (specializer (car specializers))
                                        (arg (car args)))
                                    (unless (if (typep specializer 'eql-specializer)
                                                (eql arg (eql-specializer-object specializer))
                                                (subclassp required-class specializer))
                                      (return nil)))))
                              (generic-function-methods gf))
               (if (eq (class-of gf) +standard-generic-function+)
                   (lambda (m1 m2)
                     (std-method-more-specific-p gf m1 m2 required-classes))
                   #+(or)
                   (lambda (m1 m2)
                     (method-more-specific-p gf m1 m2 required-classes))
                   (error "method-more-specific-p trap"))))

(defun std-method-more-specific-p (gf m1 m2 required-classes)
  (declare (ignore gf))
  (mapc (lambda (spec1 spec2 required-class)
          (unless (eq spec1 spec2)
            (return-from std-method-more-specific-p
              (sub-specializer-p spec1 spec2 required-class))))
        (method-specializers m1)
        (method-specializers m2)
        required-classes)
  nil)

(defun subclassp (c1 c2)
  (not (null (member c2 (class-precedence-list c1)))))

(defun sub-specializer-p (c1 c2 arg)
  (cond ((typep c1 'eql-specializer) t)
        ((typep c2 'eql-specializer) nil)
        (t (let ((cpl (class-precedence-list arg)))
             (not (null (member c2 (cdr (member c1 cpl)))))))))

(defun primary-method-p (method)
  (null (method-qualifiers method)))

(defun before-method-p (method)
  (equal '(:before) (method-qualifiers method)))

(defun after-method-p (method)
  (equal '(:after) (method-qualifiers method)))

(defun around-method-p (method)
  (equal '(:around) (method-qualifiers method)))

(defun std-compute-effective-method-function (gf methods)
  (let ((combination (gethash gf *generic-function-combinations*)))
    (when (and combination (not (eq (car combination) 'standard)))
      (return-from std-compute-effective-method-function
        (compute-user-method-combination gf methods combination))))
  (let ((primaries (remove-if-not #'primary-method-p methods))
        (around (find-if #'around-method-p methods)))
    (when (null primaries)
      (error "No primary methods for the generic function ~S." gf))
    (if around
        (let ((next-emfun
                (if (eq (class-of gf) +standard-generic-function+)
                    (std-compute-effective-method-function gf (remove around methods))
                    (error "compute-effective-method-function trap")
                    #+(or)
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
  (defun method-combination-argument-extractor (lambda-list gf args)
    ;; Generate the value bindings separately from the forms exposed to the
    ;; combination body. Initforms retain the definition's lexical environment.
    (let* ((whole (when (eq (car lambda-list) '&whole) (second lambda-list)))
           (parsed (compiler::parse-lambda-list (if whole (cddr lambda-list) lambda-list)))
           (shape (gensym "SHAPE")) (required (gensym "REQUIRED"))
           (optional (gensym "OPTIONAL")) (tail (gensym "TAIL"))
           (bindings nil) (variables nil) (index 0))
      (flet ((bind-argument (name value)
               (push (list name value) bindings)
               (push name variables)))
        (when whole (bind-argument whole args))
        (dolist (var (compiler::parsed-lambda-list-vars parsed))
          (bind-argument var `(if (< ,index ,required) (nth ,index ,args) nil))
          (incf index))
        (setq index 0)
        (dolist (parameter (compiler::parsed-lambda-list-optionals parsed))
          (let ((present (gensym "PRESENT")))
            (push `(,present (and (< ,index ,optional)
                                 (< (+ ,required ,index) (length ,args)))) bindings)
            (bind-argument (first parameter)
                           `(if ,present (nth (+ ,required ,index) ,args) ,(second parameter)))
            (when (third parameter) (bind-argument (third parameter) present)))
          (incf index))
        (when (compiler::parsed-lambda-list-rest-var parsed)
          (bind-argument (compiler::parsed-lambda-list-rest-var parsed) tail))
        (dolist (parameter (compiler::parsed-lambda-list-keys parsed))
          (let ((entry (gensym "ENTRY")))
            (push `(,entry (nth-value 2 (get-properties ,tail '(,(fourth parameter))))) bindings)
            (bind-argument (first parameter)
                           `(if ,entry (second ,entry) ,(second parameter)))
            (when (third parameter)
              (bind-argument (third parameter) `(not (null ,entry))))))
        (dolist (parameter (compiler::parsed-lambda-list-auxs parsed))
          (bind-argument (first parameter) (second parameter))))
      (setq variables (nreverse variables))
      (values variables
              `(lambda (,args)
                 (let* ((,shape (analyze-lambda-list (generic-function-lambda-list ,gf)))
                        (,required (length (getf ,shape :required-args)))
                        (,optional (length (getf ,shape :optional-args)))
                        (,tail (nthcdr (+ ,required ,optional) ,args))
                        ,@(nreverse bindings))
                   (list ,@variables)))))))

(defun order-method-group (methods order)
  (case order
    (:most-specific-first methods)
    (:most-specific-last (reverse methods))
    (otherwise (error "Invalid method combination order: ~S" order))))

(defmacro define-method-combination (name &rest definition)
  (if (or (null definition) (and (symbolp (car definition)) (car definition)))
      (destructuring-bind (&key documentation identity-with-one-argument (operator name)) definition
        (let ((order (gensym "ORDER")) (around (gensym "AROUND")) (primary (gensym "PRIMARY")))
          `(progn
             (define-method-combination ,name (&optional (,order :most-specific-first))
               ((,around (:around)) (,primary (,name) :required t))
               (unless (member ,order '(:most-specific-first :most-specific-last))
                 (error "Invalid method combination order: ~S" ,order))
               (let* ((calls (mapcar (lambda (method) (list 'call-method method))
                                    (if (eq ,order :most-specific-last) (reverse ,primary) ,primary)))
                      (form (if (and ',identity-with-one-argument (null (cdr calls)))
                                (car calls) (cons ',operator calls))))
                 (if ,around
                     (list 'call-method (car ,around)
                           (append (cdr ,around) (list (list 'make-method form))))
                     form)))
             (setf (get ',name 'method-combination-documentation) ,documentation)
             ',name)))
      (destructuring-bind (lambda-list groups &body body) definition
        (unless (and (symbolp name) (listp lambda-list) (listp groups))
          (error "Invalid long-form method combination: ~S" name))
        (let* ((arguments (when (and (consp (car body)) (eq (caar body) :arguments))
                            (cdr (pop body))))
               (gf-name (when (and (consp (car body)) (eq (caar body) :generic-function))
                          (second (pop body))))
               (gf (gensym "GF")) (args (gensym "ARGS"))
               (options (gensym "OPTIONS")) (group-values (gensym "GROUPS"))
               (argument-forms (gensym "ARGUMENT-FORMS")) (extractor (gensym "EXTRACTOR"))
               (declarations nil) (documentation nil))
          (do () ((not (or (and (stringp (car body)) (cdr body))
                          (and (consp (car body)) (eq (caar body) 'declare)))))
            (if (stringp (car body)) (setq documentation (pop body))
                (push (pop body) declarations)))
          (multiple-value-bind (variables extraction)
              (when arguments (method-combination-argument-extractor arguments gf args))
            `(progn
               (setf (gethash ',name *method-combination-definitions*)
                     (list ',groups
                           (lambda (,options ,group-values ,gf)
                             (destructuring-bind ,lambda-list ,options
                               ,@(nreverse declarations)
                               (let ((,extractor ,extraction)
                                     (,argument-forms (mapcar (lambda (var) (gensym (symbol-name var)))
                                                              ',variables)))
                                 (let (,@(when gf-name
                                           `((,gf-name (%slot-value ,gf 'funcallable-instance))))
                                       ,@(let ((i -1))
                                           (mapcar (lambda (var) `(,var (nth ,(incf i) ,argument-forms)))
                                                   variables))
                                       ,@(let ((i -1))
                                           (mapcar (lambda (group)
                                                     `(,(car group)
                                                       (order-method-group
                                                        (nth ,(incf i) ,group-values)
                                                        ,(getf (member-if #'keywordp (cdr group))
                                                               :order :most-specific-first))))
                                                   groups)))
                                   (values (block ,name ,@body) ,argument-forms ,extractor)))))))
               (setf (get ',name 'method-combination-documentation) ,documentation)
               ',name))))))

(define-method-combination + :identity-with-one-argument t)
(define-method-combination and :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument t)
(define-method-combination list)
(define-method-combination max :identity-with-one-argument t)
(define-method-combination min :identity-with-one-argument t)
(define-method-combination nconc :identity-with-one-argument t)
(define-method-combination or :identity-with-one-argument t)
(define-method-combination progn :identity-with-one-argument t)

(defun qualifier-pattern-matches-p (pattern qualifiers)
  (cond ((eq pattern '*) t)
        ((null pattern) (null qualifiers))
        ((and (consp pattern) (consp qualifiers))
         (and (or (eq (car pattern) '*) (eql (car pattern) (car qualifiers)))
              (qualifier-pattern-matches-p (cdr pattern) (cdr qualifiers))))))

(defun method-group-matches-p (group method)
  (do ((selectors (cdr group) (cdr selectors)))
      ((or (null selectors) (keywordp (car selectors))) nil)
    (let ((selector (car selectors)))
      (when (if (and selector (symbolp selector) (not (eq selector '*)))
                (funcall selector (method-qualifiers method))
                (qualifier-pattern-matches-p selector (method-qualifiers method)))
        (return t)))))

(defun call-combination-method (method args next-methods)
  (if (functionp method)
      (funcall method args)
      (funcall (method-function method) args
               (when next-methods
                 (lambda (new-args)
                   (call-combination-method (car next-methods) new-args (cdr next-methods)))))))

(defun compute-user-method-combination (gf methods combination)
  (let* ((definition (or (gethash (car combination) *method-combination-definitions*)
                         (error "Unknown method combination: ~S" (car combination))))
         (groups (first definition))
         (buckets (mapcar (lambda (group) (list (car group))) groups)))
    (dolist (method methods)
      (let ((group (find-if (lambda (group) (method-group-matches-p group method)) groups)))
        (unless group (error "Method qualifiers do not match a method group: ~S" (method-qualifiers method)))
        (push method (cdr (assoc (car group) buckets)))))
    (let* ((group-values
             (mapcar (lambda (group)
                       (let* ((options (member-if #'keywordp (cdr group)))
                              (methods (nreverse (cdr (assoc (car group) buckets)))))
                         (when (and (getf options :required) (null methods))
                           (error "Required method group is empty: ~S" (car group)))
                         methods)) groups))
           (method-vars (mapcar (lambda (method) (cons method (gensym "METHOD"))) methods))
           (args (gensym "ARGS")))
      (multiple-value-bind (form argument-vars extractor)
          (funcall (second definition) (cdr combination) group-values gf)
        (labels ((method-expression (method)
                 (cond ((assoc method method-vars) (cdr (assoc method method-vars)))
                       ((and (consp method) (eq (car method) 'make-method))
                        `(lambda (,args) ,(walk (second method))))
                       (t (error "Invalid CALL-METHOD argument: ~S" method))))
               (walk (form)
                 (cond ((atom form) form)
                       ((eq (car form) 'quote) form)
                       ((eq (car form) 'call-method)
                        `(call-combination-method ,(method-expression (second form)) ,args
                           (list ,@(mapcar #'method-expression (third form)))))
                       (t (walk-list form))))
               (walk-list (tail)
                 (if (consp tail)
                     (cons (walk (car tail)) (walk-list (cdr tail)))
                     tail)))
          (let ((effective
                  (apply (eval `(lambda ,(mapcar #'cdr method-vars)
                                  (lambda (,args ,@argument-vars) ,(walk form)))) methods)))
            (if extractor
                (lambda (args) (apply effective args (funcall extractor args)))
                effective)))))))

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
            (keyp nil)
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
                 (setq keyp t)
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
               :keyp keyp
               :key-args (nreverse key-args)
               :auxiliary-args (nreverse auxs)
               :optional-args (nreverse optionals)
               :allow-other-keys allow-other-keys))))

  (defun extract-lambda-list (analyzed-lambda-list)
    (destructuring-bind (&key required-names
                         rest-var
                         key-args
                         keyp
                         allow-other-keys
                         optional-args
                         auxiliary-args
                         &allow-other-keys)
        analyzed-lambda-list
      `(,@required-names
        ,@(when optional-args `(&optional ,@optional-args))
        ,@(when rest-var `(&rest ,rest-var))
        ,@(when (or keyp key-args allow-other-keys) `(&key ,@key-args))
        ,@(when allow-other-keys '(&allow-other-keys))
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
      (multiple-value-bind (body declares docstring)
          (compiler::parse-body args t)
        (let ((analyzed-lambda-list (analyze-lambda-list specialized-lambda-list)))
          (values function-name
                  method-qualifiers
                  (extract-lambda-list analyzed-lambda-list)
                  (extract-specializers analyzed-lambda-list)
                  `(,@(when docstring `(,docstring))
                    ,@(when declares `((declare ,@declares)))
                    (block ,(if (consp function-name)
                                (cadr function-name)
                                function-name)
                      ,@body))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kludge-arglist (lambda-list)
    (let* ((aux-position (position '&aux lambda-list))
           (arguments (if aux-position (subseq lambda-list 0 aux-position) lambda-list))
           (auxiliary (when aux-position (nthcdr aux-position lambda-list))))
      (append arguments
              (cond ((member '&key arguments)
                     (unless (member '&allow-other-keys arguments) '(&allow-other-keys)))
                    ((not (member '&rest arguments)) '(&key &allow-other-keys)))
              auxiliary))))

(defmacro %make-method-lambda (lambda-list body)
  (let ((g-args (gensym))
        (g-next-emfun (gensym))
        (g-call-next-method-args (gensym)))
    `(lambda (,g-args ,g-next-emfun)
       (flet ((call-next-method (&rest ,g-call-next-method-args)
                (if (null ,g-next-emfun)
                    (error "No next method")
                    (funcall ,g-next-emfun (or ,g-call-next-method-args ,g-args))))
              (next-method-p ()
                #+(and (not valtan) sbcl)(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                (not (null ,g-next-emfun))))
         (apply (lambda ,lambda-list ,@body)
                ,g-args)))))

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
                    :body ',body
                    :function (%make-method-lambda ,(kludge-arglist lambda-list) ,body))))

(defun ensure-method (function-name &rest args &key lambda-list qualifiers specializers body
                                                    function)
  (declare (ignore qualifiers specializers body function))
  (let* ((gf (%ensure-generic-function function-name :lambda-list lambda-list))
         (method-class (generic-function-method-class gf))
         (method (apply (if (eq method-class +standard-method+)
                            #'make-instance-standard-method
                            (error "make-instance trap"))
                        method-class
                        args)))
    (add-method gf method)
    method))

(defun canonicalize-method-specializers (specializers)
  (mapcar #'canonicalize-method-specializer specializers))

(defun canonicalize-method-specializer (specializer)
  (cond ((standard-instance-p specializer)
         specializer)
        ((symbolp specializer)
         (find-class specializer))
        ((and (consp specializer)
              (eq (car specializer) 'eql))
         (let ((object (cadr specializer)))
           (intern-eql-specializer
            (if (and (consp object)
                     (eq 'quote (car object)))
                (cadr object)
                object))))
        (t
         (error "Unknown specializer: ~S" specializer))))

(defun make-instance-standard-method (method-class &key lambda-list qualifiers
                                                        specializers body function)
  (declare (ignore method-class body))
  (let ((method (make-standard-instance :class +standard-method+)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (setf (method-specializers method) (canonicalize-method-specializers specializers))
    (setf (method-generic-function method) nil)
    (setf (method-function method) function)
    method))

(defun add-method (gf method)
  (when (functionp gf)
    (setq gf (or (gethash gf *generic-function-objects*) (type-error gf 'generic-function))))
  (when (and (method-generic-function method) (not (eq (method-generic-function method) gf)))
    (error "Method already belongs to another generic function."))
  (let ((old-method
          (find-method gf
                       (method-qualifiers method)
                       (method-specializers method)
                       nil)))
    (when old-method
      (remove-method gf old-method))
    (setf (method-generic-function method) gf)
    (push method (generic-function-methods gf))
    ;; (dolist (specializer (method-specializers method))
    ;;   (pushnew method (class-direct-methods specializer)))
    (finalize-generic-function gf)
    (fdefinition (generic-function-name gf))))

(defun remove-method (gf method)
  (when (functionp gf)
    (setq gf (or (gethash gf *generic-function-objects*) (type-error gf 'generic-function))))
  (setf (generic-function-methods gf)
        (delete method (generic-function-methods gf)))
  (when (eq (method-generic-function method) gf) (setf (method-generic-function method) nil))
  ;; (dolist (class (method-specializers method))
  ;;   (setf (class-direct-methods class)
  ;;         (remove method (class-direct-methods class))))
  (finalize-generic-function gf)
  (fdefinition (generic-function-name gf)))

(defun find-method (gf qualifiers specializers &optional (errorp t))
  (when (functionp gf)
    (setq gf (or (gethash gf *generic-function-objects*)
                 (type-error gf 'generic-function))))
  (let ((method
          (find-if (lambda (method)
                     (and (equal qualifiers (method-qualifiers method))
                          (equal specializers (method-specializers method))))
                   (generic-function-methods gf))))
    (if (and (null method) errorp)
        (error "No such method for ~S." (generic-function-name gf))
        method)))

(defun add-reader-method (class fn-name slot-name)
  (ensure-method fn-name
                 :lambda-list '(object)
                 :qualifiers '()
                 :specializers `(,class)
                 :body `(slot-value ',slot-name)
                 :function (lambda (args next-emfun)
                             (declare (ignore next-emfun))
                             (destructuring-bind (object &key &allow-other-keys) args
                               (slot-value object slot-name)))))

(defun add-writer-method (class fn-name slot-name)
  (ensure-method fn-name
                 :lambda-list '(new-value object)
                 :qualifiers ()
                 :specializers `(,(find-class t) ,class)
                 :body `(setf (slot-value object ',slot-name)
                              new-value)
                 :function (lambda (args next-emfun)
                             (declare (ignore next-emfun))
                             (destructuring-bind (new-value object &key &allow-other-keys) args
                               (setf (slot-value object slot-name) new-value)))))


(setq +standard-class+
      (let ((standard-class (make-standard-instance)))
        (setf (standard-instance-class standard-class) standard-class)
        (setf (class-name standard-class) 'standard-class)
        (setf (class-direct-subclasses standard-class) ())
        (setf (class-direct-superclasses standard-class) ())
        ;; (setf (class-direct-methods standard-class) ())
        (setf (class-direct-slots standard-class) ())
        (setf (class-precedence-list standard-class) (list standard-class))
        (setf (class-slots standard-class) ())
        (setf (class-direct-default-initargs standard-class) ())
        standard-class))

(setf (find-class 'standard-class) +standard-class+)

(setf (find-class 't)
      (let ((class (make-standard-instance :class +standard-class+)))
        (setf (class-name class) 't)
        (setf (class-direct-subclasses class) ())
        (setf (class-direct-superclasses class) ())
        ;; (setf (class-direct-methods class) ())
        (setf (class-direct-slots class) ())
        (setf (class-precedence-list class) (list class))
        (setf (class-slots class) ())
        (setf (class-direct-default-initargs class) ())
        class))

(defclass standard-object (t) ())

(defclass class (standard-object) ())

(defclass standard-class (class)
  ((name :initarg :name)
   (direct-superclasses :initarg :direct-superclasses)
   (direct-slots :initform '())
   (direct-default-initargs :initform '())
   (class-precedence-list)
   (effective-slots)
   (direct-subclasses :initform '())
   (direct-methods :initform '())))

(defclass built-in-class (class) ())
(defclass function (t) ())
(defclass generic-function (function standard-object) ())
(defclass method (standard-object) ())
(defclass method-combination (t) ())

(setq +standard-generic-function+
      (defclass standard-generic-function (generic-function)
        ((name)
         (methods)
         (lambda-list)
         (method-class)
         (emf-table)
         (funcallable-instance))))

(setq +standard-method+
      (defclass standard-method (method)
        ((function)
         (generic-function)
         (lambda-list)
         (specializers)
         (qualifiers))))

;;(defclass t (t) ())
(defclass array (t) ())
(defclass sequence (t) ())
(defclass vector (array sequence) ())
(defclass bit-vector (vector) ())
(defclass character (t) ())
(defclass number (t) ())
(defclass real (number) ())
(defclass rational (real) ())
(defclass complex (number) ())
(defclass list (sequence) ())
(defclass cons (list) ())
(defclass float (real) ())
(defclass integer (rational) ())
(defclass hash-table (t) ())
(defclass symbol (t) ())
(defclass null (symbol list) ())
(defclass package (t) ())
(defclass pathname (t) ())
(defclass logical-pathname (pathname) ())
(defclass random-state (t) ())
(defclass ratio (rational) ())
(defclass readtable (t) ())
(defclass stream (t) ())
(defclass string-stream (stream) ())
(defclass file-stream (stream) ())
(defclass broadcast-stream (stream) ())
(defclass concatenated-stream (stream) ())
(defclass echo-stream (stream) ())
(defclass synonym-stream (stream) ())
(defclass two-way-stream (stream) ())
(defclass string (vector) ())

(dolist (name '(t sequence array vector bit-vector character number real rational
               complex list cons float integer function hash-table symbol null
               package pathname logical-pathname random-state ratio readtable stream string
               method-combination string-stream file-stream broadcast-stream concatenated-stream
               echo-stream synonym-stream two-way-stream))
  (setf (standard-instance-class (find-class name)) (find-class 'built-in-class)))

(defclass structure-class (class) ())
(defclass structure-object (t) ())
(setf (standard-instance-class (find-class 'structure-object)) (find-class 'structure-class))

(defun %ensure-structure-class (name)
  (let* ((class (make-standard-instance :class (find-class 'structure-class)))
         (superclass (find-class 'structure-object))
         (slots (mapcar (lambda (slot) (make-slot-definition :name slot :allocation :instance))
                        (get name 'structure-slots))))
    (setf (class-name class) name
          (class-direct-superclasses class) (list superclass)
          (class-precedence-list class) (cons class (class-precedence-list superclass))
          (class-direct-subclasses class) nil
          (class-direct-slots class) slots
          (class-slots class) slots
          (class-direct-default-initargs class) nil
          (class-default-initargs class) nil
          (find-class name) class)
    class))

(defgeneric allocate-instance (class &key))

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (make-standard-instance :class class))

(defgeneric shared-initialize (instance slot-names &key))

(defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (key value tail)
          (get-properties initargs (slot-definition-initargs slot))
        (declare (ignore key))
        (if tail
            (setf (slot-value instance slot-name) value)
            (when (and (not (slot-boundp instance slot-name))
                       (or (eq slot-names t)
                           (member slot-name slot-names))
                       (slot-definition-initfunction slot))
              (setf (slot-value instance slot-name)
                    (funcall (slot-definition-initfunction slot))))))))
  instance)

(defgeneric initialize-instance (instance &key))

(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defgeneric slot-missing (class object slot-name operation &optional new-value))
(defmethod slot-missing ((class t) object slot-name operation &optional new-value)
  (declare (ignore class operation new-value))
  (error "No slot named ~S in ~S." slot-name object))

(defgeneric slot-unbound (class object slot-name))
(defmethod slot-unbound ((class t) object slot-name)
  (declare (ignore class))
  (error 'unbound-slot :instance object :name slot-name))

(defun check-initialization-initargs (instance initargs methods)
  (unless (evenp (length initargs)) (error 'program-error))
  (when (getf initargs :allow-other-keys) (return-from check-initialization-initargs nil))
  (let ((valid (cons :allow-other-keys
                     (mappend #'slot-definition-initargs (class-slots (class-of instance))))))
    (dolist (method methods)
      (let ((keywords-p nil))
        (dolist (parameter (method-lambda-list method))
          (cond ((eq parameter '&allow-other-keys)
                 (return-from check-initialization-initargs nil))
                ((eq parameter '&key) (setq keywords-p t))
                ((eq parameter '&aux) (setq keywords-p nil))
                (keywords-p
                 (let ((name (if (consp parameter) (car parameter) parameter)))
                   (push (if (consp name) (car name) (intern (symbol-name name) :keyword)) valid)))))))
    (do ((args initargs (cddr args))) ((null args))
      (unless (member (car args) valid)
        (error 'program-error :format-control "Invalid initialization argument ~S."
               :format-arguments (list (car args)))))))

(defgeneric reinitialize-instance (instance &rest initargs &key &allow-other-keys))
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (check-initialization-initargs
    instance initargs
    (append (compute-applicable-methods #'reinitialize-instance (list instance))
            (compute-applicable-methods #'shared-initialize (list instance nil))))
  (apply #'shared-initialize instance nil initargs))

(defgeneric update-instance-for-different-class (previous current &rest initargs &key &allow-other-keys))
(defmethod update-instance-for-different-class ((previous standard-object) (current standard-object)
                                               &rest initargs)
  (let ((added-slots
          (mapcar #'slot-definition-name
                  (remove-if-not
                    (lambda (slot)
                      (and (eq (slot-definition-allocation slot) :instance)
                           (not (slot-exists-p previous (slot-definition-name slot)))))
                    (class-slots (class-of current))))))
    (check-initialization-initargs
      current initargs
      (append (compute-applicable-methods #'update-instance-for-different-class (list previous current))
              (compute-applicable-methods #'shared-initialize (list current added-slots))))
    (apply #'shared-initialize current added-slots initargs)))

(defgeneric change-class (instance new-class &rest initargs &key &allow-other-keys))
(defmethod change-class ((instance t) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))
(defmethod change-class ((instance standard-object) (new-class standard-class) &rest initargs)
  (let* ((previous (make-standard-instance :class (class-of instance)
                                          :slots (copy-alist (standard-instance-slots instance))))
         (slots (class-slots new-class))
         (values nil))
    ;; Preserve only real slots shared by name. A shared old slot becomes a
    ;; local value; shared destination storage remains owned by its class.
    (dolist (slot slots)
      (let ((name (slot-definition-name slot)))
        (when (and (eq (slot-definition-allocation slot) :instance)
                   (slot-exists-p previous name) (slot-boundp previous name))
          (push (cons name (slot-value previous name)) values))))
    ;; Ordinary instances also carry an internal display name. Do not let it
    ;; initialize a newly introduced user slot named NAME.
    (when (and (not (find 'name slots :key #'slot-definition-name))
               (std-slot-boundp previous 'name))
      (push (cons 'name (%slot-value previous 'name)) values))
    (setf (standard-instance-class instance) new-class
          (standard-instance-slots instance) values)
    (apply #'update-instance-for-different-class previous instance initargs)
    instance))

(defgeneric make-instance (class &key))

(defmethod make-instance ((symbol symbol) &rest initargs)
  (apply #'make-instance (find-class symbol) initargs))

(defvar *standard-object-counter* 0)

(defmethod make-instance ((class standard-class) &rest initargs)
  (dolist (default (class-default-initargs class))
    (multiple-value-bind (key value tail) (get-properties initargs (list (first default)))
      (declare (ignore key value))
      (unless tail
        (setq initargs (append initargs (list (first default) (funcall (third default))))))))
  (let ((instance (apply #'allocate-instance class initargs)))
    (setf (class-name instance) (format nil "{~A}" (incf *standard-object-counter*)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun slot-exists-p (object slot-name)
  (unless (symbolp slot-name) (type-error slot-name 'symbol))
  (not (null (find slot-name (class-slots (class-of object)) :key #'slot-definition-name))))


(defgeneric no-applicable-method (generic-function &rest arguments))
(defmethod no-applicable-method ((generic-function t) &rest arguments)
  (error "No applicable method for ~S with arguments ~S." generic-function arguments))

(defgeneric print-object (object stream))

(defmethod print-object ((object standard-class) stream)
  (print-unreadable-object (object stream)
    (format stream "~S ~A"
            (class-name (standard-instance-class object))
            (class-name object))))

(defmethod print-object ((object standard-object) stream)
  (print-unreadable-object (object stream)
    (format stream "~S ~A"
            (class-name (standard-instance-class object))
            (class-name object))))

(defmethod print-object ((object standard-generic-function) stream)
  (print-unreadable-object (object stream)
    (format stream "~S ~S ~S"
            (class-name (standard-instance-class object))
            (generic-function-name object)
            (generic-function-lambda-list object))))

(defmethod print-object ((object standard-method) stream)
  (print-unreadable-object (object stream)
    (format stream "~S ~S ~S"
            (class-name (standard-instance-class object))
            (if (method-generic-function object)
                (generic-function-name (method-generic-function object))
                nil)
            (method-specializers object))))


(defmacro define-condition (name parent-types slot-specs &body options)
  (let ((report (second (assoc :report options)))
        (g-condition (gensym))
        (g-stream (gensym)))
    `(progn
       (defclass ,name ,(or parent-types '(condition))
         ,slot-specs
         ,@options)
       ,(cond ((null report))
              ((stringp report)
               `(defmethod print-object ((,g-condition ,name) ,g-stream)
                  (write-string ,report ,g-stream)))
              (t
               `(defmethod print-object ((,g-condition ,name) ,g-stream)
                  (if *print-escape*
                      (call-next-method)
                      (,report ,g-condition ,g-stream)))))
       ',name)))

(define-condition base-condition (standard-object) ())
(define-condition condition (base-condition) ())
(define-condition warning (condition) ())
(define-condition style-warning (warning) ())
(define-condition serious-condition (condition) ())
(define-condition error (serious-condition) ())
(define-condition cell-error (error)
  ((name :initarg :name :reader cell-error-name)))
(define-condition parse-error (error) ())
(define-condition storage-condition (serious-condition) ())
(define-condition simple-condition (condition)
  ((format-control
    :initarg :format-control
    :reader simple-condition-format-control)
   (format-arguments
    :initarg :format-arguments
    :initform nil
    :reader simple-condition-format-arguments))
  (:report
   (lambda (c stream)
     (apply #'format
            stream
            (simple-condition-format-control c)
            (simple-condition-format-arguments c)))))
(define-condition simple-error (simple-condition error) ())
(define-condition simple-warning (simple-condition warning) ())

(define-condition package-error (simple-condition error)
  ((package :initarg :package :reader package-error-package))
  (:report (lambda (condition stream)
             (format stream "Package error: ~S" (package-error-package condition)))))

(define-condition type-error (error)
  ((datum
    :initarg :datum
    :reader type-error-datum)
   (expected-type
    :initarg :expected-type
    :reader type-error-expected-type))
  (:report
   (lambda (c stream)
     (format stream "The value ~S is not of the expected type ~A"
             (type-error-datum c)
             (type-error-expected-type c)))))

(define-condition simple-type-error (simple-condition type-error) ())

(define-condition arithmetic-error (error)
  ((operation :initarg :operation :reader arithmetic-error-operation)
   (operands :initarg :operands :reader arithmetic-error-operands)))
(define-condition division-by-zero (arithmetic-error) ())
(define-condition floating-point-invalid-operation (arithmetic-error) ())
(define-condition floating-point-overflow (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())
(define-condition floating-point-inexact (arithmetic-error) ())

(define-condition control-error (error) ())

(define-condition program-error (simple-condition error) ()
  (:default-initargs :format-control "Program error"))

(define-condition undefined-function (cell-error) ()
  (:report
   (lambda (c stream)
     (format stream "The function ~S is undefined" (cell-error-name c)))))

(define-condition unbound-variable (cell-error) ()
  (:report
   (lambda (c stream)
     (format stream "The variable ~S is unbound" (cell-error-name c)))))

(define-condition unbound-slot (cell-error)
  ((instance :initarg :instance :reader unbound-slot-instance)))

(define-condition stream-error (error)
  ((stream :initarg :stream :reader stream-error-stream)))
(define-condition end-of-file (stream-error) ())
(define-condition print-not-readable (error)
  ((object :initarg :object :reader print-not-readable-object))
  (:report (lambda (condition stream)
             (let ((*print-readably* nil))
               (format stream "Object ~S cannot be printed readably."
                       (print-not-readable-object condition))))))
(define-condition file-error (error)
  ((pathname :initarg :pathname :reader file-error-pathname)
   (message :initarg :message :initform "File error"))
  (:report (lambda (condition stream)
             (format stream "~A: ~A" (slot-value condition 'message) (file-error-pathname condition)))))

(define-condition reader-error (parse-error stream-error)
  ())

(define-condition simple-reader-error (reader-error simple-condition)
  ())

(defun make-condition (type &rest initargs)
  (let ((class (find-class type nil)))
    (when (or (null class)
              (not (subclassp class (find-class 'base-condition))))
      (type-error type 'condition)))
  (apply #'make-instance type initargs))

(defclass eql-specializer ()
  ((object :initarg :object :reader eql-specializer-object)))

(let ((eql-specializer-table (make-hash-table)))
  (defun intern-eql-specializer (object)
    (or (gethash object eql-specializer-table)
        (setf (gethash object eql-specializer-table)
              (make-instance 'eql-specializer :object object)))))
