;;; Valtan's MOP uses the same class graph, slots and dispatch as CL:CLOS.
;;; Load the protocol after bootstrapping CLOS, then enable the core hooks.
(defpackage :closer-mop
  (:use :cl)
  (:nicknames :c2mop)
  (:import-from :cl
   #:class-slots #:class-direct-slots #:class-direct-superclasses
   #:class-direct-subclasses #:class-precedence-list
   #:class-direct-default-initargs #:class-default-initargs
   #:slot-definition-name #:slot-definition-initargs #:slot-definition-initform
   #:slot-definition-initfunction #:slot-definition-allocation
   #:slot-definition-location #:slot-definition-type
   #:slot-definition-readers #:slot-definition-writers
   #:ensure-class #:method-specializers #:method-qualifiers #:method-lambda-list
   #:method-function #:method-generic-function #:generic-function-name
   #:generic-function-methods #:generic-function-lambda-list
   #:generic-function-method-class)
  (:export
   #:class-slots #:class-direct-slots #:class-direct-superclasses
   #:class-direct-subclasses #:class-precedence-list #:class-prototype
   #:class-direct-default-initargs #:class-default-initargs
   #:slot-definition-name #:slot-definition-initargs #:slot-definition-initform
   #:slot-definition-initfunction #:slot-definition-allocation
   #:slot-definition-location #:slot-definition-type
   #:slot-definition-readers #:slot-definition-writers
   #:ensure-class #:method-specializers #:method-qualifiers #:method-lambda-list
   #:method-function #:method-generic-function #:generic-function-name
   #:generic-function-methods #:generic-function-lambda-list
   #:generic-function-method-class
   #:standard-class #:funcallable-standard-class #:forward-referenced-class
   #:slot-definition #:standard-slot-definition #:direct-slot-definition
   #:effective-slot-definition #:standard-direct-slot-definition
   #:standard-effective-slot-definition #:validate-superclass
   #:direct-slot-definition-class #:effective-slot-definition-class
   #:compute-effective-slot-definition #:compute-slots
   #:compute-class-precedence-list #:compute-default-initargs
   #:finalize-inheritance #:class-finalized-p #:ensure-finalized
   #:slot-value-using-class #:slot-boundp-using-class #:slot-makunbound-using-class
   #:standard-instance-access #:funcallable-standard-instance-access
   #:extract-lambda-list #:extract-specializer-names
   #:compute-applicable-methods #:compute-applicable-methods-using-classes))
(in-package :closer-mop)

(defclass forward-referenced-class (class) ())
(defclass funcallable-standard-class (standard-class) ())
(defclass slot-definition (standard-object)
  ((cl::name :initarg :name)
   (cl::initargs :initarg :initargs :initform nil)
   (cl::initform :initarg :initform :initform nil)
   (cl::initfunction :initarg :initfunction :initform nil)
   (cl::allocation :initarg :allocation :initform :instance)
   (cl::location :initarg :location :initform nil)
   (cl::type :initarg :type :initform t)
   (cl::readers :initarg :readers :initform nil)
   (cl::writers :initarg :writers :initform nil)
   (cl::documentation :initarg :documentation :initform nil)))
(defclass standard-slot-definition (slot-definition) ())
(defclass direct-slot-definition (slot-definition) ())
(defclass effective-slot-definition (slot-definition) ())
(defclass standard-direct-slot-definition (standard-slot-definition direct-slot-definition) ())
(defclass standard-effective-slot-definition (standard-slot-definition effective-slot-definition) ())

;; Bootstrap slot descriptions are structures. New descriptions are CLOS
;; metaobjects so applications can specialize their class and initialization.
(dolist (name '(cl::name cl::initargs cl::initform cl::initfunction cl::allocation
               cl::location cl::type cl::readers cl::writers cl::documentation))
  (let* ((slot-name name)
         (accessor (intern (concatenate 'string "SLOT-DEFINITION-" (symbol-name name)) :cl))
         (reader (fdefinition accessor))
         (writer (fdefinition (list 'setf accessor))))
    (setf (fdefinition accessor)
          (lambda (object)
            (if (cl::standard-instance-p object)
                (cl::%slot-value object slot-name)
                (funcall reader object))))
    (setf (fdefinition (list 'setf accessor))
          (lambda (value object)
            (if (cl::standard-instance-p object)
                (setf (cl::%slot-value object slot-name) value)
                (funcall writer value object))))))

(defvar *finalized* (make-hash-table :test 'eq))
(defvar *prototypes* (make-hash-table :test 'eq))
(defun class-finalized-p (class)
  (multiple-value-bind (value present) (gethash class *finalized*)
    (if present value (cl::std-slot-boundp class 'cl::slots))))
(defun ensure-finalized (class)
  (unless (class-finalized-p class) (finalize-inheritance class))
  class)
(defun class-prototype (class)
  (ensure-finalized class)
  (or (gethash class *prototypes*)
      (setf (gethash class *prototypes*) (allocate-instance class))))

(defgeneric validate-superclass (class superclass))
(defmethod validate-superclass ((class class) (superclass class))
  (or (eq (class-of class) (class-of superclass))
      (eq (class-of superclass) (find-class 'standard-class))
      (typep superclass 'forward-referenced-class)
      (eq superclass (find-class t))))
(defgeneric direct-slot-definition-class (class &rest initargs))
(defmethod direct-slot-definition-class ((class standard-class) &rest initargs)
  (declare (ignore initargs)) (find-class 'standard-direct-slot-definition))
(defgeneric effective-slot-definition-class (class &rest initargs))
(defmethod effective-slot-definition-class ((class standard-class) &rest initargs)
  (declare (ignore initargs)) (find-class 'standard-effective-slot-definition))
(defgeneric compute-class-precedence-list (class))
(defmethod compute-class-precedence-list ((class standard-class))
  (cl::std-compute-class-precedence-list class))
(defgeneric compute-default-initargs (class))
(defmethod compute-default-initargs ((class standard-class))
  (cl::std-compute-default-initargs class))
(defgeneric compute-effective-slot-definition (class name direct-slots))
(defmethod compute-effective-slot-definition ((class standard-class) name direct-slots)
  (let* ((first (car direct-slots))
         (initer (find-if #'slot-definition-initfunction direct-slots))
         (args (list :name name
                     :initargs (remove-duplicates (mapcan (lambda (s) (copy-list (slot-definition-initargs s))) direct-slots))
                     :initform (when initer (slot-definition-initform initer))
                     :initfunction (when initer (slot-definition-initfunction initer))
                     :allocation (slot-definition-allocation first)
                     :location (when (eq (slot-definition-allocation first) :class)
                                 (slot-definition-location first))
                     :type (cons 'and (mapcar #'slot-definition-type direct-slots)))))
    (apply #'make-instance (apply #'effective-slot-definition-class class args) args)))
(defgeneric compute-slots (class))
(defmethod compute-slots ((class standard-class))
  (let* ((slots (mapcan (lambda (super) (copy-list (class-direct-slots super)))
                        (class-precedence-list class)))
         (names (remove-duplicates (mapcar #'slot-definition-name slots) :from-end t)))
    (mapcar (lambda (name)
              (compute-effective-slot-definition class name
                (remove name slots :key #'slot-definition-name :test-not #'eq))) names)))
(defgeneric finalize-inheritance (class))
(defmethod finalize-inheritance ((class forward-referenced-class))
  (error "Cannot finalize forward-referenced class ~S" (class-name class)))
(defmethod finalize-inheritance ((class standard-class))
  (dolist (super (class-direct-superclasses class)) (ensure-finalized super))
  (setf (class-precedence-list class) (compute-class-precedence-list class))
  (let ((slots (compute-slots class)) (index 0))
    (dolist (slot slots)
      (if (eq (slot-definition-allocation slot) :instance)
          (setf (slot-definition-location slot) (prog1 index (incf index)))
          (let ((cell (or (slot-definition-location slot) (list nil nil nil))))
            (setf (slot-definition-location slot) cell)
            (unless (first cell)
              (setf (first cell) t)
              (when (slot-definition-initfunction slot)
                (setf (third cell) (funcall (slot-definition-initfunction slot))
                      (second cell) t))))))
    (setf (class-slots class) slots))
  (setf (class-default-initargs class) (compute-default-initargs class)
        (gethash class *finalized*) t)
  class)

(defun unresolved-superclass-p (class)
  (some (lambda (super) (typep super 'forward-referenced-class))
        (cl::collect-superclasses* class)))
(defun configure-class (class initargs)
  (let* ((old-supers (if (cl::std-slot-boundp class 'cl::direct-superclasses)
                         (class-direct-superclasses class) nil))
         (supers (if (member :direct-superclasses initargs)
                     (mapcar (lambda (super)
                               (if (symbolp super)
                                   (or (find-class super nil)
                                       (ensure-class super :metaclass 'forward-referenced-class)) super))
                             (getf initargs :direct-superclasses))
                     old-supers)))
    (unless (or supers (typep class 'forward-referenced-class))
      (setf supers (list (find-class 'standard-object))))
    (dolist (old old-supers)
      (setf (class-direct-subclasses old) (remove class (class-direct-subclasses old))))
    (setf (class-direct-superclasses class) supers)
    (dolist (super supers)
      (unless (validate-superclass class super)
        (error "Invalid superclass ~S for ~S" super class))
      (pushnew class (class-direct-subclasses super))))
  (when (member :direct-default-initargs initargs)
    (setf (class-direct-default-initargs class) (getf initargs :direct-default-initargs)))
  (when (member :direct-slots initargs)
    (setf (class-direct-slots class)
          (mapcar (lambda (args)
                    (let* ((slot (apply #'make-instance (apply #'direct-slot-definition-class class args) args))
                           (name (slot-definition-name slot)))
                      (when (eq (slot-definition-allocation slot) :class)
                        (setf (slot-definition-location slot) (list nil nil nil)))
                      (dolist (reader (slot-definition-readers slot))
                        (cl::add-reader-method class reader name))
                      (dolist (writer (slot-definition-writers slot))
                        (cl::add-writer-method class writer name))
                      slot)) (getf initargs :direct-slots))))
  (setf (gethash class *finalized*) nil)
  (unless (unresolved-superclass-p class) (finalize-inheritance class))
  ;; Recompute subclasses after a superclass is redefined or a forward
  ;; reference is resolved; method caches depend on the same class identities.
  (labels ((update (super)
             (dolist (sub (class-direct-subclasses super))
               (setf (gethash sub *finalized*) nil)
               (unless (unresolved-superclass-p sub) (finalize-inheritance sub))
               (update sub))))
    (update class))
  class)

(defmethod initialize-instance :after ((class class) &rest initargs)
  (configure-class class initargs))
(defmethod reinitialize-instance :after ((class class) &rest initargs &key &allow-other-keys)
  (configure-class class initargs))

(defun cl::%mop-ensure-class (class name &rest args &key (metaclass 'standard-class) &allow-other-keys)
  (cl::check-duplicate-direct-slots (getf args :direct-slots))
  (cl::check-duplicate-direct-default-initargs (getf args :direct-default-initargs) name)
  (setf metaclass (cl::canonicalize-class metaclass))
  (remf args :metaclass)
  (if class
      (progn
        (unless (eq (class-of class) metaclass) (change-class class metaclass))
        (apply #'reinitialize-instance class :name name args)
        ;; The class identity stays the same when its precedence changes.
        ;; Existing generic dispatch caches must not keep the old method order.
        (maphash (lambda (function gf)
                   (declare (ignore function))
                   (cl::invalidate-generic-function-caches gf)) cl::*generic-function-objects*))
      (setf class (apply #'make-instance metaclass :name name args)))
  (setf (find-class name) class))

(defgeneric slot-boundp-using-class (class object slot))
(defmethod slot-boundp-using-class ((class class) object slot)
  (let ((location (slot-definition-location slot)))
    (if (consp location) (not (null (second location)))
        (cl::std-slot-boundp object (slot-definition-name slot)))))
(defgeneric slot-value-using-class (class object slot))
(defmethod slot-value-using-class ((class class) object slot)
  (unless (slot-boundp-using-class class object slot)
    (return-from slot-value-using-class (slot-unbound class object (slot-definition-name slot))))
  (let ((location (slot-definition-location slot)))
    (if (consp location) (third location) (cl::%slot-value object (slot-definition-name slot)))))
(defgeneric (setf slot-value-using-class) (value class object slot))
(defmethod (setf slot-value-using-class) (value (class class) object slot)
  (let ((location (slot-definition-location slot)))
    (if (consp location) (setf (second location) t (third location) value)
        (setf (cl::%slot-value object (slot-definition-name slot)) value)))
  value)
(defgeneric slot-makunbound-using-class (class object slot))
(defmethod slot-makunbound-using-class ((class class) object slot)
  (let ((location (slot-definition-location slot)))
    (if (consp location) (setf (second location) nil (third location) nil)
        (setf (cl::standard-instance-slots object)
              (delete (slot-definition-name slot) (cl::standard-instance-slots object) :key #'car))))
  object)
(defun cl::%mop-slot-value (class object slot) (slot-value-using-class class object slot))
(defun (setf cl::%mop-slot-value) (value class object slot)
  (setf (slot-value-using-class class object slot) value))
(defun cl::%mop-slot-boundp (class object slot) (slot-boundp-using-class class object slot))
(defun cl::%mop-slot-makunbound (class object slot) (slot-makunbound-using-class class object slot))

(defun location-slot-name (object location)
  (unless (and (integerp location) (<= 0 location)) (error "Invalid instance location ~S" location))
  (let ((slot (find location (class-slots (class-of object)) :key #'slot-definition-location)))
    (unless slot (error "No instance slot at location ~S" location))
    (slot-definition-name slot)))
(defun standard-instance-access (object location)
  (cl::%slot-value object (location-slot-name object location)))
(defun (setf standard-instance-access) (value object location)
  (setf (cl::%slot-value object (location-slot-name object location)) value))
(defun funcallable-standard-instance-access (object location)
  (standard-instance-access object location))
(defun (setf funcallable-standard-instance-access) (value object location)
  (setf (standard-instance-access object location) value))
(defun extract-lambda-list (specialized-lambda-list)
  (cl::extract-lambda-list (cl::analyze-lambda-list specialized-lambda-list)))
(defun extract-specializer-names (specialized-lambda-list)
  (getf (cl::analyze-lambda-list specialized-lambda-list) :specializers))

;; Dispatch the public function object and its internal metaobject identically.
(defun generic-metaobject (gf)
  (if (functionp gf)
      (or (gethash gf cl::*generic-function-objects*) (error "Not a generic function: ~S" gf)) gf))
(fmakunbound 'compute-applicable-methods)
(defgeneric compute-applicable-methods (generic-function arguments))
(defmethod compute-applicable-methods ((gf standard-generic-function) arguments)
  (let ((gf (generic-metaobject gf)))
    (cl::%compute-applicable-methods gf arguments (cl::required-classes gf arguments))))
(defgeneric compute-applicable-methods-using-classes (generic-function classes))
(defmethod compute-applicable-methods-using-classes ((gf standard-generic-function) classes)
  (let ((gf (generic-metaobject gf)) (applicable nil))
    (dolist (method (generic-function-methods gf))
      (let ((possible t) (uncertain nil))
        (loop for specializer in (method-specializers method)
              for class in classes do
                (if (typep specializer 'cl::eql-specializer)
                    (if (eq class (class-of (cl::eql-specializer-object specializer)))
                        (setf uncertain t) (setf possible nil))
                    (unless (cl::subclassp class specializer) (setf possible nil))))
        (when (and possible uncertain)
          (return-from compute-applicable-methods-using-classes (values nil nil)))
        (when possible (push method applicable))))
    (values (stable-sort applicable (lambda (a b) (cl::std-method-more-specific-p gf a b classes))) t)))
(defun cl::%mop-applicable-methods (gf args classes)
  (multiple-value-bind (methods definitive) (compute-applicable-methods-using-classes gf classes)
    (if definitive methods (compute-applicable-methods gf args))))

(defpackage :closer-common-lisp (:use :cl) (:nicknames :c2cl))
(do-external-symbols (symbol :cl) (export (list symbol) :closer-common-lisp))
(defpackage :closer-common-lisp-user (:use :closer-common-lisp) (:nicknames :c2cl-user))
(setf cl::*mop-enabled* t)
