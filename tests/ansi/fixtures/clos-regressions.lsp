(in-package :cl-test)

(deftest standard-logical-pathname-class-is-present
  (values (typep (find-class 'logical-pathname) 'built-in-class)
          (typep (find-class 'method-combination) 'class)
          (multiple-value-list (subtypep 'logical-pathname 'pathname)))
  t t (t t))

(deftest standard-condition-subclasses-and-readers
  (let ((condition (make-condition 'simple-type-error :datum 99 :expected-type 'string
                                                    :format-control "Bad input" :format-arguments nil)))
    (values (typep condition 'type-error) (typep condition 'simple-condition)
            (type-error-datum condition) (type-error-expected-type condition)
            (print-not-readable-object (make-condition 'print-not-readable :object :opaque))))
  t t 99 string :opaque)

(defgeneric regression-stream-kind (stream))
(defmethod regression-stream-kind ((stream stream)) :stream)
(defmethod regression-stream-kind ((stream string-stream)) :string)
(deftest standard-stream-class-dispatch
  (let ((input (make-string-input-stream "abc")) (output (make-string-output-stream)))
    (values (regression-stream-kind input) (regression-stream-kind output)
            (class-name (class-of (make-two-way-stream input output)))
            (class-name (class-of (make-broadcast-stream output)))
            (multiple-value-list (subtypep 'file-stream 'stream))))
  :string :string two-way-stream broadcast-stream (t t))

(defclass regression-parent ()
  ((value :initform 8 :initarg :value :type integer :documentation "Value slot")
   (other :initform 7)))
(defclass regression-child (regression-parent) ((value :initform nil)))

(deftest clos-slot-metadata
  (let ((x (make-instance 'regression-parent)))
    (values (slot-value x 'value) (slot-exists-p x 'value)
            (slot-exists-p x 'absent) (slot-boundp x 'value)))
  8 t nil t)

(deftest clos-initform-specificity
  (let ((x (make-instance 'regression-child)))
    (values (slot-value x 'value) (slot-value x 'other) (slot-boundp x 'value)))
  nil 7 t)

(defvar *regression-default-count* 0)
(defclass regression-defaults () ((value :initarg :value))
  (:default-initargs :value (incf *regression-default-count*)))
(deftest clos-default-initargs-lazy
  (let* ((before *regression-default-count*)
         (x (make-instance 'regression-defaults))
         (y (make-instance 'regression-defaults :value nil)))
    (values before (slot-value x 'value) (slot-value y 'value)
            *regression-default-count*))
  0 1 nil 1)

(defun regression-ordinary-function (x) (+ x 1))
(deftest clos-ensure-preserves-ordinary-functions
  (values (handler-case (ensure-generic-function 'regression-ordinary-function)
            (error () :rejected))
          (regression-ordinary-function 3)
          (handler-case (ensure-generic-function 'car) (error () :rejected))
          (car '(a b))
          (handler-case (ensure-generic-function 'defclass) (error () :rejected))
          (handler-case (ensure-generic-function 'tagbody) (error () :rejected)))
  :rejected 4 :rejected a :rejected :rejected)

(deftest clos-ensure-returns-function
  (let ((f (ensure-generic-function 'regression-empty-generic :lambda-list '(x))))
    (values (functionp f) (typep f 'generic-function)
            (eq f (ensure-generic-function 'regression-empty-generic))
            (eq f (fdefinition 'regression-empty-generic))))
  t t t t)

(defgeneric regression-method-lifecycle (object))
(defmethod regression-method-lifecycle ((object t)) (declare (ignore object)) 0)
(deftest clos-generic-function-identity-through-method-changes
  (let* ((f #'regression-method-lifecycle)
         (method (eval '(defmethod regression-method-lifecycle ((object integer)) (+ object 10)))))
    (values (eq f #'regression-method-lifecycle) (funcall f 3)
            (eq f (remove-method f method)) (funcall f 3)
            (eq f (add-method f method)) (funcall f 3)
            (length (compute-applicable-methods f '(3)))))
  t 13 t 0 t 13 2)

(deftest clos-generic-function-arity
  (values (handler-case (regression-method-lifecycle) (program-error () :rejected))
          (handler-case (regression-method-lifecycle 1 2) (program-error () :rejected))
          (handler-case (regression-method-lifecycle)
            (program-error (condition) (stringp (princ-to-string condition)))))
  :rejected :rejected t)

(defvar *shared-initialization-count* 0)
(defclass regression-shared ()
  ((shared :allocation :class :initform (incf *shared-initialization-count*) :initarg :shared)
   (local :initform 4 :initarg :local)))
(defclass regression-shared-child (regression-shared) ())
(defclass regression-shared-override (regression-shared)
  ((shared :allocation :class :initform 99)))
(deftest clos-shared-slot-lifetime
  (let* ((at-definition *shared-initialization-count*)
         (x (make-instance 'regression-shared))
         (y (make-instance 'regression-shared-child :shared 8))
         (z (make-instance 'regression-shared-override)))
    (setf (slot-value x 'local) 7)
    (values at-definition *shared-initialization-count*
            (slot-value x 'shared) (slot-value y 'shared) (slot-value z 'shared)
            (slot-value x 'local) (slot-value y 'local)))
  1 1 8 8 99 7 4)

(deftest clos-shared-slot-makunbound
  (let ((x (make-instance 'regression-shared))
        (y (make-instance 'regression-shared-child)))
    (values (eq x (slot-makunbound x 'shared))
            (slot-boundp y 'shared)
            (progn (setf (slot-value y 'shared) nil) (slot-boundp x 'shared))
            (slot-value x 'shared)))
  t nil t nil)

(deftest clos-unbound-slot-condition
  (let ((x (make-instance 'regression-parent)))
    (slot-makunbound x 'value)
    (values (slot-boundp x 'value)
            (handler-case (slot-value x 'value)
              (unbound-slot (condition)
                (list (eq (unbound-slot-instance condition) x)
                      (cell-error-name condition))))))
  nil (t value))

(deftest clos-reinitialize-preserves-unbound-and-other-values
  (let ((x (make-instance 'regression-parent)))
    (slot-makunbound x 'other)
    (values (eq x (reinitialize-instance x :value nil :value 99))
            (slot-value x 'value) (slot-boundp x 'other)))
  t nil nil)

(defmethod reinitialize-instance :after ((x regression-parent) &key override)
  (when override (setf (slot-value x 'value) override)))
(deftest clos-reinitialize-method-keywords
  (let ((x (make-instance 'regression-parent)))
    (reinitialize-instance x :override 10)
    (values (slot-value x 'value)
            (handler-case (reinitialize-instance x :invalid 2) (program-error () :rejected))
            (eq x (reinitialize-instance x :invalid 2 :allow-other-keys t))
            (handler-case (reinitialize-instance x :invalid 2 :allow-other-keys nil
                                                :allow-other-keys t)
              (program-error () :rejected))))
  10 :rejected t :rejected)

(defclass regression-missing-slot () ())
(defmethod slot-missing ((class t) (object regression-missing-slot) slot operation &optional value)
  (declare (ignore class object slot operation value))
  (values :replacement :extra))
(deftest clos-missing-slot-protocol
  (let ((x (make-instance 'regression-missing-slot)))
    (values (multiple-value-list (slot-value x 'absent))
            (multiple-value-list (slot-boundp x 'absent))
            (multiple-value-list (setf (slot-value x 'absent) 9))
            (eq x (slot-makunbound x 'absent))))
  (:replacement) (t) (9) t)

(deftest clos-builtin-metaclasses
  (every (lambda (object) (eq (class-of (class-of object)) (find-class 'built-in-class)))
         (list nil 'symbol 1 1.0 1/2 #C(1 2) #\a "abc" #(1 2) #*01
               (make-hash-table) (find-package :cl) #'car))
  t)
(deftest clos-generic-function-and-method-superclasses
  (values (multiple-value-list (subtypep 'generic-function 'function))
          (multiple-value-list (subtypep 'standard-generic-function 'standard-object))
          (multiple-value-list (subtypep 'standard-method 'method))
          (typep #'regression-method-lifecycle 'standard-object)
          (typep (find-class 'integer) 'class))
  (t t) (t t) (t t) t t)
(deftest clos-specialized-array-classes
  (values (class-name (class-of "abc")) (class-name (class-of #*01))
          (class-name (class-of #(1 2)))
          (typep "abc" (find-class 'string))
          (typep #*01 (find-class 'bit-vector)))
  string bit-vector vector t t)

(define-method-combination regression-product :operator * :identity-with-one-argument t
  :documentation "Multiply method results")
(defgeneric regression-product-gf (x) (:method-combination regression-product))
(defmethod regression-product-gf regression-product ((x t)) 5)
(defmethod regression-product-gf regression-product ((x rational)) 3)
(defmethod regression-product-gf regression-product ((x integer)) 2)
(defmethod regression-product-gf :around ((x integer)) (1+ (call-next-method)))
(deftest clos-short-combination-and-around
  (values (regression-product-gf 1) (regression-product-gf 1/2) (regression-product-gf 1.0)
          (documentation 'regression-product 'method-combination))
  31 15 5 "Multiply method results")

(define-method-combination regression-ordered :operator list)
(defgeneric regression-ordered-gf (x) (:method-combination regression-ordered :most-specific-last))
(defmethod regression-ordered-gf regression-ordered ((x t)) :general)
(defmethod regression-ordered-gf regression-ordered ((x number)) :number)
(defmethod regression-ordered-gf regression-ordered ((x integer)) :integer)
(deftest clos-short-combination-order
  (regression-ordered-gf 1)
  (:general :number :integer))

(defvar *combination-effects* nil)
(defgeneric regression-or-gf (x) (:method-combination or))
(defmethod regression-or-gf or ((x t)) (push :general *combination-effects*) :fallback)
(defmethod regression-or-gf or ((x integer)) (push :integer *combination-effects*) :found)
(deftest clos-builtin-combination-short-circuit
  (let ((*combination-effects* nil))
    (values (regression-or-gf 1) *combination-effects*))
  :found (:integer))

(defgeneric regression-one-result-gf (x) (:method-combination +))
(defmethod regression-one-result-gf + ((x t)) (values x :second))
(deftest clos-combination-identity-preserves-values
  (regression-one-result-gf 9)
  9 :second)

(let ((fallback :captured))
  (define-method-combination regression-arguments () ((methods *))
    (:arguments &whole all x extra &optional (y :default supplied) &rest rest &key (z y zp)
                &aux (auxiliary (list x y z)))
    (:generic-function gf)
    "Inspect generic function arguments"
    `(list ,all ,x ,extra ,y ,supplied ,rest ,z ,zp
           ',gf ,@(mapcar (lambda (method) `(call-method ,method)) methods) ,auxiliary ',fallback)))
(defgeneric regression-arguments-empty () (:method-combination regression-arguments))
(defmethod regression-arguments-empty () :empty)
(defgeneric regression-arguments-full (a &optional b &rest rest)
  (:method-combination regression-arguments))
(defmethod regression-arguments-full (a &optional b &rest rest)
  (declare (ignore a b rest)) :full)
(deftest clos-combination-missing-argument-sections
  (let ((result (regression-arguments-empty)))
    (values (subseq result 0 8) (eq (nth 8 result) #'regression-arguments-empty)
            (nth 9 result)))
  (nil nil nil :default nil nil :default nil) t :empty)
(deftest clos-combination-argument-sections-and-supplied-p
  (let ((result (regression-arguments-full 1 2 :z nil :unknown 9)))
    (values (subseq result 0 8) (eq (nth 8 result) #'regression-arguments-full)
            (nth 9 result)))
  ((1 2 :z nil :unknown 9) 1 nil 2 t (:z nil :unknown 9) nil t) t :full)
(deftest clos-combination-defaults-and-definition-environment
  (values (subseq (regression-arguments-full 1) 0 8)
          (nth 10 (regression-arguments-full 1))
          (nth 11 (regression-arguments-full 1))
          (documentation 'regression-arguments 'method-combination))
  ((1) 1 nil :default nil nil :default nil) (1 :default :default) :captured "Inspect generic function arguments")

(define-method-combination regression-long-order (order)
    ((methods * :order order))
  `(list ,@(mapcar (lambda (method) `(call-method ,method)) methods)))
(defgeneric regression-long-order-gf (x)
  (:method-combination regression-long-order :most-specific-last))
(defmethod regression-long-order-gf ((x t)) :general)
(defmethod regression-long-order-gf ((x integer)) :integer)
(deftest clos-long-combination-evaluates-order
  (regression-long-order-gf 1)
  (:general :integer))

(defgeneric regression-eql-cache (x y))
(defmethod regression-eql-cache ((x t) (y t)) :general)
(defmethod regression-eql-cache ((x integer) (y integer)) :integer)
(defmethod regression-eql-cache ((x (eql 1)) (y t)) :first)
(defmethod regression-eql-cache ((x integer) (y (eql 1))) :second)
(deftest clos-eql-specializer-cache-and-precedence
  (values (regression-eql-cache 1 2) (regression-eql-cache 2 1)
          (regression-eql-cache 2 2) (regression-eql-cache 1 1)
          (regression-eql-cache 1 2))
  :first :second :integer :first :first)

(defgeneric regression-method-aux (x &optional y &key z))
(defmethod regression-method-aux ((x integer) &optional (y (1+ x)) &key (z (1+ y))
                                 &aux (result (+ x y z)))
  result)
(deftest clos-method-lambda-list-order-and-auxiliary
  (values (regression-method-aux 1) (regression-method-aux 1 10 :z 100))
  6 111)

(define-method-combination regression-unqualified () ((primary ()) (ignored *))
  `(list ,@(mapcar (lambda (method) `(call-method ,method)) primary)))
(defgeneric regression-unqualified-gf (x) (:method-combination regression-unqualified))
(defmethod regression-unqualified-gf ((x integer)) :integer)
(defmethod regression-unqualified-gf ignored ((x t)) :ignored)
(deftest clos-long-combination-empty-qualifier-pattern
  (values (regression-unqualified-gf 1) (regression-unqualified-gf "text"))
  (:integer) nil)

(defgeneric regression-no-methods-gf (x) (:method-combination regression-unqualified))
(deftest clos-long-combination-without-applicable-methods-signals
  (handler-case (regression-no-methods-gf 1) (error () :caught))
  :caught)
