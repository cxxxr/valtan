(in-package :cl-test)

(defclass change-regression-source ()
  ((common :initarg :common :accessor change-common)
   (unbound :initarg :unbound)
   (removed :initarg :removed)
   (shared :allocation :class :initarg :shared :initform 40)))
(defclass change-regression-target ()
  ((common :initarg :common :initform 100 :accessor change-common)
   (unbound :initarg :unbound :initform 200)
   (added :initarg :added :initform 9)
   (shared :initarg :shared)))
(defclass change-regression-shared ()
  ((common :allocation :class :initarg :common :initform 70)))

(deftest with-slots-evaluates-receiver-once-and-assigns
  (let ((object (make-instance 'change-regression-source :common 2)) (calls 0))
    (values (with-slots ((value common)) (progn (incf calls) object)
              (setq value (+ value 3))
              (incf value 4))
            calls (slot-value object 'common)
            (multiple-value-list (with-slots () nil (values)))))
  9 1 9 nil)

(deftest with-accessors-uses-accessor-and-lexical-scope
  (let ((object (make-instance 'change-regression-source :common 2)) (calls 0))
    (values (with-accessors ((value change-common)) (progn (incf calls) object)
              (incf value 3)
              (let ((value :shadow)) value))
            calls (change-common object)
            (with-slots ((value common)) object
              (with-slots ((value common)) (make-instance 'change-regression-source :common 12)
                value))))
  :shadow 1 5 12)

(deftest change-class-preserves-identity-values-and-unbound-slots
  (let* ((value (list :value))
         (object (make-instance 'change-regression-source :common value :removed :old :shared 40)))
    (values (eq object (change-class object 'change-regression-target))
            (class-name (class-of object))
            (eq value (slot-value object 'common))
            (slot-boundp object 'unbound)
            (slot-exists-p object 'removed)
            (slot-value object 'added)
            (slot-value object 'shared)))
  t change-regression-target t nil nil 9 40)

(deftest change-class-shared-and-local-storage
  (let ((object (make-instance 'change-regression-source :common 3 :shared 40))
        (other (make-instance 'change-regression-source)))
    (setf (slot-value (make-instance 'change-regression-shared) 'common) 70)
    (change-class object 'change-regression-target)
    (setf (slot-value object 'shared) 99)
    (values (slot-value other 'shared)
            (progn (change-class object 'change-regression-shared)
                   (slot-value object 'common))
            (progn (change-class object 'change-regression-shared :common 71)
                   (slot-value (make-instance 'change-regression-shared) 'common))))
  40 70 71)

(defclass change-regression-callback-source (change-regression-source) ())
(defclass change-regression-callback-target (change-regression-target) ())
(defvar *change-regression-observation* nil)
(defmethod update-instance-for-different-class :before
  ((previous change-regression-callback-source) (current change-regression-callback-target)
   &key (factor 1))
  (setq *change-regression-observation*
        (list (not (eq previous current)) (class-name (class-of previous))
              (class-name (class-of current)) (slot-value previous 'removed)))
  (setf (slot-value current 'added) (* factor (slot-value previous 'common))))
(deftest change-class-callback-receives-copy-and-validates-method-keywords
  (let ((object (make-instance 'change-regression-callback-source :common 4 :removed :old)))
    (values (eq object (change-class object 'change-regression-callback-target :factor 3))
            *change-regression-observation*
            (slot-value object 'added)))
  t (t change-regression-callback-source change-regression-callback-target :old) 12)

(deftest change-class-initarg-validation-and-first-key-wins
  (values
    (handler-case (change-class (make-instance 'change-regression-source)
                                'change-regression-target :unknown t)
      (program-error () :rejected))
    (slot-value (change-class (make-instance 'change-regression-source)
                              'change-regression-target :common 1 :common 2
                              :unknown t :allow-other-keys t) 'common)
    (handler-case (change-class (make-instance 'change-regression-source)
                                'change-regression-target :unknown t
                                :allow-other-keys nil :allow-other-keys t)
      (program-error () :rejected)))
  :rejected 1 :rejected)

(defclass change-regression-named () ((cl::name :initform :real-slot)))
(deftest change-class-new-name-slot-does-not-inherit-display-name
  (let ((object (make-instance 'change-regression-source)))
    (change-class object 'change-regression-named)
    (slot-value object 'cl::name))
  :real-slot)

(defmethod update-instance-for-different-class
  ((previous change-regression-named) (current change-regression-target) &key)
  (declare (ignore previous current))
  (values :ignored :also-ignored))
(deftest change-class-returns-one-value-despite-custom-update-result
  (let* ((object (make-instance 'change-regression-named))
         (results (multiple-value-list (change-class object 'change-regression-target))))
    (values (eq object (first results)) (length results)))
  t 1)
