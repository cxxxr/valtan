(in-package :cl-test)
(defstruct regression-structure
  "A structure with runtime class metadata."
  (a 3) (b nil))
(defgeneric regression-structure-dispatch (object))
(defmethod regression-structure-dispatch ((object regression-structure))
  (regression-structure-a object))
(deftest structure-class-identity
  (let ((object (make-regression-structure)) (class (find-class 'regression-structure)))
    (values (eq (class-of object) class) (typep class 'structure-class)
            (typep class 'class) (class-name class) (typep object class)))
  t t t regression-structure t)
(deftest structure-class-subtyping
  (values (multiple-value-list (subtypep 'regression-structure 'structure-object))
          (multiple-value-list (subtypep 'regression-structure 'cons))
          (multiple-value-list (subtypep (find-class 'regression-structure)
                                        (find-class 'structure-object))))
  (t t) (nil t) (t t))
(deftest structure-slot-access
  (let ((object (make-regression-structure)))
    (values (slot-exists-p object 'a) (slot-exists-p object 'absent)
            (slot-boundp object 'b) (slot-value object 'b)
            (setf (slot-value object 'a) 9) (regression-structure-a object)))
  t nil t nil 9 9)
(deftest structure-method-dispatch
  (let* ((object (make-regression-structure :a 17)) (copy (copy-regression-structure object)))
    (values (regression-structure-dispatch copy) (eq object copy)
            (eq (class-of object) (class-of copy))))
  17 nil t)

(defstruct (regression-default-constructor :constructor) value)
(defstruct (regression-no-constructor (:constructor nil)) value)
(defstruct (regression-no-prefix (:conc-name nil)) regression-plain-slot)
(deftest structure-constructor-options
  (let ((object (make-regression-default-constructor :value 8)))
    (values (regression-default-constructor-value object)
            (regression-default-constructor-p object)
            (fboundp 'make-regression-no-constructor)
            (regression-plain-slot (make-regression-no-prefix :regression-plain-slot 9))))
  8 t nil 9)
