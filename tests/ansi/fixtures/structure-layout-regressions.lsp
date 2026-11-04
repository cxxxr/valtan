(in-package :cl-test)

(unless (find-class 'layout-conditional nil)
  (defstruct layout-conditional (value 17)))

(deftest conditional-structure-registers-with-constructors
  (values (layout-conditional-value (make-layout-conditional))
          (layout-conditional-value (read-from-string "#S(LAYOUT-CONDITIONAL :VALUE 23)")))
  17 23)

(defstruct layout-parent (a 1) (b (list :fresh)))
(defstruct (layout-child (:include layout-parent (a 9))) (c 3))
(defgeneric layout-dispatch (x))
(defmethod layout-dispatch ((x layout-parent)) (layout-parent-a x))

(deftest native-structure-copy-is-shallow-but-independent
  (let* ((a (make-layout-child)) (b (copy-layout-parent a)))
    (setf (layout-child-a a) 17)
    (values (layout-parent-a b) (layout-child-c b)
            (eq (layout-parent-b a) (layout-parent-b b))
            (eq (class-of a) (class-of b))))
  9 3 t t)

(deftest structure-inclusion-types-and-methods
  (let ((x (make-layout-child)))
    (values (typep x 'layout-parent) (layout-parent-p x)
            (typep x (find-class 'layout-parent))
            (multiple-value-list (subtypep 'layout-child 'layout-parent))
            (layout-dispatch x)))
  t t t (t t) 9)

(defstruct (layout-list-parent (:type list) :named (:initial-offset 2)) (a 1))
(defstruct (layout-list-child (:type list) :named (:initial-offset 1)
                              (:include layout-list-parent (a 8))) (b 2))
(deftest typed-list-inherited-offsets-and-tags
  (let ((x (make-layout-list-child)))
    (values (append (subseq x 2 4) (subseq x 5)) (layout-list-parent-p x) (layout-list-child-p x)
            (layout-list-parent-a x) (layout-list-child-a x)))
  (layout-list-parent 8 layout-list-child 2) t t 8 8)

(defstruct (layout-vector (:type vector) :named (:initial-offset 1)) (a 3))
(deftest typed-vector-copy-and-setf
  (let* ((x (make-layout-vector)) (y (copy-layout-vector x)))
    (setf (layout-vector-a x) 9)
    (values (coerce (subseq x 1) 'list) (coerce (subseq y 1) 'list) (layout-vector-p y)
            (layout-vector-p #(nil :wrong 3))))
  (layout-vector 9) (layout-vector 3) t nil)

(defstruct (layout-bits (:type (vector bit))) (a 0) (b 1))
(deftest specialized-structure-vector
  (let ((x (make-layout-bits)))
    (setf (layout-bits-a x) 1)
    (values x (array-element-type x) (fboundp 'layout-bits-p)))
  #*11 bit nil)

(defstruct (layout-boa (:constructor) (:constructor layout-boa-defaults (&optional a &key b))
                       (:constructor layout-boa-pair (b a)) (:constructor layout-boa-empty ()))
  (a 10) (b 20) (c 30))
(deftest multiple-boa-constructors-and-slot-defaults
  (mapcar (lambda (x) (list (layout-boa-a x) (layout-boa-b x) (layout-boa-c x)))
          (list (make-layout-boa) (layout-boa-defaults)
                (layout-boa-defaults 7 :b 8) (layout-boa-pair 1 2) (layout-boa-empty)))
  ((10 20 30) (10 20 30) (7 8 30) (2 1 30) (10 20 30)))

(defvar *layout-special* 41)
(defstruct layout-special (before *layout-special*) (*layout-special* 8) (after *layout-special*))
(deftest keyword-constructors-do-not-bind-slot-names
  (let ((x (make-layout-special :*layout-special* 17)))
    (values (layout-special-before x) (layout-special-*layout-special* x)
            (layout-special-after x) *layout-special*))
  41 17 41 41)

(let ((a 15))
  (defstruct (layout-lexical (:constructor layout-lexical-make (a &optional b)))
    a (b a)))
(deftest boa-slot-default-sees-earlier-argument
  (let ((x (layout-lexical-make 99)))
    (values (layout-lexical-a x) (layout-lexical-b x)))
  99 99)

(defstruct (layout-readonly (:include layout-parent (a 7 :read-only t))))
(deftest included-readonly-slot
  (let ((x (make-layout-readonly)))
    (values (layout-readonly-a x) (layout-parent-a x)
            (fboundp '(setf layout-readonly-a))))
  7 7 nil)

(defstruct (layout-printer (:print-object (lambda (object stream)
                                          (write (layout-printer-a object) :stream stream))))
  (a 31))
(deftest structure-print-object-uses-two-arguments
  (write-to-string (make-layout-printer))
  "31")

(deftest unnamed-typed-structure-rejects-predicate
  (handler-case
      (eval '(defstruct (invalid-layout-predicate (:type (vector single-float))
                                                 (:predicate invalid-layout-p))))
    (simple-error () :rejected))
  :rejected)

(deftest typed-structure-rejects-unrepresentable-name
  (handler-case
      (eval '(defstruct (invalid-layout-name (:type (vector single-float)) :named)))
    (simple-error () :rejected))
  :rejected)
