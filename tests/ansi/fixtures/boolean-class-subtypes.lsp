(in-package :cl-test)
(defclass boolean-type-root () ())
(defclass boolean-type-left (boolean-type-root) ())
(defclass boolean-type-right (boolean-type-root) ())
(defclass boolean-type-both (boolean-type-left boolean-type-right) ())

(deftest class-subtype-of-conjunction
  (values (multiple-value-list (subtypep 'boolean-type-left '(and boolean-type-root boolean-type-left)))
          (multiple-value-list (subtypep 'boolean-type-left '(and boolean-type-left boolean-type-left boolean-type-left))))
  (t t) (t t))
(deftest class-conjunction-known-negative
  (subtypep 'boolean-type-left '(and boolean-type-root boolean-type-right))
  nil t)
(deftest class-union-subtype
  (values (multiple-value-list (subtypep '(or boolean-type-left boolean-type-right) 'boolean-type-root))
          (multiple-value-list (subtypep '(or boolean-type-left boolean-type-root) 'boolean-type-left)))
  (t t) (nil t))
(deftest class-intersection-subtype
  (subtypep '(and boolean-type-left boolean-type-right) 'boolean-type-root)
  t t)
(deftest class-subtype-of-union
  (subtypep 'boolean-type-both '(or boolean-type-left boolean-type-right))
  t t)
(deftest boolean-class-object-specifiers
  (subtypep (find-class 'boolean-type-both)
            `(and ,(find-class 'boolean-type-left) ,(find-class 'boolean-type-right)))
  t t)
(deftest boolean-types-empty-identities
  (values (multiple-value-list (subtypep '(or) 'boolean-type-root))
          (multiple-value-list (subtypep 'boolean-type-left '(and))))
  (t t) (t t))
(deftest boolean-types-preserve-uncertainty
  (values
   (multiple-value-list (subtypep 'boolean-type-left '(and boolean-type-root (satisfies oddp))))
   (multiple-value-list (subtypep '(and boolean-type-left (satisfies oddp)) 'boolean-type-root))
   (multiple-value-list (subtypep 'boolean-type-left '(or (satisfies oddp) boolean-type-root))))
  (nil nil) (t t) (t t))
(deftest union-not-contained-in-an-individual-arm
  ;; The union contains ROOT although neither subclass arm contains ROOT alone.
  ;; A lack of a successful individual arm must not become a known rejection.
  (let ((union '(or (and boolean-type-root boolean-type-left)
                   (and boolean-type-root (not boolean-type-left)))))
    (multiple-value-bind (subset known) (subtypep 'boolean-type-root union)
      (or subset (not known))))
  t)

(deftest empty-real-complex-intersection
  ;; Both orderings denote NIL. The real/complex disjointness shortcut must
  ;; not reject a compound source before checking its intersection members.
  (values (multiple-value-list (subtypep '(and integer complex) '(and complex integer)))
          (multiple-value-list (subtypep '(and complex integer) '(and integer complex))))
  (t t) (t t))

(deftest compound-array-element-types
  (values
   (multiple-value-list (subtypep '(array (unsigned-byte 8) (2 3))
                                '(array (unsigned-byte 8) (* *))))
   (multiple-value-list (subtypep '(array (unsigned-byte 8) (* 10 * 29 *))
                                '(and (array (unsigned-byte 8) (* 10 * * *))
                                      (array (unsigned-byte 8) (* * * 29 *))))))
  (t t) (t t))

(deftest empty-intersection-is-not-known-noncontained
  ;; No individual component proves containment in COMPLEX. The whole type
  ;; is nevertheless empty, so the conservative answer must not reject it.
  (multiple-value-bind (subset known)
      (subtypep '(and integer symbol) 'complex)
    (or subset (not known)))
  t)
