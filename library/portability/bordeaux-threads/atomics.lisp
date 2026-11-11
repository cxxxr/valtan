(in-package :bordeaux-threads-2)

;; Valtan uses a 64-bit unsigned atomic word on every host, including browsers.
;; All operations execute in one native call without a scheduling checkpoint.
(deftype %atomic-integer-value () '(unsigned-byte 64))
(defstruct (atomic-integer (:constructor %make-atomic-integer (native))) native)

;; Keep the successful path out of the general TYPEP interpreter: repeatedly
;; expanding and dispatching these fixed types is costly in atomic update loops.
;; CHECK-TYPE still supplies the original condition and STORE-VALUE restart for
;; invalid arguments; the structure predicate also accepts included structures.
(defmacro %check-atomic-integer (place)
  `(unless (atomic-integer-p ,place) (check-type ,place atomic-integer)))
(defmacro %check-atomic-value (place)
  `(unless (and (integerp ,place) (<= 0 ,place) (< ,place 18446744073709551616))
     (check-type ,place %atomic-integer-value)))

(defun make-atomic-integer (&key (value 0))
  (%check-atomic-value value)
  (%make-atomic-integer (system::%cooperative-atomic-op :make value)))
(defun atomic-integer-value (integer)
  (%check-atomic-integer integer)
  (system::%cooperative-atomic-op :value (atomic-integer-native integer)))
(defun (setf atomic-integer-value) (value integer)
  (%check-atomic-integer integer)
  (%check-atomic-value value)
  (system::%cooperative-atomic-op :set (atomic-integer-native integer) value))
(defun atomic-integer-incf (integer &optional (delta 1))
  (%check-atomic-integer integer)
  (%check-atomic-value delta)
  (system::%cooperative-atomic-op :inc (atomic-integer-native integer) delta))
(defun atomic-integer-decf (integer &optional (delta 1))
  (%check-atomic-integer integer)
  (%check-atomic-value delta)
  (system::%cooperative-atomic-op :dec (atomic-integer-native integer) delta))
(defun atomic-integer-compare-and-swap (integer old new)
  (%check-atomic-integer integer)
  (%check-atomic-value old)
  (%check-atomic-value new)
  (system::%cooperative-atomic-op :cas (atomic-integer-native integer) old new))
