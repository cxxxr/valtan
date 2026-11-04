#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(#-valtan defparameter #+valtan defconstant
 most-positive-fixnum 9007199254740991)

(#-valtan defparameter #+valtan defconstant
 most-negative-fixnum -9007199254740991)

(#-valtan defparameter #+valtan defconstant array-rank-limit 256)
(#-valtan defparameter #+valtan defconstant array-dimension-limit 4294967296)
(#-valtan defparameter #+valtan defconstant array-total-size-limit 4294967296)
(#-valtan defparameter #+valtan defconstant call-arguments-limit 32768)
(#-valtan defparameter #+valtan defconstant lambda-parameters-limit 32768)
(#-valtan defparameter #+valtan defconstant multiple-values-limit 32768)

(#-valtan defparameter #+valtan defconstant
 lambda-list-keywords
 '(&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY &OPTIONAL &REST &WHOLE))

(defvar *features* '(:valtan))

;; All four permitted float names share the JavaScript binary64 format.
#+valtan
(defconstant most-positive-short-float 1.7976931348623157d308)
#+valtan
(defconstant most-negative-short-float -1.7976931348623157d308)
#+valtan
(defconstant least-positive-short-float 4.9406564584124654d-324)
#+valtan
(defconstant least-negative-short-float -4.9406564584124654d-324)
#+valtan
(defconstant least-positive-normalized-short-float 2.2250738585072014d-308)
#+valtan
(defconstant least-negative-normalized-short-float -2.2250738585072014d-308)
#+valtan
(defconstant short-float-epsilon 2.220446049250313d-16)
#+valtan
(defconstant short-float-negative-epsilon 1.1102230246251565d-16)
#+valtan
(defconstant most-positive-single-float 1.7976931348623157d308)
#+valtan
(defconstant most-negative-single-float -1.7976931348623157d308)
#+valtan
(defconstant least-positive-single-float 4.9406564584124654d-324)
#+valtan
(defconstant least-negative-single-float -4.9406564584124654d-324)
#+valtan
(defconstant least-positive-normalized-single-float 2.2250738585072014d-308)
#+valtan
(defconstant least-negative-normalized-single-float -2.2250738585072014d-308)
#+valtan
(defconstant single-float-epsilon 2.220446049250313d-16)
#+valtan
(defconstant single-float-negative-epsilon 1.1102230246251565d-16)
#+valtan
(defconstant most-positive-double-float 1.7976931348623157d308)
#+valtan
(defconstant most-negative-double-float -1.7976931348623157d308)
#+valtan
(defconstant least-positive-double-float 4.9406564584124654d-324)
#+valtan
(defconstant least-negative-double-float -4.9406564584124654d-324)
#+valtan
(defconstant least-positive-normalized-double-float 2.2250738585072014d-308)
#+valtan
(defconstant least-negative-normalized-double-float -2.2250738585072014d-308)
#+valtan
(defconstant double-float-epsilon 2.220446049250313d-16)
#+valtan
(defconstant double-float-negative-epsilon 1.1102230246251565d-16)
#+valtan
(defconstant most-positive-long-float 1.7976931348623157d308)
#+valtan
(defconstant most-negative-long-float -1.7976931348623157d308)
#+valtan
(defconstant least-positive-long-float 4.9406564584124654d-324)
#+valtan
(defconstant least-negative-long-float -4.9406564584124654d-324)
#+valtan
(defconstant least-positive-normalized-long-float 2.2250738585072014d-308)
#+valtan
(defconstant least-negative-normalized-long-float -2.2250738585072014d-308)
#+valtan
(defconstant long-float-epsilon 2.220446049250313d-16)
#+valtan
(defconstant long-float-negative-epsilon 1.1102230246251565d-16)
#+valtan
(defconstant pi 3.141592653589793d0)
(#-valtan defparameter #+valtan defconstant boole-clr 0)
(#-valtan defparameter #+valtan defconstant boole-and 8)
(#-valtan defparameter #+valtan defconstant boole-andc1 4)
(#-valtan defparameter #+valtan defconstant boole-2 12)
(#-valtan defparameter #+valtan defconstant boole-andc2 2)
(#-valtan defparameter #+valtan defconstant boole-1 10)
(#-valtan defparameter #+valtan defconstant boole-xor 6)
(#-valtan defparameter #+valtan defconstant boole-ior 14)
(#-valtan defparameter #+valtan defconstant boole-nor 1)
(#-valtan defparameter #+valtan defconstant boole-eqv 9)
(#-valtan defparameter #+valtan defconstant boole-c1 5)
(#-valtan defparameter #+valtan defconstant boole-orc1 13)
(#-valtan defparameter #+valtan defconstant boole-c2 3)
(#-valtan defparameter #+valtan defconstant boole-orc2 11)
(#-valtan defparameter #+valtan defconstant boole-nand 7)
(#-valtan defparameter #+valtan defconstant boole-set 15)
