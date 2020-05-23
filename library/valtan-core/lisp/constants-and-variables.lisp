#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(#-valtan defparameter #+valtan defconstant
 most-positive-fixnum #.(cl:expt 2 32))

(#-valtan defparameter #+valtan defconstant
 lambda-list-keywords
 '(&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY &OPTIONAL &REST &WHOLE))

(defvar *features* '(:valtan))
