(cl:defpackage "CLIM-TESTS"
  (:use "CLIM-LISP" "CLIM" "CLIME" "FIVEAM")
  (:shadowing-import-from "FIVEAM" "TEST")
  (:import-from "CLIMI" "DSET" "DREF" "DLET"
    "MAKE-DYNAMIC-VARIABLE" "MAKE-THREAD-LOCAL-VARIABLE"
    "DYNAMIC-VARIABLE-BOUND-P" "DYNAMIC-VARIABLE-MAKUNBOUND"
    "CLASS-WITH-DYNAMIC-SLOTS" "SLOT-DLET"))
(in-package :clim-tests)
(def-suite :mcclim)
