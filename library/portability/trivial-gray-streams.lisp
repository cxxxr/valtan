;;; The Valtan protocol uses the portable sequence/position signatures.
(defpackage :trivial-gray-streams (:use :cl))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (symbol :valtan-gray)
    (import symbol :trivial-gray-streams)
    (export symbol :trivial-gray-streams)))
