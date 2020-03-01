(in-package :common-lisp)

(defmacro defpackage (package &body options)
  (let ((export (cdr (assoc :export options)))
        (use (cdr (assoc :use options)))
        (nicknames (cdr (assoc :nicknames options))))
    `(*:%defpackage ,package :export ,export :use ,use :nicknames ,nicknames)))
