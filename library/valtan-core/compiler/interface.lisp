(in-package :compiler)

(defvar *compiler-interfaces* '())

(defmacro def-interface (name lambda-list &body body)
  `(progn
     (pushnew ',name *compiler-interfaces*)
     (defun ,name ,lambda-list ,@body)))

(defmacro def-implementation (name lambda-list &body body)
  `(progn
     (assert (member ',name *compiler-interfaces*))
     (defun ,name ,lambda-list ,@body)))
