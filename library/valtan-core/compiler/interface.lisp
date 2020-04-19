(in-package :compiler)

(defvar *compiler-interfaces* '())

(defmacro def-interface (name args &body body)
  `(progn
     (pushnew ',name *compiler-interfaces*)
     (setf (get ',name 'implementation)
           (lambda ,args ,@body))
     (defun ,name ,args
       (funcall (get ',name 'implementation)
                ,@args))))

(defmacro def-implementation (name args &body body)
  `(progn
     (assert (member ',name *compiler-interfaces*))
     (setf (get ',name 'implementation)
           (flet ((,name ,args ,@body))
             #',name))))
