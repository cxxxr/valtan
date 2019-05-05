(in-package :common-lisp)

(defmacro defstruct (name-and-options &rest slot-descriptions)
  (let ((name (if (consp name-and-options)
                  (first name-and-options)
                  name-and-options)))
    (check-type name symbol)
    (let ((constructor-name (intern (format nil "MAKE-~A" name))))
      `(progn
         (defun ,constructor-name (,@keys)
           )))))

