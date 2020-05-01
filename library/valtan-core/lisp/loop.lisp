#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defmacro loop (&body body)
  (let ((tag (gensym)))
    `(prog ()
       ,tag
       ,@body
       (go ,tag))))
