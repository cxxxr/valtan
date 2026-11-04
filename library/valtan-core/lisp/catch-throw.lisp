#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(define-condition throw-condition (base-condition)
  ((tag
    :initarg :tag
    :reader throw-condition-tag)
   (value
    :initarg :value
    :reader throw-condition-value)))

(defmacro catch (tag &body body)
  (let ((g-tag (gensym "TAG")))
    `(let ((,g-tag ,tag))
       (handler-case (progn ,@body)
         (throw-condition (e)
           (if (eq ,g-tag (throw-condition-tag e))
               (values-list (throw-condition-value e))
               (error e)))))))

(defun system:throw-values (tag result)
  (error 'throw-condition :tag tag :value result))

(defun throw (tag result)
  (system:throw-values tag (list result)))
