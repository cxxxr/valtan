#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defvar *catch-handlers* nil)

(defmacro catch (tag &body body)
  (let ((g-tag (gensym "TAG")) (exit (gensym "CATCH")) (result (gensym "VALUES")))
    `(let ((,g-tag ,tag))
       (block ,exit
         (let ((*catch-handlers*
                 (acons ,g-tag (lambda (,result) (return-from ,exit (values-list ,result)))
                        *catch-handlers*)))
           ,@body)))))

(defun system:throw-values (tag result)
  (let ((handler (assoc tag *catch-handlers* :test #'eq)))
    (if handler
        (funcall (cdr handler) result)
        (error 'control-error))))

(defun throw (tag result)
  (system:throw-values tag (list result)))
