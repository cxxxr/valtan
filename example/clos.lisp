(in-package :common-lisp)

(print
 (defclass foo ()
   ((x
     :initarg :x
     :accessor foo-x)
    (y)
    z)
   (:default-initargs :x 10)))

(print (defgeneric hoge (x y)))
(print (defmethod hoge ((x integer) (y integer))
         (+ x y)))

(print (defmethod hoge ((x foo) (y integer))
         (write-line "ok")
         (list x y)))

;; (print (class-of (make-instance 'foo)))
;; (print (class-precedence-list (class-of (make-instance 'foo))))
;; (terpri)
;; (hoge (make-instance 'foo) 100)

(print (make-instance 'foo))

(let ((e (make-instance 'simple-error
                        :format-control "test"
                        :format-arguments '())))
  (print e)
  (terpri)
  (princ e))
