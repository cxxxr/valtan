(in-package :common-lisp)

(print
 (defclass foo ()
   ((x
     :initarg :x
     :accessor foo-x)
    (y)
    z)
   (:default-initargs :x 10)))

;; (print (defgeneric hoge (x y)))
;; (print (defmethod hoge ((x integer) (y integer))
;;          (+ x y)))

;; (print (defmethod hoge ((x foo) (y integer))
;;          (write-line "ok")
;;          (list x y)))

(make-instance 'foo)

;(print (class-of (make-instance 'foo))) ; これが間違ってて
;; (print (class-precedence-list (class-of (make-instance 'foo)))) ; これも間違ってるので
;; (terpri)
;; (print (hoge (make-instance 'foo) 100)) ; これが動かない
