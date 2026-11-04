(in-package :cl-test)
(deftest eval-preserves-cons-identity
  (let ((x (list 1 2))) (eq x (eval (list 'quote x))))
  t)
(deftest eval-preserves-uninterned-symbol
  (let ((x (gensym)))
    (values (eq x (eval (list 'quote x)))
            (handler-case (eval x)
              (unbound-variable (c) (eq x (cell-error-name c))))))
  t t)
(deftest compile-captures-literal-objects
  (let* ((x (vector 1 2))
         (f (compile nil (list 'lambda nil (list 'quote x)))))
    (setf (aref x 0) 9)
    (values (eq x (funcall f)) (eq (funcall f) (funcall f)) (aref (funcall f) 0)))
  t t 9)
(deftest eval-function-and-class-literals
  (let ((f #'car) (c (find-class 'error)))
    (values (eq f (eval (list 'quote f))) (eq c (eval (list 'quote c)))
            (funcall (eval (list 'quote f)) '(7 8))))
  t t 7)
