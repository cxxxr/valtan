(in-package :cl-test)

(deftest push-evaluation-order
  (let ((x (vector nil nil nil nil)) (y (vector 'a 'b 'c 'd)) (i 1))
    (push (aref y (incf i)) (aref x (incf i)))
    (values x i))
  #(nil nil nil (c)) 3)

(deftest pushnew-evaluation-order
  (let ((x (vector nil nil nil nil)) (y (vector 'a 'b 'c 'd)) (i 1))
    (pushnew (aref y (incf i)) (aref x (incf i)))
    (values x i))
  #(nil nil nil (c)) 3)

(deftest local-place-macro
  (macrolet ((place (x) x))
    (let ((x nil)) (push 1 (place x)) (pushnew 2 (place x))
      (values (pop (place x)) x)))
  2 (1))

(deftest local-setf-macro
  (macrolet ((place (x) x))
    (let ((x 1)) (setf (place x) 3) (incf (place x) 2) x))
  5)

(deftest symbol-macro-place
  (let ((x (list 1)))
    (symbol-macrolet ((place (car x)))
      (incf place)
      (values place x)))
  2 (2))

(deftest macro-function-protocol
  (values (eval (funcall (macro-function 'when) '(when t 3) nil))
          (handler-case (funcall (macro-function 'push) '(push 1 x))
            (program-error () :rejected)))
  3 :rejected)

(deftest remf-reads-place-after-evaluating-indicator
  (let ((plist (list :a 1)))
    (values (remf plist (progn (setq plist (list :b 2)) :a)) plist))
  nil (:b 2))
