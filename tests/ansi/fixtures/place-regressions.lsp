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

(deftest shiftf-preserves-multiple-values
  (let ((a 1) (b 2) (c 3) (d 4))
    (values (multiple-value-list (shiftf (values a b) (values c d) (values 9 10)))
            a b c d))
  (1 2) 3 4 9 10)

(deftest psetf-stores-all-values-after-evaluation
  (let ((a 1) (b 2) (c 3) (d 4))
    (psetf (values a b) (values c d) (values c d) (values a b))
    (values a b c d))
  3 4 1 2)

(deftest setf-apply-retains-index-argument-list
  (let ((array (make-array '(2 2) :initial-element 0)) (index 0))
    (setf (apply #'aref array (list (incf index) 0)) 7)
    (values (aref array 1 0) index))
  7 1)

(deftest psetq-captures-symbol-macro-places-before-storing
  (let ((array (vector 0 1 2 3 4)) (i 0))
    (symbol-macrolet ((x (aref array (incf i))))
      (psetq x (incf i) x (incf i)))
    (values array i))
  #(0 2 2 4 4) 4)

(deftest setf-the-keeps-type-as-syntax
  (let ((cell (list 1)) (x :old))
    (values (setf (the integer (car cell)) 7)
            (multiple-value-list (setf (the (values symbol integer) (values x (car cell)))
                                      (values :new 9)))
            x cell))
  7 (:new 9) :new (9))

(defvar *place-update-called* nil)
(defmacro place-with-setf-override (cell) `(car ,cell))
(defun update-place-with-setf-override (cell value)
  (setq *place-update-called* t)
  (setf (car cell) value))
(defsetf place-with-setf-override update-place-with-setf-override)
(deftest explicit-setf-definition-precedes-global-macro
  (let ((cell (list 1)) (*place-update-called* nil))
    (values (setf (place-with-setf-override cell) 4) cell *place-update-called*))
  4 (4) t)

(define-modify-macro append-items (&rest lists) append)
(deftest modify-macro-accepts-rest-arguments
  (let ((items (list 1)))
    (values (append-items items '(2 3) '(4)) items))
  (1 2 3 4) (1 2 3 4))

(deftest multiple-value-setq-evaluates-symbol-macro-places-first
  (let ((cell (list nil)) (count 0))
    (symbol-macrolet ((place (car (progn (incf count) cell))))
      (values (multiple-value-setq (place) count) cell count)))
  1 (1) 1)
