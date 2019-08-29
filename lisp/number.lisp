(in-package :common-lisp)

(defun plusp (x)
  (< 0 x))

(defun minusp (x)
  (< x 0))

(defun 1+ (x)
  (+ x 1))

(defun 1- (x)
  (- x 1))

(defun zerop (x)
  (= x 0))

(defun evenp (x)
  (= 0 (rem x 2)))

(defun oddp (x)
  (= 1 (rem x 2)))

(defun min (number &rest more-numbers)
  (dolist (n more-numbers number)
    (when (< number n)
      (setq number n))))

(defun max (number &rest more-numbers)
  (dolist (n more-numbers number)
    (when (> number n)
      (setq number n))))
