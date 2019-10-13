(in-package :common-lisp)

(macrolet ((def (name op initial-value)
             `(defun ,name (&rest numbers)
                (let ((acc ,initial-value))
                  (dolist (n numbers)
                    (unless (numberp n)
                      (error 'type-error :datum n :expected-type 'number))
                    (setq acc (,op acc n)))
                  acc))))
  (def + system::%add 0)
  (def * system::%mul 1))

(macrolet ((def (name op)
             `(defun ,name (number &rest numbers)
                (unless (numberp number)
                  (error 'type-error :datum number :expected-type 'number))
                (dolist (n numbers)
                  (unless (numberp n)
                    (error 'type-error :datum n :expected-type 'number))
                  (setq number (,op number n)))
                number)))
  (def - system::%sub))

(defun floatp (x)
  (and (not (integerp x))
       (numberp x)))

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
    (when (< n number)
      (setq number n))))

(defun max (number &rest more-numbers)
  (dolist (n more-numbers number)
    (when (< number n)
      (setq number n))))

(defun expt (base power)
  (let ((acc 1))
    (dotimes (i power)
      (setq acc (* acc base)))
    acc))
