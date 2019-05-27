(in-package :common-lisp)

(defun numberp (x)
  (equal (ffi:typeof x) "number"))

(defun integerp (x)
  (eq (ffi:ref "true")
      ((ffi:ref "Number" "isInteger") x)))

(defun + (&rest numbers)
  (let ((acc 0))
    (dolist (n numbers)
      (setq acc (system::add acc n)))
    acc))

(defun - (number &rest numbers)
  (dolist (n numbers)
    (setq number (system::sub number n)))
  number)

(defun * (&rest numbers)
  (let ((acc 1))
    (dolist (n numbers)
      (setq acc (system::mul acc n)))
    acc))

(macrolet ((def (name cmp)
             `(defun ,name (number &rest numbers)
                (dolist (n numbers t)
                  (unless (,cmp number n)
                    (return))))))
  (def = system::number-equal)
  (def > system::greater-than)
  (def >= system::greater-equal)
  (def < system::less-than)
  (def <= system::less-equal))

(defun plusp (x)
  (< 0 x))

(defun 1+ (x)
  (+ x 1))

(defun 1- (x)
  (- x 1))
