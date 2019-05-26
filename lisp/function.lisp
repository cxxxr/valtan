(in-package :common-lisp)

(defun ensure-function (value)
  (cond ((functionp value)
         value)
        ((symbolp value)
         (symbol-function value))
        (t
         (error "type error"))))

(defun funcall (function &rest args)
  (let ((function (ensure-function function)))
    (system::apply function (system::list-to-js-array args))))

(defun apply (function arg &rest args)
  (let ((function (ensure-function function)))
    (cond ((null args)
           (unless (listp arg)
             (error "type error"))
           (system::apply function (system::list-to-js-array arg)))
          (t
           (let* ((head (list arg))
                  (tail head))
             (do ((rest args (cdr rest)))
                 ((null (cdr rest))
                  (unless (listp (car rest))
                    (error "type error"))
                  (setf (cdr tail) (car rest)))
               (let ((a (car rest)))
                 (setf (cdr tail) (list a))
                 (setq tail (cdr tail))))
             (system::apply function (system::list-to-js-array head)))))))

(defun functionp (x)
  (system::functionp x))
