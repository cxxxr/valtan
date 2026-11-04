#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defun funcall (function &rest args)
  (let ((function (ensure-function function)))
    (*:apply function (*:list-to-raw-array args))))

(defun apply (function arg &rest args)
  (let ((function (ensure-function function)))
    (cond ((null args)
           (unless (listp arg)
             (type-error arg 'list))
           (*:apply function (*:list-to-raw-array arg)))
          (t
           (let* ((head (list arg))
                  (tail head))
             (do ((rest args (cdr rest)))
                 ((null (cdr rest))
                  (unless (listp (car rest))
                    (type-error (car rest) 'list))
                  (setf (cdr tail) (car rest)))
               (let ((a (car rest)))
                 (setf (cdr tail) (list a))
                 (setq tail (cdr tail))))
             (*:apply function (*:list-to-raw-array head)))))))

(defun fdefinition (x)
  (cond ((symbolp x) (symbol-function x))
        ((and (consp x) (eq (car x) 'setf) (consp (cdr x))
              (symbolp (cadr x)) (null (cddr x)))
         (or (get (cadr x) '*:fdefinition-setf) (error 'undefined-function :name x)))
        (t (type-error x '(or symbol (cons (eql setf) (cons symbol null)))))))

(defun (cl:setf fdefinition) (function x)
  (or (cond ((consp x)
             (when (eq 'setf (car x))
               ;; TODO: clプリフィクスを外しても動くようにする
               (cl:setf (cl:get (cadr x) '*:fdefinition-setf)
                        function)))
            ((symbolp x)
             (setf (symbol-function x) function)))
      (error "Invalid function name: ~S" x)))

(defun proper-list-p (x)
  "Return T if X is a proper list (including NIL)."
  (let ((slow x) (fast x))
    (do () (nil)
      (when (atom fast) (return (null fast)))
      (setq fast (cdr fast))
      (when (atom fast) (return (null fast)))
      (setq fast (cdr fast) slow (cdr slow))
      (when (eq slow fast) (return nil)))))

(defun values-list (list)
  "Return the elements of LIST as multiple values."
  (unless (proper-list-p list)
    (type-error list '(satisfies proper-list-p)))
  (apply #'values list))
