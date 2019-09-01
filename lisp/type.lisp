(in-package :common-lisp)

(defun typep (object type &optional environment)
  (declare (ignore environment))
  (case type
    (list (listp object))
    (cons (consp object))
    (symbol (symbolp object))
    (string (stringp object))
    ;(hash-table (hash-table-p object))
    (vector (vectorp object))
    ;(array (arrayp object))
    (integer (integerp object))
    (numberp (numberp object))
    (otherwise
     (if (system::structure-p object)
         (eq (system::structure-name object)
             type)))))

(defun subtypep (type1 type2 &optional environment)
  (declare (ignore environment))
  (cond ((eq type1 type2)
         (values t t))
        ((and (member type1 '(character base-char standard-char))
              (eq type2 'character))
         (values t t))
        (t
         (values nil nil))))
