(in-package :compiler)

(defun expand-quasiquote (x)
  (cond ((atom x)
         (list 'quote x))
        ((eq '*:unquote (first x))
         (assert (= 2 (length x)))
         (second x))
        ((and (consp (first x))
              (eq (first (first x)) '*:unquote-splicing))
         (assert (= 2 (length (first x))))
         (list 'append
               (second (first x))
               (expand-quasiquote (rest x))))
        (t
         (list 'cons
               (expand-quasiquote (first x))
               (expand-quasiquote (rest x))))))
