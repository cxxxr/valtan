(in-package :compiler)

(defun expand-backquote (x)
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
               (expand-backquote (rest x))))
        (t
         (list 'cons
               (expand-backquote (first x))
               (expand-backquote (rest x))))))
