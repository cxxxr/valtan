(in-package :common-lisp)

(defun atom (x)
  (not (consp x)))

(defun null (x)
  (eq x nil))

(defun listp (x)
  (or (null x) (consp x)))

(defsetf car rplaca)
(defsetf cdr rplacd)

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))
(defun caaar (x) (car (car (car x))))
(defun caadr (x) (car (car (cdr x))))
(defun cadar (x) (car (cdr (car x))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cdaar (x) (cdr (car (car x))))
(defun cdadr (x) (cdr (car (cdr x))))
(defun cddar (x) (cdr (cdr (car x))))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun caaaar (x) (car (car (car (car x)))))
(defun caaadr (x) (car (car (car (cdr x)))))
(defun caadar (x) (car (car (cdr (car x)))))
(defun caaddr (x) (car (car (cdr (cdr x)))))
(defun cadaar (x) (car (cdr (car (car x)))))
(defun cadadr (x) (car (cdr (car (cdr x)))))
(defun caddar (x) (car (cdr (cdr (car x)))))
(defun cadddr (x) (car (cdr (cdr (cdr x)))))
(defun cdaaar (x) (cdr (car (car (car x)))))
(defun cdaadr (x) (cdr (car (car (cdr x)))))
(defun cdadar (x) (cdr (car (cdr (car x)))))
(defun cdaddr (x) (cdr (car (cdr (cdr x)))))
(defun cddaar (x) (cdr (cdr (car (car x)))))
(defun cddadr (x) (cdr (cdr (car (cdr x)))))
(defun cdddar (x) (cdr (cdr (cdr (car x)))))
(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))
(defsetf caar (x) (value) `(rplaca (car ,x) ,value))
(defsetf cadr (x) (value) `(rplaca (cdr ,x) ,value))
(defsetf cdar (x) (value) `(rplacd (car ,x) ,value))
(defsetf cddr (x) (value) `(rplacd (cdr ,x) ,value))
(defsetf caaar (x) (value) `(rplaca (car (car ,x)) ,value))
(defsetf caadr (x) (value) `(rplaca (car (cdr ,x)) ,value))
(defsetf cadar (x) (value) `(rplaca (cdr (car ,x)) ,value))
(defsetf caddr (x) (value) `(rplaca (cdr (cdr ,x)) ,value))
(defsetf cdaar (x) (value) `(rplacd (car (car ,x)) ,value))
(defsetf cdadr (x) (value) `(rplacd (car (cdr ,x)) ,value))
(defsetf cddar (x) (value) `(rplacd (cdr (car ,x)) ,value))
(defsetf cdddr (x) (value) `(rplacd (cdr (cdr ,x)) ,value))
(defsetf caaaar (x) (value) `(rplaca (car (car (car ,x))) ,value))
(defsetf caaadr (x) (value) `(rplaca (car (car (cdr ,x))) ,value))
(defsetf caadar (x) (value) `(rplaca (car (cdr (car ,x))) ,value))
(defsetf caaddr (x) (value) `(rplaca (car (cdr (cdr ,x))) ,value))
(defsetf cadaar (x) (value) `(rplaca (cdr (car (car ,x))) ,value))
(defsetf cadadr (x) (value) `(rplaca (cdr (car (cdr ,x))) ,value))
(defsetf caddar (x) (value) `(rplaca (cdr (cdr (car ,x))) ,value))
(defsetf cadddr (x) (value) `(rplaca (cdr (cdr (cdr ,x))) ,value))
(defsetf cdaaar (x) (value) `(rplacd (car (car (car ,x))) ,value))
(defsetf cdaadr (x) (value) `(rplacd (car (car (cdr ,x))) ,value))
(defsetf cdadar (x) (value) `(rplacd (car (cdr (car ,x))) ,value))
(defsetf cdaddr (x) (value) `(rplacd (car (cdr (cdr ,x))) ,value))
(defsetf cddaar (x) (value) `(rplacd (cdr (car (car ,x))) ,value))
(defsetf cddadr (x) (value) `(rplacd (cdr (car (cdr ,x))) ,value))
(defsetf cdddar (x) (value) `(rplacd (cdr (cdr (car ,x))) ,value))
(defsetf cddddr (x) (value) `(rplacd (cdr (cdr (cdr ,x))) ,value))

(defun copy-tree (tree)
  )

(defun sublis (alist tree &key key test test-not)
  )

(defun nsublis (alist tree &key key test test-not)
  )

(defun subst (new old tree &key key test test-not)
  )

(defun subst-if (new predicate tree &key key)
  )

(defun subst-if-not (new predicate tree &key key)
  )

(defun nsubst (new old tree &key key test test-not)
  )

(defun nsubst-if (new predicate tree &key key)
  )

(defun nsubst-if-not (new predicate tree &key key)
  )

(defun tree-equal (tree-1 tree-2 &key test test-not)
  )

(defun copy-list (list)
  )

(defun list (&rest list)
  list)

(defun list* (arg &rest args)
  (labels ((f (args)
             (cond ((null (cdr args))
                    (car args))
                   (t
                    (cons (car args)
                          (f (cdr args)))))))
    (if (null args)
        arg
        (cons arg (f args)))))

(defun list-length (list)
  )

(defun make-list (size &key initial-element)
  )

(defmacro push (obj place)
  (declare (ignore obj place)))

(defmacro pop (place)
  (declare (ignore place)))

(defun first (list) (car list))
(defun second (list) (nth 1 list))
(defun third (list) (nth 2 list))
(defun fourth (list) (nth 3 list))
(defun fifth (list) (nth 4 list))
(defun sixth (list) (nth 5 list))
(defun seventh (list) (nth 6 list))
(defun eighth (list) (nth 7 list))
(defun ninth (list) (nth 8 list))
(defun tenth (list) (nth 9 list))

(defsetf first (list) (value) `(setf (nth 0 ,list) ,value))
(defsetf second (list) (value) `(setf (nth 1 ,list) ,value))
(defsetf third (list) (value) `(setf (nth 2 ,list) ,value))
(defsetf fourth (list) (value) `(setf (nth 3 ,list) ,value))
(defsetf fifth (list) (value) `(setf (nth 4 ,list) ,value))
(defsetf sixth (list) (value) `(setf (nth 5 ,list) ,value))
(defsetf seventh (list) (value) `(setf (nth 6 ,list) ,value))
(defsetf eighth (list) (value) `(setf (nth 7 ,list) ,value))
(defsetf ninth (list) (value) `(setf (nth 8 ,list) ,value))
(defsetf tenth (list) (value) `(setf (nth 9 ,list) ,value))

(defun nth (n list)
  (dotimes (i n)
    (setq list (cdr list)))
  (car list))

(defun set-nth (n list value)
  (dotimes (i n)
    (setq list (cdr list)))
  (rplaca list value))

(defsetf nth set-nth)

(defun endp (x)
  (if (listp x)
      (null x)
      (error "type error")))

(defun nconc (&rest lists)
  )

(defun append (&rest lists)
  )

(defun revappend (list tail)
  )

(defun nreconc (list tail)
  )

(defun butlast (list &optional n)
  )

(defun nbutlast (list &optional n)
  )

(defun last (list &optional n)
  )

(defun ldiff (list object)
  )

(defun tailp (object list)
  )

(defun nthcdr (n list)
  )

(defun rest (list)
  (cdr list))

(defsetf rest rplacd)

(defun member (item list &key key test test-not)
  )

(defun member-if (predicate list &key key)
  )

(defun member-if-not (predicate list &key key)
  )

(defun mapc (function list &rest lists)
  )

(defun macar (function list &rest lists)
  )

(defun mapcan (function list &rest lists)
  )

(defun mapl (function list &rest lists)
  )

(defun maplist (function list &rest lists)
  )

(defun mapcon (function list &rest lists)
  )

(defun acons (key value alist)
  (cons (cons key value) alist))

(defun assoc (item alist &key key test test-not)
  )

(defun assoc-if (predicate alist &key key)
  )

(defun assoc-if-not (predicate alist &key key)
  )

(defun copy-alist (alist)
  )

(defun pairlis (keys data &optional alist)
  )

(defun rassoc (item alist &key key test test-not)
  )

(defun rassoc-if (predicate alist &key key)
  )

(defun rassoc-if-not (predicate alist &key key)
  )

(defun get-properties (plist indicator-list)
  )

(defun getf (plist indicator &optional default)
  )

(defsetf getf (plist indicator &optional default) (value)
  )

(defmacro remf (place indicator)
  (declare (ignore place indicator)))

(defun intersection (list-1 list-2 &key key test test-not)
  )

(defun nintersection (list-1 list-2 &key key test test-not)
  )

(defun adjoin (item list &key key test test-not)
  )

(defmacro pushnew (item place &key key test test-not)
  (declare (ignore item place key test test-not)))

(defun set-difference (list-1 list-2 &key key test test-not)
  )

(defun nset-difference (list-1 list-2 &key key test test-not)
  )

(defun set-exclusive-or (list-1 list-2 &key key test test-not)
  )

(defun nset-exclusive-or (list-1 list-2 &key key test test-not)
  )

(defun subsetp (list-1 list-2 &key key test test-not)
  )

(defun union (list-1 list-2 &key key test test-not)
  )

(defun nunion (list-1 list-2 &key key test test-not)
  )
