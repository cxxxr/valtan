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
(defun (setf caar) (value x) (rplaca (car x) value))
(defun (setf cadr) (value x) (rplaca (cdr x) value))
(defun (setf cdar) (value x) (rplacd (car x) value))
(defun (setf cddr) (value x) (rplacd (cdr x) value))
(defun (setf caaar) (value x) (rplaca (car (car x)) value))
(defun (setf caadr) (value x) (rplaca (car (cdr x)) value))
(defun (setf cadar) (value x) (rplaca (cdr (car x)) value))
(defun (setf caddr) (value x) (rplaca (cdr (cdr x)) value))
(defun (setf cdaar) (value x) (rplacd (car (car x)) value))
(defun (setf cdadr) (value x) (rplacd (car (cdr x)) value))
(defun (setf cddar) (value x) (rplacd (cdr (car x)) value))
(defun (setf cdddr) (value x) (rplacd (cdr (cdr x)) value))
(defun (setf caaaar) (value x) (rplaca (car (car (car x))) value))
(defun (setf caaadr) (value x) (rplaca (car (car (cdr x))) value))
(defun (setf caadar) (value x) (rplaca (car (cdr (car x))) value))
(defun (setf caaddr) (value x) (rplaca (car (cdr (cdr x))) value))
(defun (setf cadaar) (value x) (rplaca (cdr (car (car x))) value))
(defun (setf cadadr) (value x) (rplaca (cdr (car (cdr x))) value))
(defun (setf caddar) (value x) (rplaca (cdr (cdr (car x))) value))
(defun (setf cadddr) (value x) (rplaca (cdr (cdr (cdr x))) value))
(defun (setf cdaaar) (value x) (rplacd (car (car (car x))) value))
(defun (setf cdaadr) (value x) (rplacd (car (car (cdr x))) value))
(defun (setf cdadar) (value x) (rplacd (car (cdr (car x))) value))
(defun (setf cdaddr) (value x) (rplacd (car (cdr (cdr x))) value))
(defun (setf cddaar) (value x) (rplacd (cdr (car (car x))) value))
(defun (setf cddadr) (value x) (rplacd (cdr (car (cdr x))) value))
(defun (setf cdddar) (value x) (rplacd (cdr (cdr (car x))) value))
(defun (setf cddddr) (value x) (rplacd (cdr (cdr (cdr x))) value))

#+(or)
(defun copy-tree (tree)
  (error "copy-tree is undefined"))

#+(or)
(defun sublis (alist tree &key key test test-not)
  (error "sublis is undefined"))

#+(or)
(defun nsublis (alist tree &key key test test-not)
  (error "nsublis is undefined"))

#+(or)
(defun subst (new old tree &key key test test-not)
  (error "subst is undefined"))

#+(or)
(defun subst-if (new predicate tree &key key)
  (error "subst-if is undefined"))

#+(or)
(defun subst-if-not (new predicate tree &key key)
  (error "subst-if-not is undefined"))

#+(or)
(defun nsubst (new old tree &key key test test-not)
  (error "nsubst is undefined"))

#+(or)
(defun nsubst-if (new predicate tree &key key)
  (error "nsubst-if is undefined"))

#+(or)
(defun nsubst-if-not (new predicate tree &key key)
  (error "nsubst-if-not is undefined"))

#+(or)
(defun tree-equal (tree-1 tree-2 &key test test-not)
  (error "tree-equal is undefined"))

(defun copy-list (list)
  (let ((head nil)
        (tail nil))
    (dolist (x list)
      (cond ((null tail)
             (setf head (setf tail (list x))))
            (t
             (setf (cdr tail) (list x))
             (setf tail (cdr tail)))))
    head))

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
  (let ((x (do ((n 0 (+ n 2))
                (y list (cddr y))
                (z list (cdr z)))
               (())
             (when (endp y) (return n))
             (when (endp (cdr y)) (return (+ n 1)))
             (when (and (eq y z) (> n 0)) (return nil)))))
    x))

#+(or)
(defun make-list (size &key initial-element)
  (error "make-list is undefined"))

(defmacro push (obj place)
  (multiple-value-bind (vars vals stores store-form access-form)
      (!get-setf-expansion place)
    `(let* ,(mapcar #'list
                    (append vars stores)
                    (append vals (list (list 'cons obj access-form))))
       ,store-form)))

(defmacro pop (place)
  (multiple-value-bind (vars vals stores store-form access-form)
      (!get-setf-expansion place)
    `(let* ,(mapcar #'list
                    (append vars stores)
                    (append vals (list (list 'cdr access-form))))
       (prog1 (car ,access-form)
         ,store-form))))

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

#+(or)
(defun nconc (&rest lists)
  (error "nconc is undefined"))

(defun append (&rest lists)
  (let ((head '())
        (tail nil))
    (dolist (list lists)
      (dolist (x list)
        (cond ((null tail)
               (setf head (setf tail (list x))))
              (t
               (setf (cdr tail) (list x))
               (setf tail (cdr tail))))))
    head))

#+(or)
(defun revappend (list tail)
  (error "revappend is undefined"))

#+(or)
(defun nreconc (list tail)
  (error "nreconc is undefined"))

#+(or)
(defun butlast (list &optional n)
  (error "butlast is undefined"))

#+(or)
(defun nbutlast (list &optional n)
  (error "nbutlast is undefined"))

#+(or)
(defun last (list &optional n)
  (error "last is undefined"))

#+(or)
(defun ldiff (list object)
  (error "ldiff is undefined"))

#+(or)
(defun tailp (object list)
  (error "tailp is undefined"))

(defun nthcdr (n list)
  (assert (<= 0 n))
  (dotimes (_ n)
    (setq list (cdr list)))
  list)

(defun rest (list)
  (cdr list))

(defsetf rest rplacd)

(defun member (item list &key (key #'identity key-p) (test nil test-p) (test-not nil test-not-p))
  (cond ((not (or key-p test-p test-not-p))
         (do ((rest list (cdr rest)))
             ((null rest))
           (when (eql item (car rest))
             (return rest))))
        (t
         (do ((rest list (cdr rest)))
             ((null rest))
           (when (cond (test-p
                        (funcall test item (funcall key (car rest))))
                       (test-not-p
                        (not (funcall test-not item (funcall key (car rest)))))
                       (t
                        (eql item (funcall key (car rest)))))
             (return list))))))

#+(or)
(defun member-if (predicate list &key key)
  (error "member-if is undefined"))

#+(or)
(defun member-if-not (predicate list &key key)
  (error "member-if-not is undefined"))

(defmacro with-accumulate (() &body body)
  (let ((g-head (gensym))
        (g-tail (gensym)))
    `(let ((,g-head nil)
           (,g-tail nil))
       (labels ((collect (x)
                  (cond ((null ,g-head)
                         (setf ,g-head (setf ,g-tail (list x))))
                        (t
                         (setf (cdr ,g-tail) (list x))
                         (setf ,g-tail (cdr ,g-tail))))))
         ,@body)
       ,g-head)))

(defun mapc (function list &rest lists)
  (let ((arglists (copy-list (cons list lists))))
    (do ()
        ((dolist (a arglists nil)
           (if (null a) (return t))))
      (apply function
             (with-accumulate ()
               (do ((l arglists (cdr l)))
                   ((null l))
                 (collect (caar l))
                 (setf (car l) (cdar l)))))))
  list)

(defun mapcar (function list &rest lists)
  (let ((arglists (copy-list (cons list lists))))
    (with-accumulate ()
      (do ()
          ((dolist (a arglists nil)
             (if (null a) (return t))))
        (collect (apply function
                        (with-accumulate ()
                          (do ((l arglists (cdr l)))
                              ((null l))
                            (collect (caar l))
                            (setf (car l) (cdar l))))))))))

#+(or)
(defun mapcan (function list &rest lists)
  (error "mapcan is undefined"))

#+(or)
(defun mapl (function list &rest lists)
  (error "mapl is undefined"))

#+(or)
(defun maplist (function list &rest lists)
  (error "maplist is undefined"))

#+(or)
(defun mapcon (function list &rest lists)
  (error "mapcon is undefined"))

(defun acons (key value alist)
  (cons (cons key value) alist))

#+(or)
(defun assoc (item alist &key key test test-not)
  (error "assoc is undefined"))

#+(or)
(defun assoc-if (predicate alist &key key)
  (error "assoc-if is undefined"))

#+(or)
(defun assoc-if-not (predicate alist &key key)
  (error "assoc-if-not is undefined"))

#+(or)
(defun copy-alist (alist)
  (error "copy-alist is undefined"))

#+(or)
(defun pairlis (keys data &optional alist)
  (error "pairlis is undefined"))

#+(or)
(defun rassoc (item alist &key key test test-not)
  (error "rassoc is undefined"))

#+(or)
(defun rassoc-if (predicate alist &key key)
  (error "rassoc-if is undefined"))

#+(or)
(defun rassoc-if-not (predicate alist &key key)
  (error "rassoc-if-not is undefined"))

#+(or)
(defun get-properties (plist indicator-list)
  (error "get-properties is undefined"))

(defun getf (place indicator &optional default)
  (do ((list place (cddr list)))
      ((null list) default)
    (when (eq indicator (car list))
      (return (cadr list)))))

#+(or)
(defsetf getf (plist indicator &optional default) (value)
  (error "(setf getf) is undefined"))

#+(or)
(defmacro remf (place indicator)
  (declare (ignore place indicator)))

#+(or)
(defun intersection (list-1 list-2 &key key test test-not)
  (error "intersection is undefined"))

#+(or)
(defun nintersection (list-1 list-2 &key key test test-not)
  (error "nintersection is undefined"))

#+(or)
(defun adjoin (item list &key key test test-not)
  (error "adjoin is undefined"))

#+(or)
(defmacro pushnew (item place &key key test test-not)
  (declare (ignore item place key test test-not)))

#+(or)
(defun set-difference (list-1 list-2 &key key test test-not)
  (error "set-difference is undefined"))

#+(or)
(defun nset-difference (list-1 list-2 &key key test test-not)
  (error "nset-difference is undefined"))

#+(or)
(defun set-exclusive-or (list-1 list-2 &key key test test-not)
  (error "set-exclusive-or is undefined"))

#+(or)
(defun nset-exclusive-or (list-1 list-2 &key key test test-not)
  (error "nset-exclusive-or is undefined"))

#+(or)
(defun subsetp (list-1 list-2 &key key test test-not)
  (error "subsetp is undefined"))

#+(or)
(defun union (list-1 list-2 &key key test test-not)
  (error "union is undefined"))

#+(or)
(defun nunion (list-1 list-2 &key key test test-not)
  (error "nunion is undefined"))

(defun system::list-to-js-array (list)
  (let ((js-array (ffi:new (ffi:ref "Array"))))
    (dolist (x list)
      ((ffi:ref js-array "push") x))
    js-array))
