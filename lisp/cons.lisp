(in-package :common-lisp)

(defun atom (x)
  (not (consp x)))

(defun null (x)
  (eq x nil))

(defun listp (x)
  (or (null x) (consp x)))

(defun (setf car) (value cons) (rplaca cons value) value)
(defun (setf cdr) (value cons) (rplacd cons value) value)

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
(defun (setf caar) (value x) (rplaca (car x) value) value)
(defun (setf cadr) (value x) (rplaca (cdr x) value) value)
(defun (setf cdar) (value x) (rplacd (car x) value) value)
(defun (setf cddr) (value x) (rplacd (cdr x) value) value)
(defun (setf caaar) (value x) (rplaca (car (car x)) value) value)
(defun (setf caadr) (value x) (rplaca (car (cdr x)) value) value)
(defun (setf cadar) (value x) (rplaca (cdr (car x)) value) value)
(defun (setf caddr) (value x) (rplaca (cdr (cdr x)) value) value)
(defun (setf cdaar) (value x) (rplacd (car (car x)) value) value)
(defun (setf cdadr) (value x) (rplacd (car (cdr x)) value) value)
(defun (setf cddar) (value x) (rplacd (cdr (car x)) value) value)
(defun (setf cdddr) (value x) (rplacd (cdr (cdr x)) value) value)
(defun (setf caaaar) (value x) (rplaca (car (car (car x))) value) value)
(defun (setf caaadr) (value x) (rplaca (car (car (cdr x))) value) value)
(defun (setf caadar) (value x) (rplaca (car (cdr (car x))) value) value)
(defun (setf caaddr) (value x) (rplaca (car (cdr (cdr x))) value) value)
(defun (setf cadaar) (value x) (rplaca (cdr (car (car x))) value) value)
(defun (setf cadadr) (value x) (rplaca (cdr (car (cdr x))) value) value)
(defun (setf caddar) (value x) (rplaca (cdr (cdr (car x))) value) value)
(defun (setf cadddr) (value x) (rplaca (cdr (cdr (cdr x))) value) value)
(defun (setf cdaaar) (value x) (rplacd (car (car (car x))) value) value)
(defun (setf cdaadr) (value x) (rplacd (car (car (cdr x))) value) value)
(defun (setf cdadar) (value x) (rplacd (car (cdr (car x))) value) value)
(defun (setf cdaddr) (value x) (rplacd (car (cdr (cdr x))) value) value)
(defun (setf cddaar) (value x) (rplacd (cdr (car (car x))) value) value)
(defun (setf cddadr) (value x) (rplacd (cdr (car (cdr x))) value) value)
(defun (setf cdddar) (value x) (rplacd (cdr (cdr (car x))) value) value)
(defun (setf cddddr) (value x) (rplacd (cdr (cdr (cdr x))) value) value)

(defun copy-tree (tree)
  (if (atom tree)
      tree
      (cons (copy-tree (car tree))
            (copy-tree (cdr tree)))))

(defun apply-key (key value)
  (if (null key)
      value
      (funcall key value)))

(defun sublis (alist tree &key key (test #'eql testp) (test-not #'eql test-not-p))
  (when (and testp test-not-p)
    (error ":TEST and :TEST-NOT were both supplied."))
  (labels ((f (tree)
             (let* ((k (apply-key key tree))
                    (elt (if test-not-p
                             (assoc k alist :test-not test-not)
                             (assoc k alist :test test))))
               (cond (elt (cdr elt))
                     ((atom tree) tree)
                     (t (let ((car (f (car tree)))
                              (cdr (f (cdr tree))))
                          (if (and (eq car (car tree))
                                   (eq cdr (cdr tree)))
                              tree
                              (cons car cdr))))))))
    (f tree)))

(defun nsublis (alist tree &key key (test #'eql testp) (test-not #'eql test-not-p))
  (when (and testp test-not-p)
    (error ":TEST and :TEST-NOT were both supplied."))
  (labels ((assoc* (value)
             (let ((k (apply-key key value)))
               (if test-not-p
                   (assoc k alist :test-not test-not)
                   (assoc k alist :test test))))
           (f (tree)
             (let ((elt (assoc* tree)))
               (cond (elt (cdr elt))
                     ((atom tree) tree)
                     (t (let ((car (f (car tree)))
                              (cdr (f (cdr tree))))
                          (rplaca tree car)
                          (rplacd tree cdr)
                          tree))))))
    (f tree)))

(defun subst (new old tree &key key (test #'eql testp) (test-not #'eql test-not-p))
  (when (and testp test-not-p)
    (error ":TEST and :TEST-NOT were both supplied."))
  (labels ((f (tree)
             (let ((k (apply-key key tree)))
               (cond ((if test-not-p
                          (not (funcall test-not old k))
                          (funcall test old k))
                      new)
                     ((atom tree) tree)
                     (t
                      (let ((car (f (car tree)))
                            (cdr (f (cdr tree))))
                        (if (and (eq car (car tree))
                                 (eq cdr (cdr tree)))
                            tree
                            (cons car cdr))))))))
    (f tree)))

(defun subst-if (new predicate tree &key key)
  (labels ((f (tree)
             (cond ((funcall predicate (apply-key key tree)) new)
                   ((atom tree) tree)
                   (t (let ((car (f (car tree)))
                            (cdr (f (cdr tree))))
                        (if (and (eq car (car tree))
                                 (eq cdr (cdr tree)))
                            tree
                            (cons car cdr)))))))
    (f tree)))

(defun subst-if-not (new predicate tree &key key)
  (subst-if new
            (complement predicate)
            tree :key key))

(defun nsubst (new old tree &key key (test #'eql testp) (test-not nil test-not-p))
  (when (and testp test-not-p)
    (error ":TEST and :TEST-NOT were both supplied."))
  (labels ((f (tree)
             (let ((k (apply-key key tree)))
               (cond ((if test-not-p
                          (not (funcall test-not old k))
                          (funcall test old k))
                      new)
                     ((atom tree) tree)
                     (t
                      (let ((car (f (car tree)))
                            (cdr (f (cdr tree))))
                        (rplaca tree car)
                        (rplacd tree cdr)
                        tree))))))
    (f tree)))

(defun nsubst-if (new predicate tree &key key)
  (labels ((f (tree)
             (cond ((funcall predicate (apply-key key tree)) new)
                   ((atom tree) tree)
                   (t (let ((car (f (car tree)))
                            (cdr (f (cdr tree))))
                        (rplaca tree car)
                        (rplacd tree cdr)
                        tree)))))
    (f tree)))

(defun nsubst-if-not (new predicate tree &key key)
  (nsubst-if new (complement predicate) tree :key key))

(defun tree-equal (tree-1 tree-2 &key (test #'eql testp) (test-not nil test-not-p))
  (labels ((test (x y)
             (if test-not-p
                 (not (funcall test-not x y))
                 (funcall test x y)))
           (f (x y)
             (cond ((consp x)
                    (and (consp y)
                         (f (car x) (car y))
                         (f (cdr x) (cdr y))))
                   ((consp y) nil)
                   ((test x y) t)
                   (t nil))))
    (f tree-1 tree-2)))

(defun copy-list (list)
  (if (atom list)
      list
      (cons (car list) (copy-list (cdr list)))))

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

(defun make-list (size &key initial-element)
  (labels ((f (n acc)
             (if (zerop n)
                 (nreverse acc)
                 (f (1- n) (cons initial-element acc)))))
    (f size nil)))

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
  (rplaca list value)
  value)

(defsetf nth set-nth)

(defun endp (x)
  (if (listp x)
      (null x)
      (type-error x 'list)))

(defun nconc (&rest lists)
  (let ((acc '()))
    (dolist (list lists)
      (when list
        (if acc
            (rplacd (last acc) list)
            (setq acc list))))
    acc))

(defun append (&rest lists)
  (let ((head '())
        (tail nil))
    (do ((lists lists (rest lists)))
        ((null lists))
      (let ((list (first lists)))
        (cond ((null list))
              ((atom list)
               (cond ((null tail)
                      (setf head (setf tail list)))
                     (t
                      (setf (cdr tail) list)
                      (setf tail (cdr tail)))))
              ((rest lists)
               (dolist (x list)
                 (cond ((null tail)
                        (setf head (setf tail (list x))))
                       (t
                        (setf (cdr tail) (list x))
                        (setf tail (cdr tail))))))
              (t
               (cond ((null tail)
                      (setf head (setf tail list)))
                     (t
                      (setf (cdr tail) list)
                      (setf tail (cdr tail))))))))
    head))

(defun revappend (list tail)
  (nconc (reverse list) tail))

(defun nreconc (list tail)
  (nconc (nreverse list) tail))

(defun butlast (list &optional (n 1))
  (labels ((rev (list acc)
             (if (atom list)
                 acc
                 (rev (rest list) (cons (first list) acc)))))
    (nreverse (nthcdr n (rev list nil)))))

(defun nbutlast (list &optional (n 1))
  (rplacd (last list (1+ n)) nil)
  list)

(defun last1 (list)
  (if (consp list)
      (do ((l list next)
           (next (cdr list) (cdr next)))
          ((atom next) l))
      list))

(defun last (list &optional (n 1))
  (if (= n 1)
      (last1 list)
      (do ((l list (cdr l))
           (r list)
           (i 0 (1+ i)))
          ((atom l) r)
        (when (>= i n) (pop r)))))

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

(defun process-list (item list key key-p test test-p test-not test-not-p
                     get-element return)
  (unless key-p (setq key #'identity))
  (let ((cmp (cond (test-p
                    test)
                   (test-not-p
                    (complement test-not))
                   (t
                    #'eql))))
    (do ((rest list (cdr rest)))
        ((null rest))
      (when (funcall cmp item (funcall key (funcall get-element (car rest))))
        (return (funcall return rest))))))

(defun member (item list &key (key nil key-p) (test nil test-p) (test-not nil test-not-p))
  (process-list item list key key-p test test-p test-not test-not-p #'identity #'identity))

(defun member-if (predicate list &key key)
  (do ((list list (rest list)))
      ((null list))
    (when (funcall predicate (apply-key (first list)))
      (return list))))

(defun member-if-not (predicate list &key key)
  (member-if (complement predicate) list :key key))

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

(defun mapcan (function list &rest lists)
  (let ((arglists (copy-list (cons list lists)))
        (acc '()))
    (do ()
        ((dolist (a arglists nil)
           (if (null a) (return t))))
      (setq acc
            ;; TODO: appendではなくnconcにする
            (append acc
                    (apply function
                           (with-accumulate ()
                             (do ((l arglists (cdr l)))
                                 ((null l))
                               (collect (caar l))
                               (setf (car l) (cdar l))))))))
    acc))

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

(defun assoc (item alist &key (key nil key-p) (test nil test-p) (test-not nil test-not-p))
  (process-list item alist key key-p test test-p test-not test-not-p #'car #'car))

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

(defun adjoin (item list &key (key nil key-p) (test nil test-p) (test-not nil test-not-p))
  (if (process-list (if key-p (funcall key item) item)
                    list key key-p test test-p test-not test-not-p
                    #'identity #'identity)
      list
      (cons item list)))

(defmacro pushnew (item place &rest args &key key test test-not)
  (declare (ignore key test test-not))
  (multiple-value-bind (vars vals stores store-form access-form)
      (!get-setf-expansion place)
    `(let* ,(mapcar #'list
                    (append vars stores)
                    (append vals (list (list* 'adjoin item access-form args))))
       ,store-form)))

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
