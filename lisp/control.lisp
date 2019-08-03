(in-package :common-lisp)

(defmacro return (&optional value)
  `(return-from nil ,value))

(defmacro cond (&rest clauses)
  (if (null clauses)
      nil
      (let ((clause (first clauses)))
        `(if ,(first clause)
             ,(if (null (rest clause))
                  t
                  `(progn ,@(rest clause)))
             (cond ,@(rest clauses))))))

(defmacro or (&rest forms)
  (if (null forms)
      nil
      (let ((value (gensym)))
        `(let ((,value ,(first forms)))
           (if ,value
               ,value
               (or ,@(rest forms)))))))

(defmacro and (&rest forms)
  (cond ((null forms))
        ((null (rest forms)) (first forms))
        (t `(if ,(first forms)
                (and ,@(rest forms))
                nil))))

(defmacro when (test &body forms)
  `(if ,test
       (progn ,@forms)))

(defmacro unless (test &body forms)
  `(if ,test
       nil
       (progn ,@forms)))

(defun not (x)
  (if x nil t))

(defmacro psetq (&rest pairs)
  (when (oddp (length pairs))
    (error "Odd number of args to PSETQ."))
  (let ((gvars '())
        (vars '())
        (values '()))
    (do ((pairs* pairs (cddr pairs*)))
        ((null pairs*))
      (let ((var (first pairs*))
            (value (second pairs*)))
        (push var vars)
        (push value values)
        (push (gensym) gvars)))
    (setq gvars (nreverse gvars))
    (setq vars (nreverse vars))
    (setq values (nreverse values))
    `(let ,(mapcar #'list gvars values)
       ,@(mapcar (lambda (var gvar) `(setq ,var ,gvar))
                 vars gvars)
       nil)))

(defmacro do (varlist endlist &body body)
  (assert (not (null endlist)))
  (let ((g-start (gensym)))
    `(block nil
       (let ,(mapcar (lambda (var-spec)
                       `(,(first var-spec)
                         ,(second var-spec)))
                     varlist)
         (tagbody
           ,g-start
           (if ,(first endlist)
               (return (progn ,@(rest endlist)))
               (progn
                 (tagbody ,@body)
                 (psetq ,@(mapcan (lambda (var-spec)
                                    (if (cddr var-spec)
                                        `(,(first var-spec)
                                          ,(third var-spec))))
                                  varlist))
                 (go ,g-start))))))))

(defmacro dotimes ((var expr &optional result) &body body)
  (let ((g-expr (gensym)))
    `(let ((,g-expr ,expr))
       (do ((,var 0 (+ ,var 1)))
           ((>= ,var ,g-expr) ,result)
         ,@body))))

(defmacro dolist ((var expr &optional result) &body body)
  (let ((g-list (gensym))
        (g-start (gensym)))
    `(block nil
       (let ((,g-list ,expr))
         (tagbody
           ,g-start
           (unless (endp ,g-list)
             (let ((,var (car ,g-list)))
               (setq ,g-list (cdr ,g-list))
               (tagbody ,@body))
             (go ,g-start))))
       ,result)))

(defmacro case (keyform &body cases)
  (let ((var (gensym)))
    `(let ((,var ,keyform))
       (cond ,@(mapcar (lambda (c)
                         (cond ((eq 'otherwise (car c))
                                `(t ,@(cdr c)))
                               ((listp (car c))
                                `((member ,var ',(car c))
                                  ,@(cdr c)))
                               (t
                                `((eql ,var ',(car c))
                                  ,@(cdr c)))))
                       cases)))))

(defmacro ecase (keyform &body cases)
  (let ((var (gensym)))
    `(let ((,var ,keyform))
       (cond ,@(mapcar (lambda (c)
                         (cond ((listp (car c))
                                `((member ,var ',(car c))
                                  ,@(cdr c)))
                               (t
                                `((eql ,var ',(car c))
                                  ,@(cdr c)))))
                       cases)
             (t (error "ecase error"))))))

(defmacro multiple-value-bind (vars value-form &body body)
  (let ((rest (gensym)))
    `(multiple-value-call (lambda (&optional ,@vars &rest ,rest)
                            (declare (ignore ,rest))
                            ,@body)
       ,value-form)))

(defmacro multiple-value-call (function arg &rest args)
  (if (null args)
      `(system::multiple-value-call (ensure-function ,function)
         ,(if (atom arg)
              `(values ,arg)
              arg))
      `(system::multiple-value-call (ensure-function ,function)
         ,arg
         ,@(if (atom (car (last args)))
               `(,@(butlast args) (values ,@(last args)))
               args))))

(defmacro multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))

(defun eql (x y)
  (cond ((and (characterp x) (characterp y))
         (char= x y))
        (t
         (eq x y))))

(defun equal (x y)
  (cond ((and (consp x)
              (consp y))
         (and (equal (car x) (car y))
              (equal (cdr x) (cdr y))))
        ((and (stringp x)
              (stringp y))
         (string= x y))
        (t
         (eql x y))))

(defmacro prog1 (result &body body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,result))
       ,@body
       ,tmp)))

(defun identity (x) x)
