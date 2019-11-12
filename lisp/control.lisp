(in-package :common-lisp)

(defparameter lambda-list-keywords
  '(&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY &OPTIONAL &REST &WHOLE))

(*:defmacro* return (&optional value)
  `(return-from nil ,value))

(*:defmacro* cond (&rest clauses)
  (if (null clauses)
      nil
      (let ((clause (first clauses))
            (g-test (gensym)))
        `(let ((,g-test ,(first clause)))
           (if ,g-test
               ,(if (null (rest clause))
                    g-test
                    `(progn ,@(rest clause)))
               (cond ,@(rest clauses)))))))

(*:defmacro* or (&rest forms)
  (if (null forms)
      nil
      (let ((value (gensym)))
        `(let ((,value ,(first forms)))
           (if ,value
               ,value
               (or ,@(rest forms)))))))

(*:defmacro* and (&rest forms)
  (cond ((null forms))
        ((null (rest forms)) (first forms))
        (t `(if ,(first forms)
                (and ,@(rest forms))
                nil))))

(*:defmacro* when (test &rest forms)
  `(if ,test
       (progn ,@forms)))

(*:defmacro* unless (test &rest forms)
  `(if ,test
       nil
       (progn ,@forms)))

(defun not (x)
  (if x nil t))

(*:defmacro* do* (varlist endlist &rest body)
  (let ((g-start (gensym))
        (body (compiler::parse-body body nil)))
    `(block nil
       (let* ,(mapcar (lambda (var-spec)
                        `(,(first var-spec)
                          ,(second var-spec)))
                      varlist)
         (tagbody
           ,g-start
           (if ,(first endlist)
               (return (progn ,@(rest endlist)))
               (progn
                 (tagbody ,@body)
                 (setq ,@(mapcan (lambda (var-spec)
                                   (if (cddr var-spec)
                                       `(,(first var-spec)
                                         ,(third var-spec))))
                                 varlist))
                 (go ,g-start))))))))

(*:defmacro* psetq (&rest pairs)
  (when (oddp (length pairs))
    (error "Odd number of args to PSETQ."))
  (let ((gvars '())
        (vars '())
        (values '()))
    (do* ((pairs* pairs (cddr pairs*)))
        ((null pairs*))
      (let ((var (first pairs*))
            (value (second pairs*)))
        (setq vars (cons var vars))
        (setq values (cons value values))
        (setq gvars (cons (gensym) gvars))))
    (setq gvars (nreverse gvars))
    (setq vars (nreverse vars))
    (setq values (nreverse values))
    `(let ,(mapcar #'list gvars values)
       ,@(mapcar (lambda (var gvar) `(setq ,var ,gvar))
                 vars gvars)
       nil)))

(*:defmacro* do (varlist endlist &rest body)
  (let ((g-start (gensym))
        (body (compiler::parse-body body nil)))
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

(*:defmacro* dotimes (var-form &rest body)
  (let ((var (first var-form))
        (expr (second var-form))
        (result (third var-form))
        (g-expr (gensym))
        (body (compiler::parse-body body nil)))
    `(let ((,g-expr ,expr))
       (do ((,var 0 (+ ,var 1)))
           ((>= ,var ,g-expr) ,result)
         ,@body))))

(*:defmacro* dolist (var-form &rest body)
  (let* ((var (first var-form))
         (expr (second var-form))
         (result (third var-form))
         (g-list (gensym))
         (g-start (gensym))
         (body (compiler::parse-body body nil)))
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

(*:defmacro* case (keyform &rest cases)
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

(*:defmacro* ecase (keyform &rest cases)
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

(*:defmacro* multiple-value-bind (vars value-form &rest body)
  (let ((rest (gensym)))
    `(multiple-value-call (lambda (&optional ,@vars &rest ,rest)
                            (declare (ignore ,rest))
                            ,@body)
       ,value-form)))

(*:defmacro* multiple-value-call (function arg &rest args)
  (if (null args)
      `(*:multiple-value-call (ensure-function ,function)
         ,(if (atom arg)
              `(values ,arg)
              arg))
      `(*:multiple-value-call (ensure-function ,function)
         ,arg
         ,@(if (atom (car (last args)))
               `(,@(butlast args) (values ,@(last args)))
               args))))

(*:defmacro* multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))

(*:defmacro* multiple-value-prog1 (first-form &rest forms)
  (let ((g-values (gensym)))
    `(let ((,g-values (multiple-value-list ,first-form)))
       ,@forms
       (apply #'values ,g-values))))

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
        ((and (simple-bit-vector-p x)
              (simple-bit-vector-p y))
         (and (= (length x) (length y))
              (dotimes (i (length x) t)
                (unless (eql (aref x i)
                             (aref y i))
                  (return nil)))))
        (t
         (eql x y))))

(defun equalp (x y)
  (cond ((and (characterp x)
              (characterp y))
         (char-equal x y))
        ((and (numberp x)
              (numberp y))
         (= x y))
        ((and (consp x)
              (consp y))
         (and (equalp (car x) (car y))
              (equalp (cdr x) (cdr y))))
        ((and (stringp x)
              (stringp y))
         (string-equal x y))
        ((and (arrayp x)
              (arrayp y))
         (and (= (length x) (length y))
              (dotimes (i (length x) t)
                (unless (equalp (aref x i)
                                (aref y i))
                  (return nil)))))
        ((and (*:structure-p x)
              (*:structure-p y))
         (and (eq (*:%structure-name x)
                  (*:%structure-name y))
              (dotimes (i (*:%structure-slot-count x) t)
                (unless (equalp (*:%structure-ref x i)
                                (*:%structure-ref y i))
                  (return nil)))))
        ((and (hash-table-p x)
              (hash-table-p y))
         (error "trap"))
        (t
         (eql x y))))

(*:defmacro* prog1 (result &rest body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,result))
       ,@body
       ,tmp)))

(defun identity (x) x)

(defun complement (function)
  (lambda (&rest args)
    (not (apply function args))))

(defun constantly (value)
  (lambda (&rest args)
    (declare (ignore args))
    value))
