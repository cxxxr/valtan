#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(*:defmacro* return (&optional value)
  `(return-from nil ,value))

(*:defmacro* cond (&rest clauses)
  (if (cl:null clauses)
      nil
      (let ((clause (cl:first clauses))
            (g-test (cl:gensym)))
        `(let ((,g-test ,(cl:first clause)))
           (if ,g-test
               ,(if (cl:null (cl:rest clause))
                    g-test
                    `(progn ,@(cl:rest clause)))
               (cond ,@(cl:rest clauses)))))))

(*:defmacro* or (&rest forms)
  (if (cl:null forms)
      nil
      (let ((value (cl:gensym)))
        `(let ((,value ,(cl:first forms)))
           (if ,value
               ,value
               (or ,@(cl:rest forms)))))))

(*:defmacro* and (&rest forms)
  (cond ((cl:null forms))
        ((cl:null (cl:rest forms)) (cl:first forms))
        (t `(if ,(cl:first forms)
                (and ,@(cl:rest forms))
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
  (let ((g-start (cl:gensym))
        (body (compiler::parse-body body nil)))
    (cl:multiple-value-bind (body declares)
        (compiler::parse-body body nil)
      (let ((varlist
              (cl:mapcar (lambda (var-spec)
                           (if (cl:symbolp var-spec)
                               `(,var-spec nil)
                               var-spec))
                         varlist)))
        `(block nil
           (let* ,(cl:mapcar (lambda (var-spec)
                               `(,(cl:first var-spec)
                                 ,(cl:second var-spec)))
                             varlist)
             (declare ,@declares)
             (tagbody
               ,g-start
               (if ,(cl:first endlist)
                   (return (progn ,@(cl:rest endlist)))
                   (progn
                     (tagbody ,@body)
                     (setq ,@(cl:mapcan (cl:lambda (var-spec)
                                          (if (cl:cddr var-spec)
                                              `(,(cl:first var-spec)
                                                ,(cl:third var-spec))))
                                        varlist))
                     (go ,g-start))))))))))

(*:defmacro* psetq (&rest pairs)
  (when (cl:oddp (cl:length pairs))
    (cl:error "Odd number of args to PSETQ."))
  (let ((gvars '())
        (vars '())
        (values '()))
    (cl:do*
        ((pairs* pairs (cl:cddr pairs*)))
        ((cl:null pairs*))
      (let ((var (cl:first pairs*))
            (value (cl:second pairs*)))
        (setq vars (cl:cons var vars))
        (setq values (cl:cons value values))
        (setq gvars (cl:cons (cl:gensym) gvars))))
    (setq gvars (cl:nreverse gvars))
    (setq vars (cl:nreverse vars))
    (setq values (cl:nreverse values))
    `(let ,(cl:mapcar #'cl:list gvars values)
       ,@(cl:mapcar (cl:lambda (var gvar) `(setq ,var ,gvar))
                    vars gvars)
       nil)))

(*:defmacro* do (varlist endlist &rest body)
  (let ((g-start (cl:gensym)))
    (cl:multiple-value-bind (body declares)
        (compiler::parse-body body nil)
      (let ((varlist
              (cl:mapcar (lambda (var-spec)
                           (if (cl:symbolp var-spec)
                               `(,var-spec nil)
                               var-spec))
                         varlist)))
        `(block nil
           (let ,(cl:mapcar (lambda (var-spec)
                              `(,(cl:first var-spec)
                                ,(cl:second var-spec)))
                            varlist)
             (declare ,@declares)
             (tagbody
               ,g-start
               (if ,(cl:first endlist)
                   (return (progn ,@(cl:rest endlist)))
                   (progn
                     (tagbody ,@body)
                     (psetq ,@(cl:mapcan (cl:lambda (var-spec)
                                           (if (cl:cddr var-spec)
                                               `(,(cl:first var-spec)
                                                 ,(cl:third var-spec))))
                                         varlist))
                     (go ,g-start))))))))))

(*:defmacro* dotimes (var-form &rest body)
  (let ((var (cl:first var-form))
        (expr (cl:second var-form))
        (result (cl:third var-form))
        (g-expr (cl:gensym)))
    (cl:multiple-value-bind (body declares)
        (compiler::parse-body body nil)
      `(let ((,g-expr ,expr))
         (do ((,var 0 (+ ,var 1)))
             ((>= ,var ,g-expr) ,result)
           (declare ,@declares)
           ,@body)))))

(*:defmacro* dolist (var-form &rest body)
  (let* ((var (cl:first var-form))
         (expr (cl:second var-form))
         (result (cl:third var-form))
         (g-list (cl:gensym))
         (g-start (cl:gensym)))
    (cl:multiple-value-bind (body declares)
        (compiler::parse-body body nil)
      `(block nil
         (let ((,g-list ,expr))
           (tagbody
             ,g-start
             (unless (endp ,g-list)
               (let ((,var (car ,g-list)))
                 (declare ,@declares)
                 (setq ,g-list (cdr ,g-list))
                 (tagbody ,@body))
               (go ,g-start))))
         (let ((,var nil))
           (declare (ignorable ,var))
           ,result)))))

(*:defmacro* case (keyform &rest cases)
  (let ((var (cl:gensym)))
    `(let ((,var ,keyform))
       (cond ,@(cl:mapcar (cl:lambda (c)
                            (cl:cond ((cl:eq 'otherwise (cl:car c))
                                      `(t ,@(cl:cdr c)))
                                     ((cl:listp (cl:car c))
                                      `((member ,var ',(cl:car c))
                                        ,@(cl:cdr c)))
                                     (t
                                      `((eql ,var ',(cl:car c))
                                        ,@(cl:cdr c)))))
                          cases)))))

(*:defmacro* ecase (keyform &rest cases)
  (let ((var (cl:gensym)))
    `(let ((,var ,keyform))
       (cond ,@(cl:mapcar (cl:lambda (c)
                            (cl:cond ((cl:listp (cl:car c))
                                      `((member ,var ',(cl:car c))
                                        ,@(cl:cdr c)))
                                     (t
                                      `((eql ,var ',(cl:car c))
                                        ,@(cl:cdr c)))))
                          cases)
             (t (error "ecase error"))))))

(*:defmacro* multiple-value-bind (vars value-form &rest body)
  (let ((rest (cl:gensym)))
    `(multiple-value-call (lambda (&optional ,@vars &rest ,rest)
                            (declare (ignore ,rest))
                            ,@body)
       ,value-form)))

(declaim (ftype function type-error))
(defun ensure-function (value)
  (cond ((cl:functionp value)
         value)
        ((cl:symbolp value)
         (cl:symbol-function value))
        (t
         (type-error value 'function))))

(*:defmacro* multiple-value-call (function arg &rest args)
  (if (cl:null args)
      `(*:multiple-value-call (ensure-function ,function)
         ,(if (cl:atom arg)
              `(values ,arg)
              arg))
      `(*:multiple-value-call (ensure-function ,function)
         ,arg
         ,@(if (cl:atom (cl:car (cl:last args)))
               `(,@(cl:butlast args) (values ,@(cl:last args)))
               args))))

(*:defmacro* multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))

(*:defmacro* multiple-value-prog1 (first-form &rest forms)
  (let ((g-values (cl:gensym)))
    `(let ((,g-values (multiple-value-list ,first-form)))
       ,@forms
       (apply #'values ,g-values))))

(declaim (ftype function char=))

(defun eql (x y)
  (cond ((and (characterp x) (characterp y))
         (char= x y))
        (t
         (eq x y))))

(declaim (ftype function
                car cdr string=
                simple-bit-vector-p length = aref + >=))

(defun equal (x y)
  (cond ((and (consp x)
              (consp y))
         (and (equal (car x) (car y))
              (equal (cdr x) (cdr y))))
        ((and (cl:stringp x)
              (cl:stringp y))
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

(declaim (ftype function
                char-equal string-equal arrayp hash-table-p error))

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
        ((and (cl:stringp x)
              (cl:stringp y))
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
  (let ((tmp (cl:gensym)))
    `(let ((,tmp ,result))
       ,@body
       ,tmp)))

(defun identity (x) x)

(declaim (ftype function apply))

(defun complement (function)
  (lambda (&rest args)
    (not (apply function args))))

(defun constantly (value)
  (lambda (&rest args)
    (declare (ignore args))
    value))
