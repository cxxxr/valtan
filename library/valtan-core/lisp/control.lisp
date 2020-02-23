#-valtan(cl:in-package :valtan-core)
#+valtan(cl:in-package :common-lisp)

(cl:defparameter lambda-list-keywords
  '(cl:&allow-other-keys cl:&aux cl:&body cl:&environment
    cl:&key cl:&optional cl:&rest cl:&whole))

(*:defmacro* return (cl:&optional value)
  `(cl:return-from cl:nil ,value))

(*:defmacro* cond (cl:&rest clauses)
  (cl:if (cl:null clauses)
         cl:nil
         (cl:let ((clause
                    (cl:first clauses))
                  (g-test (cl:gensym)))
           `(cl:let ((,g-test
                       ,(cl:first clause)))
              (cl:if ,g-test
                     ,(cl:if (cl:null
                              (cl:rest
                               clause))
                             g-test
                             `(cl:progn
                                ,@(cl:rest
                                   clause)))
                     (cond
                       ,@(cl:rest clauses)))))))

(*:defmacro* or (cl:&rest forms)
  (cl:if (cl:null forms)
         cl:nil
         (cl:let ((value (cl:gensym)))
           `(cl:let ((,value
                       ,(cl:first forms)))
              (cl:if ,value
                     ,value
                     (or
                      ,@(cl:rest forms)))))))

(*:defmacro* and (cl:&rest forms)
  (cond ((cl:null forms))
        ((cl:null (cl:rest forms))
         (cl:first forms))
        (cl:t
         `(cl:if ,(cl:first forms)
                 (and
                  ,@(cl:rest forms))
                 cl:nil))))

(*:defmacro* when
  (test cl:&rest forms)
  `(cl:if ,test
          (cl:progn ,@forms)))

(*:defmacro* unless
  (test cl:&rest forms)
  `(cl:if ,test
          cl:nil
          (cl:progn ,@forms)))

(cl:defun not (x)
  (cl:if x
         cl:nil
         cl:t))

;; !!!
(cl:eval-when (:compile-toplevel)
  ;(cl:declaim (cl:ftype (cl:function (cl:t cl:t) cl:t) type-error))
  (cl:defun ensure-function (value)
    (cl:cond ((cl:functionp value)
              value)
             ((cl:symbolp value)
              (cl:symbol-function value))
             (cl:t
              (type-error value 'cl:function)))))

(*:defmacro* multiple-value-call
  (cl:function sb-debug:arg cl:&rest args)
  (cl:if (cl:null args)
         `(*:multiple-value-call
              (ensure-function ,cl:function)
            ,(cl:if (cl:atom sb-debug:arg)
                    `(cl:values ,sb-debug:arg)
                    sb-debug:arg))
         `(*:multiple-value-call
              (ensure-function ,cl:function) ,sb-debug:arg
            ,@(cl:if (cl:atom
                      (cl:car (cl:last args)))
                     `(,@(cl:butlast args)
                       (cl:values
                        ,@(cl:last args)))
                     args))))

(*:defmacro* multiple-value-bind
  (vars value-form cl:&rest body)
  (cl:let ((cl:rest (cl:gensym)))
    `(multiple-value-call
         (cl:lambda
             (cl:&optional ,@vars cl:&rest ,cl:rest)
           (cl:declare (cl:ignore ,cl:rest))
           ,@body)
       ,value-form)))

(*:defmacro* multiple-value-list (value-form)
  `(multiple-value-call #'cl:list ,value-form))

(*:defmacro* multiple-value-prog1
  (first-form cl:&rest forms)
  (cl:let ((g-values (cl:gensym)))
    `(cl:let ((,g-values
                (multiple-value-list ,first-form)))
       ,@forms
       (cl:apply #'cl:values ,g-values))))

;; (cl:eval-when (:compile-toplevel)
;;   (cl:defun parse-body (body look-docstring-p)
;;     (cl:let ((declares '())
;;              (docstring cl:nil))
;;       (cl:do ((forms body (cl:rest forms)))
;;           ((cl:null forms)
;;            (cl:values cl:nil declares docstring))
;;         (cl:let ((form (cl:first forms)))
;;           (cl:cond ((and (cl:consp form)
;;                          (cl:eq 'cl:declare (cl:first form)))
;;                     (cl:setf declares (cl:append (cl:rest form) declares)))
;;                    ((and (cl:stringp form) look-docstring-p (cl:rest forms))
;;                     (cl:setq docstring form))
;;                    (cl:t
;;                     (cl:return (cl:values forms declares docstring)))))))))

(*:defmacro* do*
  (varlist endlist cl:&rest body)
  (cl:let ((g-start (cl:gensym))
           (body (compiler::parse-body body cl:nil)))
    (multiple-value-bind (body declares)
        (compiler::parse-body body cl:nil)
      (cl:let ((varlist
                 (cl:mapcar
                  (cl:lambda (var-spec)
                    (cl:if (cl:symbolp var-spec)
                           `(,var-spec cl:nil)
                           var-spec))
                  varlist)))
        `(cl:block cl:nil
           (cl:let* ,(cl:mapcar
                      (cl:lambda (var-spec)
                        `(,(cl:first var-spec)
                          ,(cl:second var-spec)))
                      varlist)
             (cl:declare ,@declares)
             (cl:tagbody
               ,g-start
               (cl:if ,(cl:first endlist)
                      (return
                        (cl:progn ,@(cl:rest endlist)))
                      (cl:progn
                        (cl:tagbody ,@body)
                        (cl:setq ,@(cl:mapcan
                                    (cl:lambda
                                        (var-spec)
                                      (cl:if (cl:cddr
                                              var-spec)
                                             `(,(cl:first
                                                 var-spec)
                                               ,(cl:third
                                                 var-spec))))
                                    varlist))
                        (cl:go ,g-start))))))))))

(*:defmacro* psetq (cl:&rest pairs)
  (when (cl:oddp (cl:length pairs))
    (cl:error "Odd number of args to PSETQ."))
  (cl:let ((gvars 'cl:nil)
           (vars 'cl:nil)
           (cl:values 'cl:nil))
    (do* ((pairs* pairs
                  (cl:cddr pairs*)))
        ((cl:null pairs*))
      (cl:let ((sb-debug:var (cl:first pairs*))
               (value (cl:second pairs*)))
        (cl:setq vars
                 (cl:cons sb-debug:var vars))
        (cl:setq cl:values
                 (cl:cons value cl:values))
        (cl:setq gvars
                 (cl:cons (cl:gensym) gvars))))
    (cl:setq gvars (cl:nreverse gvars))
    (cl:setq vars (cl:nreverse vars))
    (cl:setq cl:values (cl:nreverse cl:values))
    `(cl:let ,(cl:mapcar #'cl:list gvars
                         cl:values)
       ,@(cl:mapcar
          (cl:lambda (sb-debug:var gvar)
            `(cl:setq ,sb-debug:var ,gvar))
          vars gvars)
       cl:nil)))

(*:defmacro* do
  (varlist endlist cl:&rest body)
  (cl:let ((g-start (cl:gensym)))
    (multiple-value-bind (body declares)
        (compiler::parse-body body cl:nil)
      (cl:let ((varlist
                 (cl:mapcar
                  (cl:lambda (var-spec)
                    (cl:if (cl:symbolp var-spec)
                           `(,var-spec cl:nil)
                           var-spec))
                  varlist)))
        `(cl:block cl:nil
           (cl:let ,(cl:mapcar
                     (cl:lambda (var-spec)
                       `(,(cl:first var-spec)
                         ,(cl:second var-spec)))
                     varlist)
             (cl:declare ,@declares)
             (cl:tagbody
               ,g-start
               (cl:if ,(cl:first endlist)
                      (return
                        (cl:progn ,@(cl:rest endlist)))
                      (cl:progn
                        (cl:tagbody ,@body)
                        (psetq ,@(cl:mapcan
                                  (cl:lambda
                                      (var-spec)
                                    (cl:if (cl:cddr
                                            var-spec)
                                           `(,(cl:first
                                               var-spec)
                                             ,(cl:third
                                               var-spec))))
                                  varlist))
                        (cl:go ,g-start))))))))))

(*:defmacro* dotimes
  (var-form cl:&rest body)
  (cl:let ((sb-debug:var (cl:first var-form))
           (expr (cl:second var-form))
           (result (cl:third var-form))
           (g-expr (cl:gensym)))
    (multiple-value-bind (body declares)
        (compiler::parse-body body cl:nil)
      `(cl:let ((,g-expr ,expr))
         (do ((,sb-debug:var 0 (cl:+ ,sb-debug:var 1)))
             ((cl:>= ,sb-debug:var ,g-expr)
              ,result)
           (cl:declare ,@declares)
           ,@body)))))

(*:defmacro* dolist
  (var-form cl:&rest body)
  (cl:let* ((sb-debug:var (cl:first var-form))
            (expr (cl:second var-form))
            (result (cl:third var-form))
            (g-list (cl:gensym))
            (g-start (cl:gensym)))
    (multiple-value-bind (body declares)
        (compiler::parse-body body cl:nil)
      `(cl:block cl:nil
         (cl:let ((,g-list ,expr))
           (cl:tagbody
             ,g-start
             (unless (cl:endp ,g-list)
               (cl:let ((,sb-debug:var (cl:car ,g-list)))
                 (cl:declare ,@declares)
                 (cl:setq ,g-list
                          (cl:cdr ,g-list))
                 (cl:tagbody ,@body))
               (cl:go ,g-start))))
         (cl:let ((,sb-debug:var cl:nil))
           ,result)))))

(*:defmacro* case
  (keyform cl:&rest cases)
  (cl:let ((sb-debug:var (cl:gensym)))
    `(cl:let ((,sb-debug:var ,keyform))
       (cond
         ,@(cl:mapcar
            (cl:lambda (c)
              (cond
                ((cl:eq 'cl:otherwise (cl:car c))
                 `(cl:t ,@(cl:cdr c)))
                ((cl:listp (cl:car c))
                 `((cl:member ,sb-debug:var ',(cl:car c))
                   ,@(cl:cdr c)))
                (cl:t
                 `((eql ,sb-debug:var ',(cl:car c))
                   ,@(cl:cdr c)))))
            cases)))))

(*:defmacro* ecase
  (keyform cl:&rest cases)
  (cl:let ((sb-debug:var (cl:gensym)))
    `(cl:let ((,sb-debug:var ,keyform))
       (cond
         ,@(cl:mapcar
            (cl:lambda (c)
              (cond
                ((cl:listp (cl:car c))
                 `((cl:member ,sb-debug:var ',(cl:car c))
                   ,@(cl:cdr c)))
                (cl:t
                 `((eql ,sb-debug:var ',(cl:car c))
                   ,@(cl:cdr c)))))
            cases)
         (cl:t (cl:error "ecase error"))))))

(cl:defun eql (x y)
  (cond
    ((and (cl:characterp x)
          (cl:characterp y))
     (cl:char= x y))
    (cl:t (cl:eq x y))))

(cl:defun equal (x y)
  (cond
    ((and (cl:consp x)
          (cl:consp y))
     (and
      (equal (cl:car x)
             (cl:car y))
      (equal (cl:cdr x)
             (cl:cdr y))))
    ((and (cl:stringp x)
          (cl:stringp y))
     (cl:string= x y))
    ((and (cl:simple-bit-vector-p x)
          (cl:simple-bit-vector-p y))
     (and
      (cl:= (cl:length x)
            (cl:length y))
      (dotimes
          (i (cl:length x) cl:t)
        (unless
            (eql (cl:aref x i)
                 (cl:aref y i))
          (return cl:nil)))))
    (cl:t (eql x y))))

(cl:defun equalp (x y)
  (cond
    ((and (cl:characterp x)
          (cl:characterp y))
     (cl:char-equal x y))
    ((and (cl:numberp x)
          (cl:numberp y))
     (cl:= x y))
    ((and (cl:consp x)
          (cl:consp y))
     (and
      (equalp (cl:car x)
              (cl:car y))
      (equalp (cl:cdr x)
              (cl:cdr y))))
    ((and (cl:stringp x)
          (cl:stringp y))
     (cl:string-equal x y))
    ((and (cl:arrayp x)
          (cl:arrayp y))
     (and
      (cl:= (cl:length x)
            (cl:length y))
      (dotimes
          (i (cl:length x) cl:t)
        (unless
            (equalp (cl:aref x i)
                    (cl:aref y i))
          (return cl:nil)))))
    ((and (*:structure-p x)
          (*:structure-p y))
     (and
      (cl:eq (*:%structure-name x)
             (*:%structure-name y))
      (dotimes
          (i (*:%structure-slot-count x) cl:t)
        (unless
            (equalp (*:%structure-ref x i)
                    (*:%structure-ref y i))
          (return cl:nil)))))
    ((and (cl:hash-table-p x)
          (cl:hash-table-p y))
     (cl:error "trap"))
    (cl:t (eql x y))))

(*:defmacro* prog1
  (result cl:&rest body)
  (cl:let ((tmp (cl:gensym)))
    `(cl:let ((,tmp ,result))
       ,@body
       ,tmp)))

(cl:defun identity (x) x)

(cl:defun complement (cl:function)
  (cl:lambda (cl:&rest args)
    (not (cl:apply cl:function args))))

(cl:defun constantly (value)
  (cl:lambda (cl:&rest args)
    (cl:declare (cl:ignore args))
    value))
