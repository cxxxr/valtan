(cl:defpackage :valtan-core/loop
  (:use :cl))
(cl:in-package :valtan-core/loop)

(defvar *loop-exps*)

(defvar *named*)
(defvar *temporary-variables*)
(defvar *initially-forms*)
(defvar *finally-forms*)
(defvar *with-clauses*)
(defvar *for-clauses*)
(defvar *do-clauses*)

(defstruct for-clause
  var
  init-form
  update-form
  while-form)

(defun loop-error (msg &rest args)
  (apply #'error msg args))

(defun ensure-keyword (x)
  (cond ((keywordp x)
         x)
        ((symbolp x)
         (intern (symbol-name x) :keyword))
        (t
         (loop-error "unexpected expression: ~S" x))))

(defun next-exp ()
  (pop *loop-exps*))

(defun lookahead ()
  (car *loop-exps*))

(defun end-of-loop-p ()
  (null *loop-exps*))

(defun check-variable (var)
  (unless (and (symbolp var)
               (not (keywordp var))
               (not (null var)))
    (loop-error "~S is not variable" var)))

(defun type-spec ()
  (case (lookahead)
    ((cl:fixnum cl:float cl:t cl:nil)
     (next-exp))
    ((of-type)
     (next-exp)
     (next-exp))))

(defun parse-compound-forms ()
  (do ((forms nil))
      ((or (end-of-loop-p)
           (symbolp (lookahead)))
       (nreverse forms))
    (push (next-exp) forms)))

(defun parse-with-clause ()
  (do ()
      (nil)
    (let ((var (next-exp)))
      (check-variable var)
      (type-spec)
      (let ((initial-form
              (if (eq (ensure-keyword (lookahead)) :=)
                  (progn
                    (next-exp)
                    (next-exp))
                  nil)))
        (setf *with-clauses*
              (nconc *with-clauses*
                     (list (list var initial-form)))))
      (if (eq (ensure-keyword (lookahead)) :and)
          (next-exp)
          (return)))))

(defun parse-for-as-arithmetic (var first-op)
  (let* ((form1 (next-exp))
         (second-op (ensure-keyword (next-exp)))
         (form2 (next-exp))
         (by-form (when (eq (ensure-keyword (lookahead)) :by)
                    (next-exp)
                    (next-exp)))
         (form2-gsym (when (consp form2) (gensym)))
         (by-form-gsym (when (consp by-form) (gensym)))
         step-op
         while-op)
    (when form2-gsym
      (push (list form2-gsym form2) *temporary-variables*))
    (when by-form-gsym
      (push (list by-form-gsym by-form) *temporary-variables*))
    (cond ((and (member first-op '(:from :upfrom))
                (member second-op '(:to :upto :below)))
           (setf step-op '+
                 while-op (if (eq second-op :below) '< '<=)))
          ((and (member first-op '(:from :downfrom))
                (member second-op '(:to :downto :above)))
           (setf step-op '-
                 while-op (if (eq second-op :above) '> '>=)))
          (t
           (loop-error "The combination of ~S and ~S is invalid" first-op second-op)))
    (setf *for-clauses*
          (nconc *for-clauses*
                 (list (make-for-clause :var var
                                        :init-form form1
                                        :update-form `(,step-op ,var
                                                                ,(or by-form-gsym
                                                                     by-form
                                                                     1))
                                        :while-form `(,while-op ,var
                                                                ,(or form2-gsym
                                                                     form2))))))))

(defun parse-for-as-equals-then (var)
  (let* ((init-form (next-exp))
         (update-form
           (if (and (symbolp (lookahead))
                    (string= (lookahead) :then))
               (progn
                 (next-exp)
                 (next-exp))
               init-form)))
    (setf *for-clauses*
          (nconc *for-clauses*
                 (list (make-for-clause :var var
                                        :init-form init-form
                                        :update-form update-form))))))

(defun parse-for-as-in-list (var)
  )

(defun parse-for-as-on-list (var)
  )

(defun parse-for-as-across (var)
  )

(defun parse-for-as-hash-or-package (var)
  )

(defun parse-for-as-clause ()
  (let ((var (next-exp)))
    (check-variable var)
    (type-spec)
    (let ((name (ensure-keyword (next-exp))))
      (ecase name
        ((:from :upfrom :downfrom)
         (parse-for-as-arithmetic var name))
        ((:=)
         (parse-for-as-equals-then var))
        ((:in)
         (parse-for-as-in-list var))
        ((:on)
         (parse-for-as-on-list var))
        ((:across)
         (parse-for-as-across var))
        ((:being)
         (parse-for-as-hash-or-package var))))))

(defun parse-unconditional ()
  (let ((forms (parse-compound-forms)))
    (setf *do-clauses* (nconc *do-clauses* forms))))

(defun parse-variable-clause ()
  (case (ensure-keyword (lookahead))
    ((:with)
     (next-exp)
     (parse-with-clause)
     t)
    ((:initially)
     (next-exp)
     (setf *initially-forms*
           (nconc *initially-forms* (parse-compound-forms))))
    ((:finally)
     (next-exp)
     (setf *finally-forms*
           (nconc *finally-forms* (parse-compound-forms))))
    ((:for :as)
     (next-exp)
     (parse-for-as-clause)
     t)
    (otherwise
     nil)))

(defun parse-main-clause ()
  (case (ensure-keyword (lookahead))
    ((:do :doing)
     (next-exp)
     (parse-unconditional)
     t)
    (otherwise
     nil)))

(defun parse-forms-aux (parse-fn)
  (do ()
      ((end-of-loop-p))
    (let ((x (lookahead)))
      (unless (symbolp x)
        (loop-error "unexpected token: ~S" x))
      (unless (funcall parse-fn)
        (return)))))

(defun parse-name-clause ()
  (when (eq (ensure-keyword (lookahead)) :named)
    (next-exp)
    (let ((name (next-exp)))
      (unless name
        (loop-error "Name clause must be a symbol (actual value: ~S)" name))
      (setq *named* name))))

(defun parse-variable-clauses ()
  (parse-forms-aux #'parse-variable-clause))

(defun parse-main-clauses ()
  (parse-forms-aux #'parse-main-clause))

(defun parse-loop (forms)
  (let ((*loop-exps* forms))
    (parse-name-clause)
    (parse-variable-clauses)
    (parse-main-clauses)
    (values *for-clauses*
            *do-clauses*)))

(defun expand-complex-loop (forms)
  (let ((*named* nil)
        (*initially-forms* '())
        (*finally-forms* '())
        (*with-clauses* '())
        (*temporary-variables* '())
        (*for-clauses* '())
        (*do-clauses* '()))
    (parse-loop forms)
    (let ((loop-start (gensym))
          (loop-end (gensym)))
      `(block ,*named*
         (let (,@*with-clauses*
               ,@(mapcar (lambda (for-clause)
                           (let ((var (for-clause-var for-clause))
                                 (init (for-clause-init-form for-clause)))
                             `(,var ,init)))
                         *for-clauses*)
               ,@*temporary-variables*)
           ,@*initially-forms*
           (tagbody
             ,loop-start
             ,@(mapcan (lambda (for-clause)
                         (let ((while-form (for-clause-while-form for-clause)))
                           (when while-form
                             (list `(unless ,while-form
                                      (go ,loop-end))))))
                *for-clauses*)
             ,@*do-clauses*
             ,@(mapcar (lambda (for-clause)
                         (let ((var (for-clause-var for-clause))
                               (update (for-clause-update-form for-clause)))
                           `(setq ,var ,update)))
                *for-clauses*)
             (go ,loop-start)
             ,loop-end))
         ,@*finally-forms*))))

(defun expand-loop (forms)
  (if (and forms (symbolp (car forms)))
      (expand-complex-loop forms)
      (let ((tag (gensym)))
        `(block nil
           (tagbody
             ,tag
             ,@forms
             (go ,tag))))))

#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defmacro loop (&body forms)
  (valtan-core/loop::expand-loop forms))
