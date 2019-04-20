(in-package :compiler)

(defparameter *pass1-form-table* (make-hash-table))

(defvar *lexenv*)

(defstruct binding
  name
  type
  value)

(defun make-variable-binding (symbol)
  (make-binding :name symbol :type :variable :value symbol))

(defmacro def-pass1-form (name lambda-list &body body)
  (let ((fn-name (intern (format nil "PASS1-~A" name))))
    `(progn
       (defun ,fn-name ,lambda-list ,@body)
       (setf (gethash ',name *pass1-form-table*) ',fn-name))))

(defun get-macro (symbol)
  (get symbol 'macro))

(defun set-macro (symbol form)
  (setf (get symbol 'macro) form))

(defun lookup (symbol type)
  (dolist (binding *lexenv*)
    (when (and (eq type (binding-type binding))
               (eq symbol (binding-name binding)))
      (return (binding-value binding)))))

(defun extend-lexenv (bindings lexenv)
  (append bindings lexenv))

(defun check-args (args min &optional (max min))
  (if max
      (assert (<= min (length args) max))
      (assert (<= min (length args)))))

(defun variable-symbol-p (x)
  (and (symbolp x)
       (not (null x))
       (not (keywordp x))))

(defun check-variable (x)
  (assert (variable-symbol-p x)))

(defun check-proper-list (x)
  (assert (and (listp x)
               (null (cdr (last x))))))

(defun check-lambda-list (lambda-list)
  (do ((x* lambda-list (rest x*)))
      ((null x*))
    (let ((x (first x*)))
      (assert (not (member x (rest x*))))
      (check-variable x))))

(defun check-lambda-form (form)
  (check-args (rest form) 1 nil)
  (check-lambda-list (second form)))

(defun check-duplicate-var (vars)
  (do ((var* vars (rest var*)))
      ((null var*))
    (when (member (first var*) (rest var*))
      (error "error"))))

(defun parse-lambda-list (lambda-list)
  (let ((vars '())
        (rest-var nil)
        (in-optional nil)
        (optionals '())
        (*lexenv* *lexenv*))
    (do ((arg* lambda-list (cdr arg*)))
        ((null arg*))
      (let ((arg (car arg*)))
        (cond (rest-var
               (error "error"))
              ((eq arg '&rest)
               (unless (consp (cdr arg*))
                 (error "error"))
               (check-variable (second arg*))
               (let ((binding (make-variable-binding (second arg*))))
                 (setf rest-var (binding-value binding))
                 (push binding *lexenv*))
               (setq arg* (cdr arg*)))
              ((eq arg '&optional)
               (when in-optional
                 (error "error"))
               (setq in-optional t))
              (in-optional
               (cond ((symbolp arg)
                      (check-variable arg)
                      (let ((binding (make-variable-binding arg)))
                        (push (list (binding-value binding) (pass1 nil) nil)
                              optionals)
                        (push binding *lexenv*)))
                     ((consp arg)
                      (check-proper-list arg)
                      (let ((len (length arg)))
                        (case len
                          ((1 2 3)
                           (when (= len 3)
                             (check-variable (third arg))
                             (when (eq (first arg) (third arg))
                               (error "error")))
                           (let ((binding1 (make-variable-binding (first arg)))
                                 (binding2 (make-variable-binding (third arg))))
                             (push (list (binding-value binding1)
                                         (pass1 (second arg))
                                         (binding-value binding2))
                                   optionals)
                             (push binding1 *lexenv*)
                             (when (= len 3)
                               (push binding2 *lexenv*))))
                          (t
                           (error "error")))))
                     (t
                      (error "error"))))
              (t
               (let ((binding (make-variable-binding arg)))
                 (push binding *lexenv*)
                 (push (binding-value binding) vars))))))
    (setq vars (nreverse vars)
          optionals (nreverse  optionals))
    (let* ((min (length vars))
           (parsed-lambda-list
             (make-parsed-lambda-list
              :vars vars
              :optionals optionals
              :rest-var rest-var
              :min (length vars)
              :max (cond (rest-var nil)
                         (optionals (+ min (length optionals)))
                         (t min)))))
      (let ((all-vars (collect-variables parsed-lambda-list)))
        (check-duplicate-var all-vars))
      (values parsed-lambda-list
              *lexenv*))))

(defun get-transform (symbol)
  (get symbol 'transform))

(defun transform-symbol-p (symbol)
  (get-transform symbol))

(defmacro def-transform (name lambda-list &body body)
  `(setf (get ',name 'transform)
         (lambda ,lambda-list ,@body)))

(def-transform defun (name lambda-list &rest body)
  `(system::fset ',name (lambda ,lambda-list ,@body)))

(def-transform defmacro (name lambda-list &rest body)
  (set-macro name (eval `(lambda ,lambda-list ,@body)))
  `(system::add-global-macro ',name (lambda ,lambda-list ,@body)))

(def-transform lambda (args &rest body)
  `(function (lambda ,args ,@body)))

(defun expand-quasiquote (x)
  (cond ((atom x)
         (list 'quote x))
        ((eq 'system::unquote (first x))
         (check-args x 2)
         (second x))
        ((and (consp (first x))
              (eq (first (first x)) 'system::unquote-splicing))
         (check-args (first x) 2)
         (list 'append (second (first x)) (rest x)))
        (t
         (list 'cons
               (expand-quasiquote (first x))
               (expand-quasiquote (rest x))))))

(def-transform system::quasiquote (x)
  (expand-quasiquote x))

(defun pass1-const (x)
  (make-ir 'const x))

(defun pass1-refvar (symbol)
  (let ((var (lookup symbol :variable)))
    (if var
        (make-ir 'lref var)
        (make-ir 'gref symbol))))

(defun pass1-forms (forms)
  (mapcar #'pass1 forms))

(defun pass1-lambda (form)
  (unless (eq (first form) 'lambda)
    (error "error"))
  (check-args (rest form) 1 nil)
  (multiple-value-bind (parsed-lambda-list *lexenv*)
      (parse-lambda-list (second form))
    (let ((body (or (cddr form) '(progn))))
      (make-ir 'lambda
               parsed-lambda-list
               (pass1-forms body)))))

(defun %macroexpand-1 (form)
  (cond ((symbolp form)
         (values form nil))
        ((and (consp form) (symbolp (first form)))
         (let ((fn (get-macro (first form))))
           (if fn
               (values (apply fn (rest form)) t)
               (values form nil))))
        (t
         (values form nil))))

(defun pass1-call-symbol (symbol args)
  (if (transform-symbol-p symbol)
      (pass1 (apply (get-transform symbol) args))
      (make-ir 'call symbol (mapcar #'pass1 args))))

(defun pass1-call (form)
  (let ((fn (first form))
        (args (rest form)))
    (cond ((symbolp fn)
           (pass1-call-symbol fn args))
          ((consp fn)
           (unless (eq 'lambda (first fn))
             (error "error"))
           (pass1 (list* 'funcall fn args)))
          (t
           (error "invalid form: ~S" form)))))

(defun pass1 (form)
  (multiple-value-bind (form expanded-p)
      (%macroexpand-1 form)
    (cond (expanded-p
           (pass1 form))
          ((null form)
           (pass1-const nil))
          ((symbolp form)
           (pass1-refvar form))
          ((atom form)
           (pass1-const form))
          (t
           (let ((fn (gethash (first form) *pass1-form-table*)))
             (if fn
                 (apply fn (rest form))
                 (pass1-call form)))))))

(def-pass1-form quote (x)
  (pass1-const x))

(def-pass1-form setq (&rest args)
  (if (not (evenp (length args)))
      (error "error")
      (let ((forms '()))
        (do ((args args (rest (rest args))))
            ((null args))
          (assert (symbolp (first args)))
          (let* ((symbol (first args))
                 (var (lookup symbol :variable))
                 (value (pass1 (second args))))
            (push (if var
                      (make-ir 'lset var value)
                      (make-ir 'gset symbol value))
                  forms)))
        (make-ir 'progn (nreverse forms)))))

(def-pass1-form if (test then &optional else)
  (make-ir 'if
           (pass1 test)
           (pass1 then)
           (pass1 else)))

(def-pass1-form progn (&rest forms)
  (make-ir 'progn (pass1-forms forms)))

(def-pass1-form function (thing)
  (cond
    ((eq (car thing) 'lambda)
     (pass1-lambda thing))
    (t
     (error "error"))))

(def-pass1-form let (bindings &rest body)
  (assert (consp bindings))
  (let ((bindings (mapcar (lambda (b)
                            (assert (consp b))
                            (assert (<= 1 (length b) 2))
                            (check-variable (first b))
                            (list (first b)
                                  (pass1 (second b))))
                          bindings)))
    (make-ir 'let
             bindings
             (let ((*lexenv*
                     (extend-lexenv (mapcar (lambda (b)
                                              (make-variable-binding (first b)))
                                            bindings)
                                    *lexenv*)))
               (pass1-forms body)))))

(defun pass1-toplevel (form)
  (let ((*lexenv* '()))
    (make-ir 'progn (list (pass1 form)))))
