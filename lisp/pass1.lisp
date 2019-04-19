(in-package :compiler)

(defvar *variable-env*)

(defun get-macro (symbol)
  (get symbol 'macro))

(defun set-macro (symbol form)
  (setf (get symbol 'macro) form))

(defun lookup (symbol)
  (find symbol *variable-env*))

(defun new-var (symbol)
  symbol)

(defun new-frame (symbols)
  (mapcar #'new-var symbols))

(defun check-args (args min &optional (max min))
  (if max
      (assert (<= min (length args) max))
      (assert (<= min (length args)))))

(defun check-variable (x)
  (and (symbolp x)
       (not (null x))))

(defun check-lambda-list (lambda-list)
  (do ((x* lambda-list (rest x*)))
      ((null x*))
    (let ((x (first x*)))
      (assert (not (member x (rest x*))))
      (check-variable x))))

(defun check-lambda-form (args)
  (check-args args 1 nil)
  (check-lambda-list (first args)))

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

(defun pass1-quote (args)
  (check-args args 1)
  (pass1-const (first args)))

(defun pass1-refvar (symbol)
  (let ((var (lookup symbol)))
    (if var
        (make-ir 'lref var)
        (make-ir 'gref symbol))))

(defun pass1-setq (args)
  (check-args args 2)
  (let ((symbol (first args))
        (value (second args)))
    (assert (symbolp symbol))
    (let ((var (lookup symbol)))
      (if var
          (make-ir 'lset var (pass1 value))
          (make-ir 'gset symbol (pass1 value))))))

(defun pass1-if (args)
  (check-args args 2 3)
  (make-ir 'if
           (pass1 (first args))
           (pass1 (second args))
           (pass1 (third args))))

(defun pass1-forms (forms)
  (mapcar #'pass1 forms))

(defun pass1-progn (args)
  (make-ir 'progn (pass1-forms args)))

(defun pass1-lambda (args)
  (check-lambda-form args)
  (let ((lambda-list (first args))
        (body (rest args)))
    (let ((vars (new-frame lambda-list)))
      (make-ir 'lambda
               vars
               (let ((*variable-env* (append vars *variable-env*)))
                 (pass1-forms body))))))

(defun pass1-let (args)
  (check-args args 1 nil)
  (let ((bindings (first args))
        (body (rest args)))
    (assert (consp bindings))
    (let ((bindings (mapcar (lambda (b)
                              (assert (consp b))
                              (assert (<= 1 (length b) 2))
                              (check-variable (first b))
                              (list (new-var (first b)) (pass1 (second b))))
                            bindings)))
      (make-ir 'let
               bindings
               (let ((*variable-env*
                       (append (mapcar #'first bindings)
                               *variable-env*)))
                 (pass1-forms body))))))

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

(defun lambda-to-let (form)
  ;; FIXME: lambda-listのシンボルと引数の数が合っているか確認していない
  (let ((lambda-form (first form)))
    `(let ,(mapcar #'list (second lambda-form) (rest form))
       ,@(rest (rest lambda-form)))))

(defun pass1-call (form)
  (let ((fn (first form))
        (args (rest form)))
    (cond ((symbolp fn)
           (pass1-call-symbol fn args))
          ((consp fn)
           (check-lambda-form args)
           (pass1 (lambda-to-let form)))
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
           (let ((args (rest form)))
             (case (first form)
               ((quote)
                (pass1-quote args))
               ((setq)
                (pass1-setq args))
               ((if)
                (pass1-if args))
               ((progn)
                (pass1-progn args))
               ((lambda)
                (pass1-lambda args))
               ((let)
                (pass1-let args))
               (otherwise
                (pass1-call form))))))))

(defun pass1-toplevel (form)
  (let ((*variable-env* '()))
    (make-ir 'progn (list (pass1 form)))))
