(in-package :compiler)

(defparameter *pass1-form-table* (make-hash-table))

(defvar *variable-env*)

(defmacro def-pass1-form (name lambda-list &body body)
  (let ((fn-name (intern (format nil "PASS1-~A" name))))
    `(progn
       (defun ,fn-name ,lambda-list ,@body)
       (setf (gethash ',name *pass1-form-table*) ',fn-name))))

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

(defun check-lambda-form (form)
  (check-args (rest form) 1 nil)
  (check-lambda-list (second form)))

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
  (let ((var (lookup symbol)))
    (if var
        (make-ir 'lref var)
        (make-ir 'gref symbol))))

(defun pass1-forms (forms)
  (mapcar #'pass1 forms))

(defun pass1-lambda (form)
  (check-lambda-form form)
  (let* ((args (rest form))
         (lambda-list (first args))
         (body (rest args))
         (vars (new-frame lambda-list)))
    (make-ir 'lambda
             vars
             (let ((*variable-env* (append vars *variable-env*)))
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
           (check-lambda-form form)
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
                 (var (lookup symbol))
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
                            (list (new-var (first b)) (pass1 (second b))))
                          bindings)))
    (make-ir 'let
             bindings
             (let ((*variable-env*
                     (append (mapcar #'first bindings)
                             *variable-env*)))
               (pass1-forms body)))))

(defun pass1-toplevel (form)
  (let ((*variable-env* '()))
    (make-ir 'progn (list (pass1 form)))))
