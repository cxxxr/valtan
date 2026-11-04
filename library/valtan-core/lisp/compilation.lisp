(in-package :common-lisp)

(defun eval (x)
  (let ((compiler::*file-compilation-p* nil))
    (multiple-value-bind (code constants) (compiler:compile-toplevel x t t)
      (ffi:js-eval code constants))))

(defun compiled-function-p (object)
  (functionp object))

(defun function-lambda-expression (function)
  (unless (functionp function) (type-error function 'function))
  ;; Source and closure environment information need not be retained. A true
  ;; closure flag is conservative when the lexical environment is unavailable.
  (let ((name (ffi:ref function "lisp_symbol")))
    (values nil t (if (symbolp name) name nil))))

(defun constantp (form &optional environment)
  (setq form (macroexpand form environment))
  (cond ((symbolp form)
         (not (null (or (null form) (eq form t) (keywordp form)
                        (compiler::constant-p form)))))
        ((consp form)
         (and (eq (car form) 'quote) (consp (cdr form)) (null (cddr form))))
        (t t)))

(defun compile (name &optional (definition nil definition-p))
  (unless (or (symbolp name)
              (and (consp name) (eq (car name) 'setf)
                   (consp (cdr name)) (symbolp (second name))
                   (null (cddr name))))
    (type-error name '(or symbol (cons (eql setf) (cons symbol null)))))
  (let* ((macro-p (and name (symbolp name)
                       (compiler::get-macro name)))
         (source (if definition-p definition
                     (if macro-p macro-p (fdefinition name))))
         (function (cond ((functionp source) source)
                         ((and (consp source) (eq (car source) 'lambda))
                          (eval (list 'function source)))
                         (t (type-error source '(or function cons))))))
    (when name
      (if macro-p
          (setf (compiler::get-macro name) function)
          (setf (fdefinition name) function)))
    (values (or name function) nil nil)))

(defun proclaim (declaration)
  (unless (and (consp declaration) (proper-list-p declaration))
    (error 'program-error))
  (compiler::pre-process-declaration-specifier (list declaration))
  (when (eq (car declaration) 'special)
    (dolist (name (cdr declaration))
      (setf (compiler::special-p name) t)))
  nil)

(defun macroexpand-1 (form &optional environment)
  (compiler::!macroexpand-1 form environment))

(defun macroexpand (form &optional environment)
  (let ((expanded-any-p nil))
    (do () (nil)
      (multiple-value-bind (expansion expanded-p)
          (macroexpand-1 form environment)
        (unless expanded-p (return (values form expanded-any-p)))
        (setq form expansion expanded-any-p t)))))

(defun macro-function (symbol &optional environment)
  (unless (symbolp symbol) (type-error symbol 'symbol))
  (let* ((compiler::*lexenv* environment)
         (expander (or (compiler::%macro-function symbol)
                       (and (not (special-operator-p symbol))
                            (not (compiler::lookup symbol '(:function)))
                            (compiler::get-transform symbol)))))
    (when (or expander (member symbol '(declaim in-package step)))
      (lambda (form environment)
        (let ((compiler::*macroexpansion-environment* environment)
              (compiler::*macroexpansion-form* form))
          (if expander
              (apply expander (cdr form))
              (case symbol
                (declaim `(eval-when (:compile-toplevel :load-toplevel :execute)
                            ,@(mapcar (lambda (spec) `(proclaim ',spec)) (cdr form))))
                (in-package `(setq *package* (or (find-package ',(second form))
                                                 (error 'package-error :package ',(second form)))))
                (step (second form)))))))))

(defun special-operator-p (symbol)
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  (not (null (member symbol
                     '(block catch eval-when flet function go if labels let let* load-time-value
                       locally macrolet multiple-value-call multiple-value-prog1 progn progv quote
                       return-from setq symbol-macrolet tagbody the throw unwind-protect)))))

(dolist (name '(block catch eval-when flet function go if labels let let* load-time-value
               locally macrolet multiple-value-call multiple-value-prog1 progn progv quote
               return-from setq symbol-macrolet tagbody the throw unwind-protect))
  (system:set-compiler-property name 'special-operator t))

(defun (setf macro-function) (function symbol &optional environment)
  (unless (symbolp symbol) (type-error symbol 'symbol))
  (when environment (error "A global macro definition requires a null environment"))
  (unless (or (null function) (functionp function)) (type-error function '(or null function)))
  (setf (compiler::get-macro symbol)
        (when function
          (lambda (&rest operands)
            (funcall function (cons symbol operands) compiler::*macroexpansion-environment*))))
  function)

(defvar *compiler-macro-functions* (make-hash-table :test 'equal))

(defun compiler-macro-function (name &optional environment)
  (declare (ignore environment))
  (gethash name *compiler-macro-functions*))

(defun (setf compiler-macro-function) (function name &optional environment)
  (when environment (error "A global compiler macro requires a null environment"))
  (unless (or (null function) (functionp function)) (type-error function '(or null function)))
  (setf (gethash name *compiler-macro-functions*) function))

(defmacro define-compiler-macro (name lambda-list &body body)
  (let ((form (gensym)) (environment (gensym)))
    `(progn
       (setf (compiler-macro-function ',name)
             (lambda (,form ,environment)
               (let ((compiler::*macroexpansion-form* ,form)
                     (compiler::*macroexpansion-environment* ,environment))
                 (apply ,(compiler::macro-expander-lambda
                          (if (consp name) (second name) name) lambda-list body)
                        (cdr ,form)))))
       ',name)))

(defmacro step (form) form)

(defun load (file)
  (with-open-file (in file)
    (loop :with eof := '#:eof
          :for form := (read in nil eof)
          :until (eq form eof)
          :do (eval form))))
