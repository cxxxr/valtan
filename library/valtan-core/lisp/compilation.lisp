(in-package :common-lisp)

(defun eval (x)
  (multiple-value-bind (code constants) (compiler:compile-toplevel x t t)
    (ffi:js-eval code constants)))

(defun compiled-function-p (object)
  (functionp object))

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
         (expander (compiler::%macro-function symbol)))
    (when expander
      (lambda (form environment)
        (let ((compiler::*macroexpansion-environment* environment)
              (compiler::*macroexpansion-form* form))
          (apply expander (cdr form)))))))

(defun special-operator-p (symbol)
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  (not (null (member symbol
                     '(block catch eval-when flet function go if labels let let* load-time-value
                       locally macrolet multiple-value-call multiple-value-prog1 progn progv quote
                       return-from setq symbol-macrolet tagbody the throw unwind-protect)))))

(defmacro define-compiler-macro (name lambda-list &body body)
  (declare (ignore name lambda-list body))
  `',name)

(defun load (file)
  (with-open-file (in file)
    (loop :with eof := '#:eof
          :for form := (read in nil eof)
          :until (eq form eof)
          :do (eval form))))
