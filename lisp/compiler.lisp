(defpackage :compiler
  (:use :cl)
  (:export :compile-stdin))
(in-package :compiler)

(defvar *literal-symbols*)
(defvar *variable-env*)

(defun make-ir (op &rest args)
  (ecase (length args)
    (1 (vector op (first args)))
    (2 (vector op (first args) (second args)))
    (3 (vector op (first args) (second args) (third args)))))

(defun ir-op (ir) (aref ir 0))
(defun ir-arg1 (ir) (aref ir 1))
(defun ir-arg2 (ir) (aref ir 2))
(defun ir-arg3 (ir) (aref ir 3))

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

(defparameter *character-map*
  '((#\! . "BANG")       
    (#\" . "QUOTATION")  
    (#\# . "HASH")       
    (#\$ . "DOLLAR")     
    (#\% . "PERCENT")    
    (#\& . "AMPERSAND")  
    (#\' . "QUOTE")      
    (#\( . "LPAREN")     
    (#\) . "RPAREN")     
    (#\* . "STAR")       
    (#\+ . "PLUS")       
    (#\, . "COMMA")      
    (#\- . "_")          
    (#\. . "DOT")        
    (#\/ . "SLASH")      
    (#\: . "COLON")      
    (#\; . "SEMICOLON")  
    (#\< . "LESS")       
    (#\= . "EQUAL")      
    (#\> . "GREATER")    
    (#\? . "QUESTION")   
    (#\space . "SPACE")  
    (#\@ . "AT")         
    (#\[ . "LBRACKET")   
    (#\\ . "BACKSLASH")  
    (#\] . "RBRACKET")   
    (#\^ . "CARET")      
    (#\_ . "__")         
    (#\` . "BACKQUOTE")  
    (#\{ . "LBRACE")     
    (#\| . "PIPE")       
    (#\} . "RBRACE")     
    (#\~ . "TILDE")
    (#\newline . "NEWLINE")
    (#\return . "RETURN")
    (#\backspace . "BACK")
    (#\page . "PAGE")
    (#\tab . "TAB")))

(defparameter *emitter-table* (make-hash-table))

(defun symbol-to-js-identier (symbol)
  (flet ((f (c)
           (or (cdr (assoc c *character-map*))
               (string c))))
    (with-output-to-string (out)
      (map nil (lambda (c)
                 (write-string (f c) out))
           (string symbol)))))

(defun const-to-js-literal (value)
  (typecase value
    (null "lisp.nilValue")
    (symbol
     (or (gethash value *literal-symbols*)
         (setf (gethash value *literal-symbols*)
               (symbol-to-js-identier value))))
    (otherwise (princ-to-string value))))

(defun js-call (name &rest args)
  (format nil "~A(~{~A~^,~})" name args))

(defun pass2-form (form return-value-p)
  (when return-value-p
    (princ "return "))
  (pass2 form return-value-p)
  (format t ";~%"))

(defun pass2-forms (forms return-value-p)
  (do ((ir* forms (rest ir*)))
      ((null (rest ir*))
       (pass2-form (first ir*) return-value-p))
    (pass2 (first ir*) nil)
    (format t ";~%")))

(defmacro def-emit (op (ir return-value-p) &body body)
  (let ((name (gensym)))
    `(progn
       (defun ,name (,ir ,return-value-p)
         (declare (ignorable ir return-value-p))
         ,@body)
       ,@(mapcar (lambda (op)
                   `(setf (gethash ',op *emitter-table*) ',name))
                 (if (consp op) op (list op))))))

(def-emit const (ir return-value-p)
  (princ (const-to-js-literal (ir-arg1 ir))))

(def-emit lref (ir return-value-p)
  (princ (symbol-to-js-identier (ir-arg1 ir))))

(def-emit gref (ir return-value-p)
  (princ (js-call "lisp.global_variable" (format nil "\"~A\"" (ir-arg1 ir)))))

(def-emit (lset gset) (ir return-value-p)
  (when return-value-p
    (write-string "("))
  (cond ((eq 'lset (ir-op ir))
         (format t "~A = " (symbol-to-js-identier (ir-arg1 ir)))
         (pass2 (ir-arg2 ir) t))
        (t
         (format t "lisp.set_global_value(\"~A\", " (ir-arg1 ir))
         (pass2 (ir-arg2 ir) t)
         (write-string ")")))
  (when return-value-p
    (write-string ")")))

(def-emit if (ir return-value-p)
  (when return-value-p
    (format t "(function() {~%"))
  (write-string "if (")
  (pass2 (ir-arg1 ir) t)
  (format t " !== lisp.nilValue) {~%")
  (pass2-form (ir-arg2 ir) return-value-p)
  (format t "} else {~%")
  (pass2-form (ir-arg3 ir) return-value-p)
  (format t "}")
  (if return-value-p
      (write-string "})()")
      (terpri)))

(def-emit progn (ir return-value-p)
  (when return-value-p
    (format t "(function() {~%"))
  (pass2-forms (ir-arg1 ir) return-value-p)
  (when return-value-p
    (format t "})()")))

(def-emit lambda (ir return-value-p)
  (format t "(function(~{~A~^, ~}) {~%" (ir-arg1 ir))
  (pass2-forms (ir-arg2 ir) t)
  (format t "})"))

(def-emit let (ir return-value-p)
  (if return-value-p
      (format t "(function() {~%")
      (format t "{~%"))
  (dolist (binding (ir-arg1 ir))
    (format t "let ~A = " (symbol-to-js-identier (first binding)))
    (pass2 (second binding) t)
    (format t ";~%"))
  (pass2-forms (ir-arg2 ir) return-value-p)
  (if return-value-p
      (format t "})()")
      (format t "}~%")))

(def-emit call (ir return-value-p)
  (let ((symbol (ir-arg1 ir)))
    (format t "lisp.call_function(~S, ~S"
            (package-name (symbol-package symbol))
            (symbol-name symbol))
    (dolist (arg (ir-arg2 ir))
      (write-string ", ")
      (pass2 arg t))
    (write-string ")")))

(defun pass2 (ir return-value-p)
  (funcall (gethash (ir-op ir) *emitter-table*)
           ir
           return-value-p))

(defun pass2-toplevel (ir)
  (pass2 ir nil))

(defun compile-toplevel (form)
  (let ((*literal-symbols* (make-hash-table)))
    (let ((output
            (with-output-to-string (*standard-output*)
              (pass2-toplevel (pass1-toplevel form)))))
      (maphash (lambda (symbol ident)
                 (format t "~A = intern('~A');~%" ident symbol))
               *literal-symbols*)
      (write-string output)))
  (values))

(defun compile-stdin ()
  (write-line "import * as lisp from 'lisp';")
  (loop :with eof-value := '#:eof-value
        :for form := (let ((*package* (find-package "CL-USER")))
                       (read *standard-input* nil eof-value))
        :until (eq form eof-value)
        :do (compile-toplevel form)))
