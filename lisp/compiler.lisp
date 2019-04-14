(defpackage :compiler
  (:use :cl)
  (:export :compile-stdin))
(in-package :compiler)

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

(defun comp1-const (x)
  (make-ir 'const x))

(defun comp1-quote (args)
  (check-args args 1)
  (comp1-const (first args)))

(defun comp1-refvar (symbol)
  (let ((var (lookup symbol)))
    (if var
        (make-ir 'lref var)
        (make-ir 'gref symbol))))

(defun comp1-setq (args)
  (check-args args 2)
  (let ((symbol (first args))
        (value (second args)))
    (assert (symbolp symbol))
    (let ((var (lookup symbol)))
      (if var
          (make-ir 'lset var (comp1 value))
          (make-ir 'gset symbol (comp1 value))))))

(defun comp1-if (args)
  (check-args args 2 3)
  (make-ir 'if
           (comp1 (first args))
           (comp1 (second args))
           (comp1 (third args))))

(defun comp1-forms (forms)
  (mapcar #'comp1 forms))

(defun comp1-progn (args)
  (make-ir 'progn (comp1-forms args)))

(defun comp1-lambda (args)
  (check-args args 1 nil)
  (let ((lambda-list (first args))
        (body (rest args)))
    (check-lambda-list lambda-list)
    (let ((vars (new-frame lambda-list)))
      (make-ir 'lambda
               vars
               (let ((*variable-env* (append vars *variable-env*)))
                 (comp1-forms body))))))

(defun comp1-let (args)
  (check-args args 1 nil)
  (let ((bindings (first args))
        (body (rest args)))
    (assert (consp bindings))
    (let ((bindings (mapcar (lambda (b)
                              (assert (consp b))
                              (assert (<= 1 (length b) 2))
                              (check-variable (first b))
                              (list (new-var (first b)) (comp1 (second b))))
                            bindings)))
      (make-ir 'let
               bindings
               (let ((*variable-env*
                       (append (mapcar #'first bindings)
                               *variable-env*)))
                 (comp1-forms body))))))

(defun comp1-call (form)
  (assert (symbolp (first form)))
  (make-ir 'call (first form) (mapcar #'comp1 (rest form))))

(defun comp1 (form)
  (cond ((null form)
         (comp1-const nil))
        ((symbolp form)
         (comp1-refvar form))
        ((atom form)
         (comp1-const form))
        (t
         (let ((args (rest form)))
           (case (first form)
             ((quote)
              (comp1-quote args))
             ((setq)
              (comp1-setq args))
             ((if)
              (comp1-if args))
             ((progn)
              (comp1-progn args))
             ((lambda)
              (comp1-lambda args))
             ((let)
              (comp1-let args))
             (otherwise
              (comp1-call form)))))))

(defun comp1-top (form)
  (let ((*variable-env* '()))
    (comp1 form)))

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

(defparameter *trans-table* (make-hash-table))

(defun const-to-js-literal (value)
  (if (null value)
      "lisp.nilValue"
      (princ-to-string value)))

(defun symbol-to-js-identier (symbol)
  (flet ((f (c)
           (or (cdr (assoc c *character-map*))
               (string c))))
    (with-output-to-string (out)
      (map nil (lambda (c)
                 (write-string (f c) out))
           (string symbol)))))

(defun js-call (name &rest args)
  (format nil "~A(~{~A~^,~})" name args))

(defun comp2-forms (forms return-value-p)
  (do ((ir* forms (rest ir*)))
      ((null (rest ir*))
       (when return-value-p
         (princ "return "))
       (comp2 (first ir*) return-value-p)
       (format t ";~%"))
    (comp2 (first ir*) nil)
    (format t ";~%")))

(defun comp2 (ir &optional return-value-p)
  (funcall (gethash (ir-op ir) *trans-table*)
           ir
           return-value-p))

(defmacro def-trans (op (ir return-value-p) &body body)
  (let ((name (gensym)))
    `(progn
       (defun ,name (,ir ,return-value-p)
         (declare (ignorable ir return-value-p))
         ,@body)
       ,@(mapcar (lambda (op)
                   `(setf (gethash ',op *trans-table*) ',name))
                 (if (consp op) op (list op))))))

(def-trans const (ir return-value-p)
  (princ (const-to-js-literal (ir-arg1 ir))))

(def-trans lref (ir return-value-p)
  (princ (symbol-to-js-identier (ir-arg1 ir))))

(def-trans gref (ir return-value-p)
  (princ (js-call "lisp.global_variable" (format nil "\"~A\"" (ir-arg1 ir)))))

(def-trans (lset gset) (ir return-value-p)
  (when return-value-p
    (write-string "("))
  (cond ((eq 'lset (ir-op ir))
         (format t "~A = " (symbol-to-js-identier (ir-arg1 ir)))
         (comp2 (ir-arg2 ir) t))
        (t
         (format t "lisp.set_global_value(\"~A\", " (ir-arg1 ir))
         (comp2 (ir-arg2 ir) t)
         (write-string ")")))
  (when return-value-p
    (write-string ")")))

(def-trans if (ir return-value-p)
  (when return-value-p
    (format t "(function() {~%"))
  (write-string "if (")
  (comp2 (ir-arg1 ir) t)
  (format t " === lisp.nilValue) {~%")
  (comp2 (ir-arg2 ir) return-value-p)
  (format t ";~%")
  (format t "} else {~%")
  (comp2 (ir-arg3 ir) return-value-p)
  (format t "}")
  (if return-value-p
      (write-string "})()")
      (terpri)))

(def-trans progn (ir return-value-p)
  (when return-value-p
    (format t "(function() {~%"))
  (comp2-forms (ir-arg1 ir) return-value-p)
  (when return-value-p
    (format t "})()")))

(def-trans lambda (ir return-value-p)
  (format t "(function(~{~A~^, ~}) {~%" (ir-arg1 ir))
  (comp2-forms (ir-arg2 ir) t)
  (format t "})"))

(def-trans let (ir return-value-p)
  (if return-value-p
      (format t "(function() {~%")
      (format t "{~%"))
  (dolist (binding (ir-arg1 ir))
    (format t "let ~A = " (symbol-to-js-identier (first binding)))
    (comp2 (second binding) t)
    (format t ";~%"))
  (comp2-forms (ir-arg2 ir) return-value-p)
  (if return-value-p
      (format t "})()")
      (format t "}~%")))

(def-trans call (ir return-value-p)
  (format t "lisp.call_function(\"~A\"" (ir-arg1 ir))
  (dolist (arg (ir-arg2 ir))
    (write-string ", ")
    (comp2 arg t))
  (write-string ")"))

(defun compile-toplevel (form)
  (comp2 (comp1-top form) nil))

(defun compile-stdin ()
  (write-line "import * as lisp from 'lisp';")
  (loop :with eof-value := '#:eof-value
        :for form := (read *standard-input* nil eof-value)
        :until (eq form eof-value)
        :do (compile-toplevel form)))
