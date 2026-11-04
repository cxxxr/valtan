#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun !macroexpand-setf-place (place environment)
    (if compiler::*in-host-runtime*
        (compiler::!macroexpand-1 place environment)
        (cl:macroexpand-1 place environment)))
  (defun !get-setf-expansion (place &optional environment)
    (cl:multiple-value-bind (expansion expanded-p)
        (!macroexpand-setf-place place environment)
      (cl:when expanded-p
        (cl:return-from !get-setf-expansion
          (!get-setf-expansion expansion environment))))
    (flet ((gensyms (cl:list)
             (cl:mapcar (lambda (x) (declare (ignore x)) (cl:gensym)) cl:list)))
      (let ((setf-expander nil))
        (cl:cond
         ((symbolp place)
          (let ((store (cl:gensym)))
            (cl:values nil nil (cl:list store) `(setq ,place ,store) place)))
         ((cl:and (cl:consp place) (setq setf-expander (cl:get (cl:first place) 'setf-expander)))
          (cl:cond
           ((symbolp setf-expander)
            (let ((vars (gensyms (cl:rest place))) (store (cl:gensym)))
              (cl:values vars (cl:rest place) (cl:list store) `(,setf-expander ,@vars ,store)
                         `(,(cl:first place) ,@vars))))
           ((cl:consp setf-expander)
            (let ((vars (gensyms (cl:rest place)))
                  (store (cl:gensym))
                  (fn
                   (cl:eval
                    `(lambda ,(cl:first setf-expander) (lambda ,@(cl:rest setf-expander))))))
              (cl:values vars (cl:rest place) (cl:list store) (cl:funcall (cl:apply fn vars) store)
                         `(,(cl:first place) ,@vars))))
           ((functionp setf-expander) (cl:funcall setf-expander (cl:rest place) environment))))
         (t
          (cl:multiple-value-bind (expansion expanded-p)
              (!macroexpand-setf-place place environment)
            (if expanded-p
                (!get-setf-expansion expansion environment)
                (let ((newvar (cl:gensym))
                      (vars (gensyms (cl:cdr expansion)))
                      (vals (cl:cdr expansion)))
                  (cl:values vars vals (cl:list newvar)
                             `(cl:funcall (cl:fdefinition '(cl:setf ,(cl:car expansion))) ,newvar
                                          ,@vars)
                             `(,(cl:car expansion) ,@vars)))))))))))

(defun get-setf-expansion (place &optional environment)
  (!get-setf-expansion place environment))

(defmacro setf (&rest pairs cl:&environment environment)
  (labels ((setf-expand-1 (place value)
             (cl:multiple-value-bind (vars forms store cl:set access)
                 (!get-setf-expansion place environment)
               (declare (ignore access))
               (if (cl:and store (cl:null (cl:cdr store)))
                   `(let* ,(cl:mapcar #'cl:list (cl:append vars store)
                                     (cl:append forms (cl:list value)))
                      ,cl:set)
                   `(let* ,(cl:mapcar #'cl:list vars forms)
                      (multiple-value-bind ,store ,value
                        ,cl:set)))))
           (setf-expand (pairs)
             (cl:cond ((cl:endp pairs) nil)
                      ((cl:endp (cl:cdr pairs)) (cl:error "Odd number of args to SETF."))
                      (t
                       (cl:cons (setf-expand-1 (cl:first pairs) (cl:second pairs))
                                (setf-expand (cl:cddr pairs)))))))
    `(progn ,@(setf-expand pairs))))

(defmacro defsetf (access-fn &rest cl:rest)
  ;; TODO: documentation文字列
  ;; TODO: restが単一のシンボルか関数ではないときの処理
  (cl:unless (cl:symbolp access-fn)
    (cl:error 'cl:type-error :datum access-fn :expected-type 'cl:symbol))
  (cl:cond
   ((cl:and (cl:first cl:rest) (cl:or (symbolp (cl:first cl:rest)) (functionp (cl:first cl:rest))))
    (*:put access-fn 'setf-expander (cl:first cl:rest))
    `(progn (*:put ',access-fn 'setf-expander ',(cl:first cl:rest)) ',access-fn))
   (t (*:put access-fn 'setf-expander cl:rest)
    `(progn (*:put ',access-fn 'setf-expander ',cl:rest) ',access-fn))))

(defmacro define-setf-expander (access-fn lambda-list &body body)
  (cl:unless (symbolp access-fn)
    (cl:error "DEFINE-SETF-EXPANDER access-function name ~S is not a symbol." access-fn))
  ;; Extract &ENVIRONMENT from lambda-list since standard destructuring-bind doesn't support it
  ;; Handle dotted pairs like (a b . rest) properly
  (let ((clean-lambda-list nil)
        (env-var nil)
        (rest-var nil))
    (cl:do ((ll lambda-list (if (cl:consp ll) (cl:cdr ll) nil)))
           ((cl:atom ll)
            ;; Handle dotted pair: (a b . rest) -> ll is 'rest at the end
            (cl:when ll (setq rest-var ll)))
      (cl:cond
       ((eq (cl:car ll) 'cl:&environment)
        (cl:when (cl:and (cl:consp (cl:cdr ll)) (cl:cdr ll))
          (setq env-var (cl:cadr ll))
          (setq ll (cl:cdr ll))))  ; skip the variable (loop will advance past env-var)
       (t
        (setq clean-lambda-list (cl:cons (cl:car ll) clean-lambda-list)))))
    (setq clean-lambda-list (cl:nreverse clean-lambda-list))
    ;; Reconstruct dotted pair if present
    (cl:when rest-var
      (setq clean-lambda-list (cl:nconc clean-lambda-list rest-var)))
    (let ((g-rest (cl:gensym))
          (g-env (cl:gensym)))
      ;; Create a function that takes (args env) and binds appropriately
      (let ((inner-body (if env-var
                            `(let ((,env-var ,g-env))
                               (cl:destructuring-bind ,clean-lambda-list ,g-rest ,@body))
                            `(cl:destructuring-bind ,clean-lambda-list ,g-rest ,@body))))
        (setq inner-body `(block ,access-fn ,inner-body))
        (*:put access-fn 'setf-expander
                 (cl:eval `(lambda (,g-rest &optional ,g-env) ,inner-body)))
        `(progn
           (*:put ',access-fn 'setf-expander
                  (lambda (,g-rest &optional ,g-env) ,inner-body))
           ',access-fn)))))

(defmacro define-modify-macro
    (name lambda-list function &optional (cl:documentation nil documentation-p))
  (let ((update-form
          (cl:do ((cl:rest lambda-list (cl:cdr cl:rest))
                  (vars 'nil))
              ((cl:null cl:rest) `(cl:list ',function access-form ,@(cl:nreverse vars)))
            (cl:cond ((eq '&optional (cl:car cl:rest)))
                     ((eq '&rest (cl:car cl:rest))
                      (cl:return
                        `(cl:list* ',function access-form ,@(cl:nreverse vars) (cl:cadr cl:rest))))
                     ((symbolp (cl:car cl:rest)) (setq vars (cl:cons (cl:car cl:rest) vars)))
                     (t (setq vars (cl:cons (cl:caar cl:rest) vars)))))))
    (let ((reference (cl:gensym)) (environment (cl:gensym)))
      `(defmacro ,name (,reference ,@lambda-list cl:&environment ,environment)
         ,@(cl:when documentation-p `(,cl:documentation))
         (cl:multiple-value-bind (vars cl:values stores set-form access-form)
             (!get-setf-expansion ,reference ,environment)
           (cl:list 'let*
                    (cl:mapcar #'cl:list (cl:append vars stores)
                               (cl:append cl:values (cl:list ,update-form)))
                    set-form))))))

(defmacro psetf (&rest pairs cl:&environment environment)
  (cl:when (cl:oddp (cl:length pairs))
    (cl:error "Odd number of args to PSETF."))
  (if (cl:null pairs)
      nil
      (let ((bindings '())
            (set-forms '()))
        (cl:do* ((p pairs (cl:cddr p)))
                ((cl:null p))
          (let ((place (cl:first p))
                (value (cl:second p)))
            (cl:multiple-value-bind (vars forms store-vars set-form access)
                (!get-setf-expansion place environment)
              (declare (ignore access))
              ;; Add bindings for the place subforms
              (setq bindings (cl:nconc bindings (cl:mapcar #'cl:list vars forms)))
              ;; Add binding for the store variable to the value
              (setq bindings (cl:nconc bindings (cl:list (cl:list (cl:first store-vars) value))))
              ;; Collect the set form (use cons instead of push to avoid macro issues at runtime)
              (setq set-forms (cl:cons set-form set-forms)))))
        `(let* ,bindings
           ,@(cl:nreverse set-forms)
           nil))))

(define-modify-macro incf (&optional (n 1)) +)
(define-modify-macro decf (&optional (n 1)) -)

(define-setf-expander values (&rest places cl:&environment environment)
  (let ((all-vars nil) (all-forms nil) (all-stores nil)
        (all-setters nil) (all-accessors nil))
    (cl:dolist (place places)
      (cl:multiple-value-bind (vars forms stores setter accessor)
          (!get-setf-expansion place environment)
        (setq all-vars (cl:append all-vars vars (cl:cdr stores))
              all-forms (cl:append all-forms forms (cl:mapcar (lambda (x) (declare (ignore x)) nil)
                                                                           (cl:cdr stores)))
              all-stores (cl:append all-stores (cl:list (cl:car stores)))
              all-setters (cl:append all-setters (cl:list setter))
              all-accessors (cl:append all-accessors (cl:list accessor)))))
    (cl:values all-vars all-forms all-stores
               `(values ,@all-setters) `(values ,@all-accessors))))

(defmacro rotatef (&rest places cl:&environment environment)
  (let ((bindings nil) (value-bindings nil) (values-vars nil)
        (stores-list nil) (set-forms nil))
    (cl:dolist (place places)
      (cl:multiple-value-bind (vars forms stores set-form access)
          (!get-setf-expansion place environment)
        (let ((value-var (cl:gensym)))
          (setq bindings (cl:append bindings (cl:mapcar #'cl:list vars forms))
                value-bindings (cl:append value-bindings
                                         (cl:list (cl:list value-var `(multiple-value-list ,access))))
                values-vars (cl:append values-vars (cl:list value-var))
                stores-list (cl:append stores-list (cl:list stores))
                set-forms (cl:append set-forms (cl:list set-form))))))
    `(let* (,@bindings ,@value-bindings)
       ,@(cl:mapcar (lambda (stores value-var set-form)
                      `(multiple-value-bind ,stores (values-list ,value-var)
                         ,set-form))
                    stores-list
                    (cl:append (cl:cdr values-vars) (cl:list (cl:car values-vars)))
                    set-forms)
       nil)))
