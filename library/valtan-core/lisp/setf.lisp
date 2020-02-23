#-valtan(cl:in-package :valtan-core)
#+valtan(cl:in-package :common-lisp)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun !get-setf-expansion (place cl:&optional environment)
    (cl:flet ((gensyms (cl:list)
                (cl:mapcar (cl:lambda (x) (cl:declare (cl:ignore x)) (cl:gensym)) cl:list)))
      (cl:let ((setf-expander cl:nil))
        (cl:cond
          ((cl:symbolp place)
           (cl:let ((store (cl:gensym)))
             (cl:values cl:nil cl:nil (cl:list store) `(cl:setq ,place ,store) place)))
          ((cl:and (cl:consp place) (cl:setq setf-expander (cl:get (cl:first place) 'setf-expander)))
           (cl:cond
             ((cl:symbolp setf-expander)
              (cl:let ((vars (gensyms (cl:rest place))) (store (cl:gensym "STORE")))
                (cl:values vars (cl:rest place) (cl:list store) `(,setf-expander ,@vars ,store)
                           `(,(cl:first place) ,@vars))))
             ((cl:consp setf-expander)
              (cl:let ((vars (gensyms (cl:rest place)))
                       (store (cl:gensym "STORE"))
                       (fn
                         (cl:eval `(cl:lambda ,(cl:first setf-expander) (cl:lambda ,@(cl:rest setf-expander))))))
                (cl:values vars (cl:rest place) (cl:list store)
                           (cl:funcall (cl:apply fn vars) store) `(,(cl:first place) ,@vars))))
             ((cl:functionp setf-expander) (cl:funcall setf-expander (cl:rest place)))))
          ;; TODO: マクロはホスト側で管理しているので
          ;; コンパイラ内の情報を参照する必要があるはず
          ;; ((and (consp place) (symbolp (first place)) (macro-function (first place)))
          ;;  (get-setf-expansion (macroexpand place)))
          (cl:t
           (cl:multiple-value-bind (expansion expanded-p)
               (cl:macroexpand-1 place environment)
             (cl:if expanded-p
                    (!get-setf-expansion expansion environment)
                    (cl:let ((newvar (cl:gensym))
                             (vars (gensyms (cl:cdr expansion)))
                             (vals (cl:cdr expansion)))
                      (cl:values vars vals (cl:list newvar)
                                 `(cl:funcall (cl:fdefinition '(cl:setf ,(cl:car expansion))) ,newvar ,@vars)
                                 `(,(cl:car expansion) ,@vars)))))))))))

(cl:defmacro setf (cl:&rest pairs)
  (cl:labels ((setf-expand-1 (place value)
                (cl:multiple-value-bind (vars forms store cl:set access)
                    (!get-setf-expansion place)
                  (cl:declare (cl:ignore access))
                  `(cl:let* (,@(cl:mapcar #'cl:list (cl:append vars store)
                                          (cl:append forms (cl:list value))))
                     ,cl:set)))
              (setf-expand (pairs)
                (cl:cond ((cl:endp pairs) cl:nil)
                         ((cl:endp (cl:cdr pairs)) (cl:error "Odd number of args to SETF."))
                         (cl:t
                          (cl:cons (setf-expand-1 (cl:first pairs) (cl:second pairs))
                                   (setf-expand (cl:cddr pairs)))))))
    `(cl:progn ,@(setf-expand pairs))))

(cl:defmacro defsetf (access-fn cl:&rest cl:rest)
  ;; TODO: documentation文字列
  ;; TODO: restが単一のシンボルか関数ではないときの処理
  (cl:check-type access-fn cl:symbol)
  (cl:cond
    ((cl:and (cl:first cl:rest) (cl:or (cl:symbolp (cl:first cl:rest)) (cl:functionp (cl:first cl:rest))))
     (cl:setf (cl:get access-fn 'setf-expander) (cl:first cl:rest))
     `(cl:progn (system:%put ',access-fn 'setf-expander ',(cl:first cl:rest)) ',access-fn))
    (cl:t (cl:setf (cl:get access-fn 'setf-expander) cl:rest)
          `(cl:progn (system:%put ',access-fn 'setf-expander ',cl:rest) ',access-fn))))

(cl:defmacro define-setf-expander (access-fn lambda-list cl:&body body)
  (cl:unless (cl:symbolp access-fn) (cl:error "DEFINE-SETF-EXPANDER access-function name ~S is not a symbol." access-fn))
  (cl:let ((g-rest (cl:gensym)))
    (cl:setf (cl:get access-fn 'setf-expander)
             (cl:eval `(cl:lambda (,g-rest) (cl:destructuring-bind ,lambda-list ,g-rest ,@body))))
    `(cl:progn
       (system:%put ',access-fn 'setf-expander
                    (cl:lambda (,g-rest) (cl:destructuring-bind ,lambda-list ,g-rest ,@body)))
       ',access-fn)))

(cl:defmacro define-modify-macro
    (name lambda-list cl:function cl:&optional (documentation cl:nil documentation-p))
  (cl:let ((update-form
             (cl:do ((cl:rest lambda-list (cl:cdr cl:rest))
                     (vars 'cl:nil))
                 ((cl:null cl:rest) `(cl:list ',cl:function access-form ,@(cl:nreverse vars)))
               (cl:cond ((cl:eq 'cl:&optional (cl:car cl:rest)))
                        ((cl:eq 'cl:&rest (cl:car cl:rest))
                         (cl:return
                           `(cl:list* ',cl:function access-form ,@(cl:nreverse vars) (cl:cadr cl:rest))))
                        ((cl:symbolp (cl:car cl:rest)) (cl:push (cl:car cl:rest) vars))
                        (cl:t (cl:push (cl:caar cl:rest) vars))))))
    (cl:let ((reference (cl:gensym "REFERENCE")))
      `(cl:defmacro ,name (,reference ,@lambda-list)
         ,(cl:when documentation-p `(,documentation))
         (cl:multiple-value-bind (vars cl:values stores set-form access-form)
             (!get-setf-expansion ,reference)
           (cl:list 'cl:let*
                    (cl:mapcar #'cl:list (cl:append vars stores)
                               (cl:append cl:values (cl:list ,update-form)))
                    set-form))))))

(cl:define-modify-macro incf (cl:&optional (n 1)) cl:+)
(cl:define-modify-macro decf (cl:&optional (n 1)) cl:-)
