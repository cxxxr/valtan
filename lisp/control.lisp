(in-package :common-lisp)

(defmacro cond (&rest clauses)
  (if (null clauses)
      nil
      (let ((clause (first clauses)))
        `(if ,(first clause)
             ,(if (null (rest clause))
                  t
                  `(progn ,@(rest clause)))
             (cond ,@(rest clauses))))))

#|
(defun gensyms (list)
  (mapcar (lambda (x)
            (declare (ignore x))
            (gensym))
          list))

;; 内部でevalを使っていて今はターゲット側で処理できないのでコメントアウト
(defun get-setf-expansion (place &optional environment)
  (declare (ignore environment))
  (let ((setf-expander nil))
    (cond ((and (consp place)
                (setq setf-expander (get (first place) 'setf-expander)))
           (if (symbolp setf-expander)
               (let ((vars (gensyms (rest place)))
                     (store (gensym "STORE")))
                 (values vars
                         (rest place)
                         (list store)
                         `(,setf-expander ,@vars ,store)
                         `(,(first place) ,@vars)))
               (let ((vars (gensyms (rest place)))
                     (store (gensym "STORE"))
                     (fn (eval `(lambda ,(first setf-expander)
                                  (lambda ,@(rest setf-expander))))))
                 (values vars
                         (rest place)
                         (list store)
                         (funcall (apply fn vars) store)
                         `(,(first place) ,@vars)))))
          ;; TODO: マクロはホスト側で管理しているのでコンパイラ内の情報を参照する必要があるはず
          ;; ((and (consp place) (symbolp (first place)) (macro-function (first place)))
          ;;  (get-setf-expansion (macroexpand place)))
          (t
           (let ((store (gensym)))
             (values nil nil (list store) `(setq ,place ,store) place))))))
|#

(defmacro setf (&rest pairs)
  (labels ((gensyms (list)
             (mapcar (lambda (x)
                       (declare (ignore x))
                       (gensym))
                     list))
           (get-setf-expansion (place &optional environment)
             ;; これと同じものをdefunで定義しているが
             ;; defmacroはホスト側で評価されるのでここにも必要
             (declare (ignore environment))
             (let ((setf-expander nil))
               (cond ((and (consp place)
                           (setq setf-expander (get (first place) 'setf-expander)))
                      (if (symbolp setf-expander)
                          (let ((vars (gensyms (rest place)))
                                (store (gensym "STORE")))
                            (values vars
                                    (rest place)
                                    (list store)
                                    `(,setf-expander ,@vars ,store)
                                    `(,(first place) ,@vars)))
                          (let ((vars (gensyms (rest place)))
                                (store (gensym "STORE"))
                                (fn (eval `(lambda ,(first setf-expander)
                                             (lambda ,@(rest setf-expander))))))
                            (values vars
                                    (rest place)
                                    (list store)
                                    (funcall (apply fn vars) store)
                                    `(,(first place) ,@vars)))))
                     ;; TODO: マクロはホスト側で管理しているので
                     ;; コンパイラ内の情報を参照する必要があるはず
                     ;; ((and (consp place) (symbolp (first place)) (macro-function (first place)))
                     ;;  (get-setf-expansion (macroexpand place)))
                     (t
                      (let ((store (gensym)))
                        (values nil nil (list store) `(setq ,place ,store) place))))))
           (setf-expand-1 (place value)
             (multiple-value-bind (vars forms store set access)
                 (get-setf-expansion place)
               (declare (ignore access))
               `(let* (,@(mapcar #'list
                                 (append vars store)
                                 (append forms (list value))))
                  ,set)))
           (setf-expand (pairs)
             (cond ((endp pairs) nil)
                   ((endp (cdr pairs)) (error "Odd number of args to SETF."))
                   (t (cons (setf-expand-1 (first pairs) (second pairs))
                            (setf-expand (cddr pairs)))))))
    `(progn ,@(setf-expand pairs))))

(defmacro defsetf (access-fn &rest rest)
  ;; TODO: documentation文字列
  ;; TODO: restが単一のシンボルか関数ではないときの処理
  (check-type access-fn symbol)
  (cond ((and (first rest)
              (or (symbolp (first rest)) (functionp (first rest))))
         (setf (get access-fn 'setf-expander) (first rest))
         `(progn
            ;(setf (get ',access-fn 'setf-expander) ,(first rest))
            ',access-fn))
        (t
         (setf (get access-fn 'setf-expander) rest)
         `(progn
            ;(setf (get ',access-fn 'setf-expander) ',rest)
            ',access-fn))))

(defmacro psetq (&rest pairs)
  (when (oddp (length pairs))
    (error "Odd number of args to PSETQ."))
  (let ((gvars '())
        (vars '())
        (values '()))
    (do ((pairs* pairs (cddr pairs*)))
        ((null pairs*))
      (let ((var (first pairs*))
            (value (second pairs*)))
        (push var vars)
        (push value values)
        (push (gensym) gvars)))
    (setq gvars (nreverse gvars))
    (setq vars (nreverse vars))
    (setq values (nreverse values))
    `(let ,(mapcar #'list gvars values)
       ,@(mapcar (lambda (var gvar) `(setq ,var ,gvar))
                 vars gvars)
       nil)))

(defmacro do (varlist endlist &body body)
  (let ((g-start (gensym)))
    `(let ,(mapcar (lambda (var-spec)
                     `(,(first var-spec)
                       ,(second var-spec)))
                   varlist)
       (tagbody
         ,g-start
         (if ,(first endlist)
             (progn
               ,@(rest endlist))
             (progn
               (tagbody ,@body)
               (psetq ,@(mapcan (lambda (var-spec)
                                  `(,(first var-spec)
                                    ,(third var-spec)))
                                varlist))
               (go ,g-start)))))))

(defmacro dotimes ((var expr &optional result) &body body)
  (let ((g-expr (gensym)))
    `(let ((,g-expr ,expr))
       (do ((,var 0 (+ ,var 1)))
           ((>= ,var ,g-expr) ,result)
         ,@body))))
