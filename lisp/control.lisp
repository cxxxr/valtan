(in-package :common-lisp)

(defmacro return (&optional value)
  `(return-from nil ,value))

(defmacro cond (&rest clauses)
  (if (null clauses)
      nil
      (let ((clause (first clauses)))
        `(if ,(first clause)
             ,(if (null (rest clause))
                  t
                  `(progn ,@(rest clause)))
             (cond ,@(rest clauses))))))

(defmacro or (&rest forms)
  (if (null forms)
      nil
      (let ((value (gensym)))
        `(let ((,value ,(first forms)))
           (if ,value
               ,value
               (or ,@(rest forms)))))))

(defmacro and (&rest forms)
  (cond ((null forms))
        ((null (rest forms)) (first forms))
        (t `(if ,(first forms)
                (and ,@(rest forms))
                nil))))

(defmacro when (test &body forms)
  `(if ,test
       (progn ,@forms)))

(defmacro unless (test &body forms)
  `(if ,test
       nil
       (progn ,@forms)))

(defun not (x)
  (if x nil t))

(defmacro let* (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      `(let (,(first bindings))
         (let* ,(rest bindings)
           ,@body))))

(eval-when (:compile-toplevel)
  (defun gensyms (list)
    (mapcar (lambda (x)
              (declare (ignore x))
              (gensym))
            list))
  (defun !get-setf-expansion (place &optional environment)
    (declare (ignore environment))
    (let ((setf-expander nil))
      (cond ((and (consp place)
                  (setq setf-expander (get (first place) 'setf-expander)))
             (cond
               ((symbolp setf-expander)
                (let ((vars (gensyms (rest place)))
                      (store (gensym "STORE")))
                  (values vars
                          (rest place)
                          (list store)
                          `(,setf-expander ,@vars ,store)
                          `(,(first place) ,@vars))))
               ((consp setf-expander)
                (let ((vars (gensyms (rest place)))
                      (store (gensym "STORE"))
                      (fn (eval `(lambda ,(first setf-expander)
                                   (lambda ,@(rest setf-expander))))))
                  (values vars
                          (rest place)
                          (list store)
                          (funcall (apply fn vars) store)
                          `(,(first place) ,@vars))))
               ((functionp setf-expander)
                (funcall setf-expander (rest place)))))
            ;; TODO: マクロはホスト側で管理しているので
            ;; コンパイラ内の情報を参照する必要があるはず
            ;; ((and (consp place) (symbolp (first place)) (macro-function (first place)))
            ;;  (get-setf-expansion (macroexpand place)))
            (t
             (let ((store (gensym)))
               (values nil nil (list store) `(setq ,place ,store) place)))))))

(defmacro setf (&rest pairs)
  (labels ((setf-expand-1 (place value)
             (multiple-value-bind (vars forms store set access)
                 (!get-setf-expansion place)
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

(defmacro define-setf-expander (access-fn lambda-list &body body)
  (unless (symbolp access-fn)
    (error "DEFINE-SETF-EXPANDER access-function name ~S is not a symbol." access-fn))
  (let ((g-rest (gensym)))
    (setf (get access-fn 'setf-expander)
          (eval `(lambda (,g-rest)
                   (destructuring-bind ,lambda-list ,g-rest ,@body))))
    `',access-fn
    #+(or)
    `(eval-when (#|:compile-toplevel :load-toplevel|# :execute)
       (setf (get ',access-fn 'setf-expander)
             (lambda (,g-rest)
               (destructuring-bind ,lambda-list ,g-rest ,@body)))
       ',access-fn)))

(defmacro define-modify-macro (name lambda-list function &optional (documentation nil documentation-p))
  (let ((update-form
          (do ((rest lambda-list (cdr rest))
               (vars '()))
              ((null rest)
               `(list ',function access-form ,@(nreverse vars)))
            (cond ((eq '&optional (car rest)))
                  ((eq '&rest (car rest))
                   (return `(list* ',function access-form ,@(nreverse vars) (cadr rest))))
                  ((symbolp (car rest))
                   (push (car rest) vars))
                  (t
                   (push (caar rest) vars))))))
    (let ((reference (gensym "REFERENCE")))
      `(defmacro ,name (,reference ,@lambda-list)
         ,(when documentation-p `(,documentation))
         (multiple-value-bind (vars values stores set-form access-form)
             (!get-setf-expansion ,reference)
           (list 'let*
                 (mapcar #'list
                         (append vars stores)
                         (append values (list ,update-form)))
                 set-form))))))

(define-modify-macro incf (&optional (n 1)) +)
(define-modify-macro decf (&optional (n 1)) -)

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

;; TODO: (do ((var init)) ...)のときvarをループ毎に更新しないようにする
(defmacro do (varlist endlist &body body)
  (let ((g-start (gensym)))
    `(block nil
       (let ,(mapcar (lambda (var-spec)
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
                 (go ,g-start))))))))

(defmacro dotimes ((var expr &optional result) &body body)
  (let ((g-expr (gensym)))
    `(let ((,g-expr ,expr))
       (do ((,var 0 (+ ,var 1)))
           ((>= ,var ,g-expr) ,result)
         ,@body))))

(defmacro dolist ((var expr &optional result) &body body)
  (let ((g-list (gensym))
        (g-start (gensym)))
    `(block nil
       (let ((,g-list ,expr))
         (tagbody
           ,g-start
           (unless (endp ,g-list)
             (let ((,var (car ,g-list)))
               (setq ,g-list (cdr ,g-list))
               (tagbody ,@body))
             (go ,g-start))))
       ,result)))

(defmacro case (keyform &body cases)
  (let ((var (gensym)))
    `(let ((,var ,keyform))
       (cond ,@(mapcar (lambda (c)
                         (cond ((eq 'otherwise (car c))
                                `(t ,@(cdr c)))
                               ((listp (car c))
                                `((member ,var ',(car c))
                                  ,@(cdr c)))
                               (t
                                `((eql ,var ',(car c))
                                  ,@(cdr c)))))
                       cases)))))

(defmacro ecase (keyform &body cases)
  (let ((var (gensym)))
    `(let ((,var ,keyform))
       (cond ,@(mapcar (lambda (c)
                         (cond ((listp (car c))
                                `((member ,var ',(car c))
                                  ,@(cdr c)))
                               (t
                                `((eql ,var ',(car c))
                                  ,@(cdr c)))))
                       cases)
             (t (error "ecase error"))))))

(defmacro multiple-value-bind (vars value-form &body body)
  (let ((rest (gensym)))
    `(multiple-value-call (lambda (&optional ,@vars &rest ,rest)
                            (declare (ignore ,rest))
                            ,@body)
       ,value-form)))

(defvar *check-arg-forms*)

(defun parse-db-lambda-list (lambda-list arg)
  (let ((bindings '())
        (path arg)
        (min 0)
        (max 0)
        (state nil)
        (rest-p nil))
    (do ((rest lambda-list (cdr rest)))
        ((null rest))
      (let ((x (car rest)))
        (cond ((eq x '&optional)
               (setq state :optional))
              ((member x '(&rest &body))
               (setq rest-p t)
               ;;
               )
              ((listp x)
               )
              ((symbolp x)
               (ecase state
                 ((nil)
                  (incf min)
                  (incf max)
                  (push (list x `(car ,path)) bindings)
                  (let ((cdr-var (gensym "TMP")))
                    (push (list cdr-var `(cdr ,path)) bindings)
                    (setq path cdr-var)))
                 ((:optional)
                  (incf max)
                  (push (list x `(car ,path)) bindings)
                  (let ((cdr-var (gensym "TMP")))
                    (push (list cdr-var `(cdr ,path)) bindings)
                    (setq path cdr-var))))))))
    (push `(unless (<= ,min (length ,arg) ,max)
             (error "Invalid number of arguments: ~S ~S" ',lambda-list ,arg))
          *check-arg-forms*)
    (nreverse bindings)))

(defun expand-destructuring-bind (lambda-list expression body)
  (let ((*check-arg-forms* '()))
    (let ((bindings (parse-db-lambda-list lambda-list expression)))
      `(progn
         ,@*check-arg-forms*
         (let ,bindings
           ,@body)))))

(defmacro !destructuring-bind (lambda-list expression &body body)
  )
