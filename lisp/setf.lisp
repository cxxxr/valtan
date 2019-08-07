(in-package :common-lisp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun !get-setf-expansion (place &optional environment)
    (declare (ignore environment))
    (flet ((gensyms (list)
             (mapcar (lambda (x)
                       (declare (ignore x))
                       (gensym))
                     list)))
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
                 (values nil nil (list store) `(setq ,place ,store) place))))))))

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
