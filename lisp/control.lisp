(in-package :common-lisp)

(defun get-setf-expansion (place &optional environment)
  )

(defun setf-expand-1 (place value)
  (cond ((symbolp place)
         `(setq ,place ,value))
        ((and (consp place) (symbolp (first place)))
         (let ((expander (get (first place) 'setf-expander)))
           (cond ((or (symbolp expander) (functionp expander))
                  (if (functionp expander)
                      `(funcall ,expander ,@(rest place) ,value)
                      `(,expander ,@(rest place) ,value)))
                 (t
                  (error "TODO")))))
        (t
         (error "Invalid place: ~S" place))))

(defun setf-expand (pairs)
  (cond ((endp pairs) nil)
        ((endp (cdr pairs)) (error "Odd number of args to SETF."))
        (t (cons (setf-expand-1 (first pairs) (second pairs))
                 (setf-expand (cddr pairs))))))

(defmacro setf (&rest pairs)
  `(progn ,@(setf-expand pairs)))

(defmacro defsetf (access-fn &rest rest)
  ;; TODO: documentation文字列
  ;; TODO: restが単一のシンボルか関数ではないときの処理
  (check-type access-fn symbol)
  (cond ((and (first rest)
              (or (symbolp (first rest)) (functionp (first rest))))
         `(progn
            (setf (get access-fn 'setf-expander) (first rest))
            ',access-fn))
        (t
         `(progn
            (setf (get access-fn 'setf-expander) rest)
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
  `(let ,(mapcar (lambda (var-spec)
                   `(,(first var-spec)
                     ,(second var-spec)))
                 varlist)
     (if ,(first endlist)
         (progn ,@(rest endlist))
         (progn
           (tagbody ,@body)
           (psetq ,@(mapcan (lambda (var-spec)
                              `(,(first var-spec)
                                ,(third var-spec)))
                            varlist))))))

(defmacro dotimes ((var expr &optional result) &body body)
  (let ((,=expr= (gensym)))
    `(let ((,=expr= ,expr))
       (do ((,var 0 (+ var 1)))
           ((>= ,var ,=expr=) ,result)
         ,@body))))
