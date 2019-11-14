(in-package :compiler)

(defmacro with-ir-args ((&rest args) ir &body body)
  (let ((g-ir (gensym)))
    `(let ((,g-ir ,ir))
       (symbol-macrolet ,(mapcar (lambda (arg ir-arg)
                                   (list arg `(,ir-arg ,g-ir)))
                                 args
                                 '(ir-arg1 ir-arg2 ir-arg3))
         ,@body))))

(defmacro define-optimize (name (ir) &body body)
  (let ((g-name (make-symbol (string name))))
    `(progn
       (setf (get ',name 'optimize1) ',g-name)
       (defun ,g-name (,ir)
         ,@body))))

(defun get-optimizer (ir)
  (get (ir-op ir) 'optimize1))

(defun optimize1 (ir)
  (funcall (get-optimizer ir) ir))

(defun const-ir-p (ir)
  (or (eq (ir-op ir) 'const)
      (eq (ir-op ir) 'lref)
      (eq (ir-op ir) 'gref)))

(defun immutable-p (ir)
  (or (eq (ir-op ir) 'const)
      (and (eq (ir-op ir) 'lref)
           (zerop (binding-set-count (ir-arg1 ir))))))

(define-optimize const (ir)
  ir)

(define-optimize lref (ir)
  (with-ir-args (binding) ir
    (cond ((and (zerop (binding-set-count binding))
                (immutable-p (binding-init-value binding)))
           (decf (binding-used-count binding))
           (let ((init-value (binding-init-value binding)))
             (apply #'make-ir
                    (ir-op init-value)
                    (ir-return-value-p ir)
                    (ir-multiple-values-p ir)
                    (ir-args init-value))))
          (t
           ir))))

(define-optimize gref (ir)
  ir)

(define-optimize lset (ir)
  (with-ir-args (binding value) ir
    (remake-ir 'lset ir binding (optimize1 value))))

(define-optimize gset (ir)
  (with-ir-args (binding value) ir
    (remake-ir 'gset ir binding (optimize1 value))))

(define-optimize if (ir)
  (with-ir-args (test then else) ir
    (setq test (optimize1 test))
    (cond ((eq (ir-op test) 'const)
           (cond ((null (ir-arg1 test))
                  else)
                 (t
                  test)))
          (t
           (setq then (optimize1 then)
                 else (optimize1 else))
           ir))))

(defun optimize-progn-forms (ir forms)
  (let ((new-forms '()))
    (dolist (form forms)
      (if (eq (ir-op form) 'progn)
          (if (ir-return-value-p form)
              (dolist (form1 (ir-arg1 form))
                (push (optimize1 form1) new-forms))
              (dolist (form1 (ir-arg1 form))
                (let ((optimized-form (optimize1 form1)))
                  (unless (const-ir-p optimized-form)
                    (push optimized-form new-forms)))))
          (let ((optimized-form (optimize1 form)))
            (unless (and (not (ir-return-value-p optimized-form)) (const-ir-p optimized-form))
              (push optimized-form new-forms)))))
    (cond ((null new-forms)
           (remake-ir 'const ir nil))
          ((null (cdr new-forms))
           (car new-forms))
          (t
           (remake-ir 'progn ir (nreverse new-forms))))))

(define-optimize progn (ir)
  (with-ir-args (forms) ir
    (optimize-progn-forms ir forms)))

(define-optimize lambda (ir)
  (with-ir-args (name lambda-list body) ir
    (remake-ir 'lambda ir name lambda-list (mapcar #'optimize1 body))))

(define-optimize let (ir)
  (with-ir-args (bindings body) ir
    (dolist (binding bindings)
      (setf (binding-init-value binding)
            (optimize1 (binding-init-value binding))))
    (let ((forms (optimize-progn-forms ir body)))
      (setf body
            (if (consp forms)
                forms
                (list forms))))
    (setf bindings
          (delete-if (lambda (binding)
                       (zerop (binding-used-count binding)))
                     bindings))
    (if (null bindings)
        (optimize1 (remake-ir 'progn ir body))
        ir)))

(define-optimize lcall (ir)
  (with-ir-args (fn-binding args) ir
    (remake-ir 'lcall ir fn-binding (mapcar #'optimize1 args))))

(define-optimize call (ir)
  (with-ir-args (fn-name args) ir
    (remake-ir 'call ir fn-name (mapcar #'optimize1 args))))

(define-optimize unwind-protect (ir)
  (with-ir-args (protected-form cleanup-form) ir
    (remake-ir 'unwind-protect ir (optimize1 protected-form) (optimize1 cleanup-form))))

(define-optimize block (ir)
  (with-ir-args (name body) ir
    (remake-ir 'block ir name (optimize1 body))))

(define-optimize return-from (ir)
  (with-ir-args (name value) ir
    (remake-ir 'return-from ir name (optimize1 value))))

(define-optimize tagbody (ir)
  (with-ir-args (tagbody-id tag-statements-pairs) ir
    (remake-ir 'tagbody
               ir
               tagbody-id
               (mapcar (lambda (pair)
                         (destructuring-bind (tag-binding . body) pair
                           (cons tag-binding (optimize1 body))))
                       tag-statements-pairs))))

(define-optimize go (ir)
  ir)

(define-optimize catch (ir)
  (with-ir-args (tag body) ir
    (remake-ir 'catch ir (optimize1 tag) (optimize1 body))))

(define-optimize throw (ir)
  (with-ir-args (tag result) ir
    (remake-ir 'throw ir (optimize1 tag) (optimize1 result))))

(define-optimize *:%defun (ir)
  (with-ir-args (name lambda-form) ir
    (remake-ir '*:%defun ir name (optimize1 lambda-form))))

(define-optimize *:%defpackage (ir)
  ir)

(define-optimize *:%in-package (ir)
  ir)

(define-optimize ffi:ref (ir)
  ir)

(define-optimize ffi:set (ir)
  ir)

(define-optimize ffi:var (ir)
  ir)

(define-optimize ffi:typeof (ir)
  ir)

(define-optimize ffi:new (ir)
  ir)

(define-optimize ffi:aget (ir)
  ir)

(define-optimize js-call (ir)
  ir)

(define-optimize module (ir)
  ir)
