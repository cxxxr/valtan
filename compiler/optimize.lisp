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
  ir)

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
  ir)

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
  ir)

(define-optimize call (ir)
  ir)

(define-optimize unwind-protect (ir)
  ir)

(define-optimize block (ir)
  ir)

(define-optimize return-from (ir)
  ir)

(define-optimize tagbody (ir)
  ir)

(define-optimize go (ir)
  ir)

(define-optimize catch (ir)
  ir)

(define-optimize throw (ir)
  ir)

(define-optimize *:%defun (ir)
  ir)

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
