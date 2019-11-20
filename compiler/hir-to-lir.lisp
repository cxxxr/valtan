(in-package :compiler)

(defparameter *temp-counter* 0)

(defvar *compiland-vars* '())
(defvar *compiland-functions* '())
(defvar *compiland-body* '())

(defstruct compiland
  vars
  functions
  body)

(defstruct lir-fn
  name
  lambda-list
  body)

(defun make-lir (op &rest args)
  (cons op args))

(defun lir-op (lir)
  (first lir))

(defun lir-arg1 (lir)
  (second lir))

(defun lir-arg2 (lir)
  (third lir))

(defun gen-temp (prefix)
  (prog1 (make-symbol (format nil "~A~A" prefix *temp-counter*))
    (incf *temp-counter*)))

(defun gen-label ()
  (gen-temp "L"))

(defun gen-var ()
  (let ((var (gen-temp "V")))
    (push var *compiland-vars*)
    var))

(defun emit-lir (lir)
  (push lir *compiland-body*))

(defun emit-lir-forms (hir-forms)
  (let (r)
    (dolist (hir hir-forms)
      (setq r (hir-to-lir-1 hir)))
    r))

(defun hir-to-lir-1 (hir)
  (ecase (hir-op hir)
    ((const)
     (let ((lir (make-lir 'const (hir-arg1 hir))))
       lir))
    ((lref)
     (binding-id (hir-arg1 hir)))
    ((gref)
     (hir-arg1 hir))
    ((lset)
     (let ((var (binding-id (hir-arg1 hir)))
           (result (hir-to-lir-1 (hir-arg2 hir))))
       (emit-lir (make-lir 'lset var result))
       var))
    ((gset)
     (emit-lir (make-lir 'gset (hir-arg1 hir) (hir-arg2 hir)))
     (hir-arg1 hir))
    ((if)
     (let ((test (hir-to-lir-1 (hir-arg1 hir)))
           (flabel (gen-label))
           (tlabel (gen-label))
           (result (gen-var)))
       ;; then
       (emit-lir (make-lir 'fjump test flabel))
       (let ((then (hir-to-lir-1 (hir-arg2 hir))))
         (emit-lir (make-lir 'move result then)))
       (emit-lir (make-lir 'jump tlabel))
       ;; else
       (emit-lir (make-lir 'label flabel))
       (let ((else (hir-to-lir-1 (hir-arg3 hir))))
         (emit-lir (make-lir 'move result else)))
       ;; merge branch
       (emit-lir (make-lir 'label tlabel))
       result))
    ((progn)
     (emit-lir-forms (hir-arg1 hir)))
    ((lambda)
     (let* ((name (hir-arg1 hir))
            (lambda-list (hir-arg2 hir))
            (body (hir-arg3 hir))
            (lir-body (let ((*compiland-body* '()))
                        (emit-lir-forms body)
                        *compiland-body*)))
       (let ((fn (make-lir-fn :name (or name (gensym)) :lambda-list lambda-list :body lir-body)))
         (push fn
               *compiland-functions*)
         (make-lir 'fn (lir-fn-name fn)))))
    ((let)
     (let ((bindings (hir-arg1 hir)))
       (dolist (binding bindings)
         (ecase (binding-type binding)
           (:variable
            (push (binding-id binding) *compiland-vars*)
            (let ((r (hir-to-lir-1 (binding-init-value binding))))
              (emit-lir (make-lir 'move (binding-id binding) r))))
           (:function
            (push (binding-id binding) *compiland-vars*)
            (let ((r (hir-to-lir-1 (binding-init-value binding))))
              (emit-lir (make-lir 'move (binding-id binding) r))))))
       (emit-lir-forms (hir-arg2 hir))))
    ((lcall call)
     (let ((args (mapcar (lambda (arg)
                           (let ((r (hir-to-lir-1 arg))
                                 (a (gen-var)))
                             (emit-lir (make-lir 'move a r))
                             a))
                         (hir-arg2 hir)))
           (result (gen-var)))
       (emit-lir (make-lir 'move
                           result
                           (make-lir (hir-op hir)
                                     (if (eq 'lcall (hir-op hir))
                                         (binding-id (hir-arg1 hir))
                                         (hir-arg1 hir))
                                     args)))
       result))
    ((unwind-protect)
     (let ((protected-form (hir-arg1 hir))
           (cleanup-form (hir-arg2 hir)))
       (emit-lir (make-lir 'unwind-protect))
       (let ((result (hir-to-lir-1 protected-form)))
         (emit-lir (make-lir 'cleanup-start))
         (hir-to-lir-1 cleanup-form)
         (emit-lir (make-lir 'cleanup-end))
         result)))
    ((block)
     (let ((name (hir-arg1 hir))
           (body (hir-arg2 hir)))
       (setf (binding-init-value name) (gen-var))
       (let ((result (emit-lir-forms body)))
         (emit-lir (make-lir 'label (binding-id name)))
         result)))
    ((return-from)
     (let ((name (hir-arg1 hir))
           (result (hir-to-lir-1 (hir-arg2 hir))))
       (emit-lir (make-lir 'move (binding-init-value name) result))
       (emit-lir (make-lir 'jump (binding-id name)))
       (binding-init-value name)))
    ((tagbody)
     (dolist (elt (hir-arg2 hir))
       (destructuring-bind (tag . form) elt
         (emit-lir (make-lir 'label (tagbody-value-index (binding-id tag))))
         (hir-to-lir-1 form)))
     (make-lir 'const nil))
    ((go)
     (emit-lir (make-lir 'jump (tagbody-value-index (hir-arg1 hir))))
     (make-lir 'const nil))
    ((catch)
     )
    ((throw)
     )
    ((*:%defun)
     )
    ((*:%defpackage) hir)
    ((*:%in-package) hir)
    ((ffi:ref) hir)
    ((ffi:set) hir)
    ((ffi:var) hir)
    ((ffi:typeof) hir)
    ((ffi:new) hir)
    ((ffi:aget) hir)
    ((js-call) hir)
    ((module) hir)))

(defun hir-to-lir (hir)
  (let ((*temp-counter* 0)
        (*compiland-vars* '())
        (*compiland-functions* '())
        (*compiland-body* '()))
    (emit-lir (make-lir 'return (hir-to-lir-1 hir)))
    (make-compiland :vars *compiland-vars*
                    :functions *compiland-functions*
                    :body (coerce (nreverse *compiland-body*) 'vector))))


#|
(LET (("X_1" #(CONST 0)))
  (CALL + (LREF "X_1") #(CONST 1)))

(prog (X_1)
  (move X_1 (const 0))
  (call + X_1 (const 1)))

(let ((x 0))
  (defun counter ()
    (setq x (+ x 1))))

(let (("x_1" #(const 0)))
  (call system:fset #(const counter)
        (named-lambda nil #s(parsed-lambda-list :vars nil
                                                :rest-var nil
                                                :optionals nil
                                                :keys nil
                                                :min 0
                                                :max 0
                                                :allow-other-keys nil)
          (progn (progn (lset "x_1" (call + (lref "x_1") #(const 1))))))))

(compiland :vars (x tmp0 tmp1 tmp2 tmp3 tmp4)
           :functions #((named-lambda nil #s(parsed-lambda-list :vars nil
                                                                :rest-var nil
                                                                :optionals nil
                                                                :keys nil
                                                                :min 0
                                                                :max 0
                                                                :allow-other-keys nil)
                          (lset tmp2 x)
                          (lset tmp3 (const 1))
                          (lset tmp4 (call + tmp2 tmp3))
                          (return tmp4)))
           :body #((lset x 0)
                   (lset tmp0 (const counter))
                   (lset tmp1 (fn 0))
                   (call fset tmp0 tmp1)))

|#