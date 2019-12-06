(in-package :compiler)

(defstruct (hir (:constructor %make-hir))
  op
  return-value-p
  multiple-values-p
  args)

(defun hir-arg1 (hir) (first (hir-args hir)))
(defun hir-arg2 (hir) (second (hir-args hir)))
(defun hir-arg3 (hir) (third (hir-args hir)))

(defun (setf hir-arg1) (value hir) (setf (first (hir-args hir)) value))
(defun (setf hir-arg2) (value hir) (setf (second (hir-args hir)) value))
(defun (setf hir-arg3) (value hir) (setf (third (hir-args hir)) value))

(defun reduce-hir (hir)
  (ecase (hir-op hir)
    ((const)
     `#(const ,(hir-arg1 hir)))
    ((lref)
     `(,(hir-op hir) ,(binding-id (hir-arg1 hir))))
    ((gref)
     `(,(hir-op hir) ,(hir-arg1 hir)))
    ((lset)
     `(,(hir-op hir)
       ,(binding-id (hir-arg1 hir))
       ,(reduce-hir (hir-arg2 hir))))
    ((gset)
     `(,(hir-op hir)
       ,(hir-arg1 hir)
       ,(reduce-hir (hir-arg2 hir))))
    ((if)
     `(if ,(reduce-hir (hir-arg1 hir)) ,(reduce-hir (hir-arg2 hir)) ,(reduce-hir (hir-arg3 hir))))
    ((progn)
     `(progn ,@(mapcar #'reduce-hir (hir-arg1 hir))))
    ((lambda)
     `(named-lambda ,(hir-arg1 hir) ,(hir-arg2 hir) ,@(mapcar #'reduce-hir (hir-arg3 hir))))
    ((let)
     `(let ,(mapcar (lambda (b)
                      `(,(binding-id b)
                        ,(reduce-hir (binding-init-value b))
                        ,@(if (eq :special (binding-type b))
                              '(:special))))
                    (hir-arg1 hir))
        ,@(mapcar #'reduce-hir (hir-arg2 hir))))
    ((lcall call)
     `(,(hir-op hir) ,(hir-arg1 hir) ,@(mapcar #'reduce-hir (hir-arg2 hir))))
    ((unwind-protect)
     `(unwind-protect ,(reduce-hir (hir-arg1 hir)) ,(reduce-hir (hir-arg2 hir))))
    ((block)
     `(block ,(binding-id (hir-arg1 hir))
        ,@(mapcar #'reduce-hir (hir-arg2 hir))))
    ((return-from)
     `(return-from ,(binding-id (hir-arg1 hir))
        ,(reduce-hir (hir-arg2 hir))))
    ((tagbody)
     `(tagbody ,(hir-arg1 hir)
        ,@(mapcar (lambda (x)
                    (destructuring-bind (binding . body) x
                      `(,(tagbody-value-index (binding-id binding))
                        ,(reduce-hir body))))
            (hir-arg2 hir))))
    ((go)
     `(go ,(tagbody-value-index (binding-id (hir-arg1 hir)))))
    ((catch)
     `(catch ,(reduce-hir (hir-arg1 hir))
        ,(reduce-hir (hir-arg2 hir))))
    ((throw)
     `(throw ,(reduce-hir (hir-arg1 hir)) ,(reduce-hir (hir-arg2 hir))))
    ((*:%defun)
     `(*:%defun ,(hir-arg1 hir)
                ,(reduce-hir (hir-arg2 hir))))
    ((*:%defpackage) hir)
    ((*:%in-package) hir)
    ((ffi:ref) hir)
    ((ffi:set) hir)
    ((ffi:var) hir)
    ((ffi:typeof) hir)
    ((ffi:new) hir)
    ((ffi:aget) hir)
    ((js-call) hir)
    ((module)
     `(module ,(hir-arg1 hir) ,@(mapcar #'reduce-hir (hir-arg2 hir))))
    ((recur)
     `(recur))
    ((loop)
     `(loop ,(reduce-hir (hir-arg1 hir))))))

(defun make-hir (op return-value-p multiple-values-p &rest args)
  (%make-hir :op op
            :return-value-p return-value-p
            :multiple-values-p multiple-values-p
            :args args))

(defun remake-hir (op hir &rest args)
  (apply #'make-hir op (hir-return-value-p hir) (hir-multiple-values-p hir) args))

(defstruct parsed-lambda-list
  vars
  rest-var
  optionals
  keys
  min
  max
  allow-other-keys)

(defun collect-variables (parsed-lambda-list)
  (append (parsed-lambda-list-vars parsed-lambda-list)
          (if (parsed-lambda-list-rest-var parsed-lambda-list)
              (list (parsed-lambda-list-rest-var parsed-lambda-list)))
          (mapcar #'first (parsed-lambda-list-optionals parsed-lambda-list))
          (remove nil (mapcar #'third (parsed-lambda-list-optionals parsed-lambda-list)))
          (mapcar #'first (parsed-lambda-list-keys parsed-lambda-list))
          (remove nil (mapcar #'third (parsed-lambda-list-keys parsed-lambda-list)))))

(defstruct binding
  name
  type
  id
  init-value
  (used-count 0)
  (set-count 0))

(defstruct tagbody-value
  index
  id
  escape-count)

(defparameter *equal-error* t)

(defun maybe-equal-error (x y)
  (if *equal-error*
      (error "~:W is false" `(equal-with-hir ,x ,y))
      nil))

(defun equal-with-hir (x y)
  (cond ((and (consp x)
              (consp y))
         (and (equal-with-hir (car x) (car y))
              (equal-with-hir (cdr x) (cdr y))))
        ((and (stringp x)
              (stringp y))
         (string= x y))
        ((and (simple-bit-vector-p x)
              (simple-bit-vector-p y))
         (and (= (length x) (length y))
              (dotimes (i (length x) t)
                (unless (eql (aref x i)
                             (aref y i))
                  (return nil)))))
        ((and (hir-p x) (hir-p y))
         (equal-hir x y))
        ((and (binding-p x) (binding-p y))
         (equal-binding x y))
        (t
         (eql x y))))

(defun equal-hir (hir1 hir2)
  (if (and (eq (hir-op hir1) (hir-op hir2))
           (eq (hir-return-value-p hir1) (hir-return-value-p hir2))
           (eq (hir-multiple-values-p hir1) (hir-multiple-values-p hir2))
           (every #'equal-with-hir
                  (hir-args hir1)
                  (hir-args hir2)))
      t
      (maybe-equal-error hir1 hir2)))

(defun equal-binding (b1 b2)
  (if (and (string= (binding-id b1)
                    (binding-id b2))
           (eq (binding-type b1)
               (binding-type b2))
           (equal-with-hir (binding-init-value b1)
                   (binding-init-value b2)))
      t
      (maybe-equal-error b1 b2)))
