(in-package :compiler)

(defvar *env-for-escape* '())
(defvar *current-tagbody-label* nil)
(defvar *tagbody-escape-pairs* nil)

(defmacro with-hir-args ((&rest args) hir &body body)
  (let ((g-hir (gensym)))
    `(let ((,g-hir ,hir))
       (symbol-macrolet ,(mapcar (lambda (arg hir-arg)
                                   (list arg `(,hir-arg ,g-hir)))
                                 args
                                 '(hir-arg1 hir-arg2 hir-arg3))
         ,@body))))

(defmacro define-hir-optimizer (name (hir) &body body)
  (let ((g-name (make-symbol (string name))))
    `(progn
       (setf (get ',name 'hir-optimize) ',g-name)
       (defun ,g-name (,hir)
         ,@body))))

(defun get-hir-optimizer (hir)
  (get (hir-op hir) 'hir-optimize))

(defun hir-optimize (hir)
  (funcall (get-hir-optimizer hir) hir))

(defun const-hir-p (hir)
  (or (eq (hir-op hir) 'const)
      (eq (hir-op hir) 'lref)
      (eq (hir-op hir) 'gref)))

(defun immutable-p (hir)
  (or (eq (hir-op hir) 'const)
      (and (eq (hir-op hir) 'lref)
           (zerop (binding-set-count (hir-arg1 hir))))))

(define-hir-optimizer const (hir)
  hir)

(define-hir-optimizer lref (hir)
  (with-hir-args (binding) hir
    (incf (binding-used-count binding))
    hir))

(define-hir-optimizer gref (hir)
  hir)

(define-hir-optimizer lset (hir)
  (with-hir-args (binding value) hir
    (incf (binding-set-count binding))
    (remake-hir 'lset hir binding (hir-optimize value))))

(define-hir-optimizer gset (hir)
  (with-hir-args (binding value) hir
    (remake-hir 'gset hir binding (hir-optimize value))))

(define-hir-optimizer if (hir)
  (with-hir-args (test then else) hir
    (setq test (hir-optimize test))
    (cond ((eq (hir-op test) 'const)
           (cond ((null (hir-arg1 test))
                  (hir-optimize else))
                 (t
                  (hir-optimize then))))
          (t
           (setq then (hir-optimize then)
                 else (hir-optimize else))
           hir))))

(defun hir-optimize-progn-forms (hir forms)
  (let ((new-forms '()))
    (dolist (form forms)
      (if (eq (hir-op form) 'progn)
          (if (hir-return-value-p form)
              (dolist (form1 (hir-arg1 form))
                (push (hir-optimize form1) new-forms))
              (dolist (form1 (hir-arg1 form))
                (let ((optimized-form (hir-optimize form1)))
                  (unless (const-hir-p optimized-form)
                    (push optimized-form new-forms)))))
          (let ((optimized-form (hir-optimize form)))
            (unless (and (not (hir-return-value-p optimized-form))
                         (const-hir-p optimized-form))
              (push optimized-form new-forms)))))
    (cond ((null new-forms)
           (remake-hir 'const hir nil))
          ((null (cdr new-forms))
           (car new-forms))
          (t
           (remake-hir 'progn hir (nreverse new-forms))))))

(define-hir-optimizer progn (hir)
  (with-hir-args (forms) hir
    (hir-optimize-progn-forms hir forms)))

(define-hir-optimizer lambda (hir)
  (with-hir-args (name lambda-list body) hir
    (remake-hir 'lambda hir name lambda-list
                (let ((*current-tagbody-label* nil)
                      (*env-for-escape* '()))
                  (hir-optimize body)))))

(define-hir-optimizer let (hir)
  (with-hir-args (bindings body) hir
    (dolist (binding bindings)
      (setf (binding-init-value binding)
            (hir-optimize (binding-init-value binding)))
      (setf (binding-used-count binding) 0)
      (setf (binding-set-count binding) 0))
    (let ((forms (hir-optimize-progn-forms hir body)))
      (setf body
            (if (consp forms)
                forms
                (list forms))))
    (setf bindings
          (delete-if (lambda (binding)
                       (zerop (binding-used-count binding)))
                     bindings))
    (if (null bindings)
        (hir-optimize (remake-hir 'progn hir body))
        hir)))

(define-hir-optimizer lcall (hir)
  (with-hir-args (fn-binding args) hir
    (remake-hir 'lcall hir fn-binding (mapcar #'hir-optimize args))))

(define-hir-optimizer call (hir)
  (with-hir-args (fn-name args) hir
    (remake-hir 'call hir fn-name (mapcar #'hir-optimize args))))

(define-hir-optimizer unwind-protect (hir)
  (with-hir-args (protected-form cleanup-form) hir
    (remake-hir 'unwind-protect hir (hir-optimize protected-form) (hir-optimize cleanup-form))))

(define-hir-optimizer block (hir)
  (with-hir-args (name body) hir
    (setf (binding-escape-count name) 0)
    (let ((form
            (let ((*env-for-escape* (cons name *env-for-escape*)))
              (hir-optimize-progn-forms hir body))))
      (remake-hir 'block hir name (list form)))))

(define-hir-optimizer return-from (hir)
  (with-hir-args (name value) hir
    (unless (member name *env-for-escape*)
      (incf (binding-escape-count name)))
    (remake-hir 'return-from hir name (hir-optimize value))))

(define-hir-optimizer tagbody (hir)
  (with-hir-args (tagbody-id tag-body-pairs) hir
    (dolist (tag-body-pair tag-body-pairs)
      (let ((binding (car tag-body-pair)))
        (setf (binding-used-count binding) 1)
        (setf (binding-escape-count binding) 0)))
    (let ((*tagbody-escape-pairs* (acons tagbody-id nil *tagbody-escape-pairs*)))
      (let ((*env-for-escape* (nconc (mapcar #'car tag-body-pairs) *env-for-escape*)))
        (dolist (pair tag-body-pairs)
          (setf (cdr pair)
                (let ((*current-tagbody-label* (car pair)))
                  (hir-optimize (cdr pair))))))
      (let ((tag-body-pairs
              (delete-if (lambda (pair)
                           (or (zerop (binding-used-count (car pair)))
                               (and (= (binding-used-count (car pair)) 1)
                                    (const-hir-p (cdr pair)))))
                         tag-body-pairs)))
        (cond ((null tag-body-pairs)
               (remake-hir 'const hir nil))
              ((and (length=1 tag-body-pairs)
                    (zerop (binding-escape-count (car (first tag-body-pairs)))))
               (remake-hir 'loop
                           hir
                           (cdr (first tag-body-pairs))))
              (t
               (remake-hir 'tagbody
                           hir
                           tagbody-id
                           tag-body-pairs
                           (not (cdr (assoc tagbody-id *tagbody-escape-pairs* :test #'string=))))))))))

(define-hir-optimizer go (hir)
  (with-hir-args (binding) hir
    (incf (binding-used-count binding))
    (cond ((eq *current-tagbody-label* binding)
           (remake-hir 'recur hir))
          ((member binding *env-for-escape*)
           hir)
          (t
           (let ((tagbody-name (tagbody-value-id (binding-id binding))))
             (setf (cdr (assoc tagbody-name *tagbody-escape-pairs* :test #'string=))
                   t))
           (incf (binding-escape-count binding))
           hir))))

(define-hir-optimizer loop (hir)
  hir)

(define-hir-optimizer recur (hir)
  hir)

(define-hir-optimizer *:%defun (hir)
  (with-hir-args (name lambda-form) hir
    (remake-hir '*:%defun hir name (hir-optimize lambda-form))))

(define-hir-optimizer *:%defpackage (hir)
  hir)

(define-hir-optimizer *:%in-package (hir)
  hir)

(define-hir-optimizer ffi:ref (hir)
  hir)

(define-hir-optimizer ffi:set (hir)
  hir)

(define-hir-optimizer ffi:var (hir)
  hir)

(define-hir-optimizer ffi:typeof (hir)
  hir)

(define-hir-optimizer ffi:new (hir)
  hir)

(define-hir-optimizer ffi:aget (hir)
  hir)

(define-hir-optimizer js-call (hir)
  hir)

(define-hir-optimizer module (hir)
  hir)

(defun hir-optimize-toplevel (hir)
  (let ((*env-for-escape* '())
        (*tagbody-escape-pairs* '()))
    (hir-optimize hir)))



(defun hir-optimize-test ()
  (flet ((test (input expected)
           (let ((actual (hir-optimize (pass1-toplevel input t nil))))
             (unless (equal-hir actual expected)
               (format t "ERROR: ~%expected: ~:W~%actual: ~W" expected actual))))
         (binding (id type init-value)
           (make-binding :id id :type type :init-value init-value)))
    (test '(if t x y)
          (make-HIR 'GREF T NIL 'X))
    (test '(if t (if t 1 2) 3)
          (make-HIR 'CONST T NIL 1))
    (test '(let ((x 3)) x)
          (make-HIR 'CONST T NIL 3))
    (test '(let ((x 0))
            (setq x 1)
            x)
          (make-hir 'let t nil
                    (list (binding
                           "X_1"
                           :variable
                           (make-hir 'const t nil 0)))
                    (list (make-hir 'progn
                                    t
                                    nil
                                    (list
                                     (make-hir 'lset nil nil
                                               (binding "X_1" :variable (make-hir 'const t nil 0))
                                               (make-hir 'const t nil 1))
                                     (make-hir 'lref
                                               t
                                               nil
                                               (binding "X_1" :variable (make-hir 'const t nil 0))))))))
    (test '(defun foo (x) 10 x)
          (make-hir 'system:%defun t nil
                    'foo
                    (make-hir 'lambda t nil
                              'foo
                              (make-parsed-lambda-list
                               :VARS (list (BINDING
                                            "X_1"
                                            :variable
                                            nil))
                               :REST-VAR NIL
                               :OPTIONALS NIL
                               :KEYS NIL
                               :MIN 1
                               :MAX 1
                               :ALLOW-OTHER-KEYS NIL)
                              (list (make-hir 'block t t
                                              (binding  "FOO_2" :block nil)
                                              (make-hir 'lref t nil
                                                        (binding "X_1" :variable nil)))))))))
