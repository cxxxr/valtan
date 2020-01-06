(in-package :compiler)

(defgeneric walk-hir-aux (walker op hir))

(defclass hir-walker () ())

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'const)) hir)
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'const)) hir)
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'lref)) hir)
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'gref)) hir)
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'lset)) hir)
  (walk-hir walker (hir-arg2 hir))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'gset)) hir)
  (walk-hir walker (hir-arg2 hir))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'if)) hir)
  (walk-hir walker (hir-arg1 hir))
  (walk-hir walker (hir-arg2 hir))
  (walk-hir walker (hir-arg3 hir))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'progn)) hir)
  (dolist (arg (hir-arg1 hir))
    (walk-hir walker arg))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'lambda)) hir)
  (walk-hir walker (hir-arg3 hir))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'let)) hir)
  (let ((bindings (hir-arg1 hir))
        (body (hir-arg2 hir)))
    (dolist (binding bindings)
      (walk-hir walker (binding-init-value binding)))
    (walk-hir walker body))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'call)) hir)
  (dolist (arg (hir-arg2 hir))
    (walk-hir walker arg))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'lcall)) hir)
  (dolist (arg (hir-arg2 hir))
    (walk-hir walker arg))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'unwind-protect)) hir)
  (walk-hir walker (hir-arg1 hir))
  (walk-hir walker (hir-arg2 hir))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'block)) hir)
  (dolist (form (hir-arg2 hir))
    (walk-hir walker form))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'return-from)) hir)
  (walk-hir walker (hir-arg2 hir))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'tagbody)) hir)
  (dolist (tag-body-pairs (hir-arg2 hir))
    (walk-hir walker (cdr tag-body-pairs)))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'go)) hir)
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql '*:%defun)) hir)
  (walk-hir walker (hir-arg2 hir))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql '*:%defpackage)) hir)
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql '*:%in-package)) hir)
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'ffi:ref)) hir)
  (let ((place (hir-arg1 hir)))
    (unless (stringp (first place))
      (walk-hir walker (first place))))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'ffi:set)) hir)
  (walk-hir walker (hir-arg1 hir))
  (walk-hir walker (hir-arg2 hir))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'ffi:var)) hir)
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'ffi:typeof)) hir)
  (walk-hir walker (hir-arg1 hir))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'ffi:new)) hir)
  (let ((object (hir-arg1 hir))
        (args (hir-arg2 hir)))
    (walk-hir walker object)
    (dolist (arg args)
      (walk-hir walker arg)))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'ffi:aget)) hir)
  (let ((array (hir-arg1 hir))
        (indexes (hir-arg2 hir)))
    (walk-hir walker array)
    (dolist (index indexes)
      (walk-hir walker index)))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'js-call)) hir)
  (let ((place (hir-arg1 hir))
        (args (hir-arg2 hir)))
    (unless (stringp (first place))
      (walk-hir walker (first place)))
    (dolist (arg args)
      (walk-hir walker arg)))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'module)) hir)
  (let ((forms (hir-arg2 hir)))
    (dolist (form forms)
      (walk-hir walker form)))
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'recur)) hir)
  hir)

(defmethod walk-hir-aux ((walker hir-walker) (op (eql 'loop)) hir)
  (walk-hir walker (hir-arg1 hir))
  hir)

(defun walk-hir (walker hir)
  (walk-hir-aux walker (hir-op hir) hir))
