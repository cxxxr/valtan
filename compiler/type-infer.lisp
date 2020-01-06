(in-package :compiler)

(defclass type-infer-walker (hir-walker) ())

(defmethod walk-hir-aux ((walker type-infer-walker) (op (eql 'lref)) hir)
  (let ((binding (hir-arg1 hir)))
    (setf (hir-result-type hir)
          (binding-var-type binding)))
  hir)
