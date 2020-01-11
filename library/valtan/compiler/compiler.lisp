(in-package :compiler)

(defun pass1-toplevel-usign-optimize (form)
  (let ((hir (pass1-toplevel form)))
    (if *hir-optimize*
        (hir-optimize hir)
        hir)))

(defun compile-toplevel (x)
  (let ((ir (pass1-toplevel x t)))
    (with-output-to-string (*standard-output*)
      (p2-toplevel ir))))

(defun !macroexpand-1 (form &optional environment)
  (declare (ignore environment))
  (let ((*lexenv* nil))
    (%macroexpand-1 form)))
