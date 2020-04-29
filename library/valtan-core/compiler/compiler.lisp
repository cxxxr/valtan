(in-package :compiler)

(defun pass1-toplevel-using-optimize (form)
  (let ((hir (pass1-toplevel form)))
    (if *hir-optimize*
        (hir-optimize hir)
        hir)))

(defun compile-toplevel (x &optional (return-value-p t))
  (let ((ir (pass1-toplevel x return-value-p return-value-p)))
    (with-output-to-string (*standard-output*)
      (p2-toplevel ir *standard-output* return-value-p))))

(defun !macroexpand-1 (form &optional environment)
  (declare (ignore environment))
  (let ((*lexenv* nil))
    (%macroexpand-1 form)))
