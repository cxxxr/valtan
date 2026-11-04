(in-package :compiler)

(defun pass1-toplevel-using-optimize (form &optional return-value-p)
  (let ((hir (pass1-toplevel form return-value-p)))
    (if *hir-optimize*
        (hir-optimize hir)
        hir)))

(defun compile-toplevel (x &optional (return-value-p t) capture-literals)
  (let ((*p2-runtime-literals* (when capture-literals (make-hash-table :test 'eq)))
        (*p2-runtime-literal-values* nil))
    (let* ((ir (pass1-toplevel x return-value-p return-value-p))
           (code (with-output-to-string (*standard-output*)
                   (p2-toplevel ir *standard-output* return-value-p)))
           (constants (when capture-literals
                        (system:make-raw-array (hash-table-count *p2-runtime-literals*)))))
      (when capture-literals
        (let ((i 0))
          (dolist (object (reverse *p2-runtime-literal-values*))
            (system:raw-array-set constants i object)
            (incf i))))
      (values code constants))))

(defun !macroexpand-1 (form &optional environment)
  (let ((*lexenv* environment))
    (%macroexpand-1 form)))
