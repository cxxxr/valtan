(in-package :common-lisp)

(defvar *handlers* '())

(defun coerce-to-condition (datum arguments default-condition)
  (if (or (stringp datum) (functionp datum))
      (make-condition default-condition
                      :format-control datum
                      :format-arguments arguments)
      (apply #'make-condition datum arguments)))

(defun signal-1 (condition)
  (dolist (handler *handlers*)
    (when (subclassp (car handler) condition)
      (funcall (cdr handler) condition))))

(defun signal (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-condition)))
    (signal-1 condition)))

(defun invoke-debugger (condition)
  (system::error
   (ffi:cl->js (apply #'format nil
                      (simple-condition-format-control condition)
                      (simple-condition-format-arguments condition)))))

(defun error (datum &rest arguments)
  (if (not (fboundp 'make-instance))
      (system::error (ffi:cl->js (apply #'format nil datum arguments)))
      (let ((condition (coerce-to-condition datum arguments 'simple-error)))
        (signal-1 condition)
        (invoke-debugger condition))))

(defmacro assert (test-form &optional place datum-form argument-form)
  (declare (ignore place datum-form argument-form))
  `(unless ,test-form
     (error ,(format nil "assertion error: ~S" test-form))))

(defun type-error (value type-name)
  (error "The value ~S is not of the expected type ~A" value type-name))

(defun eof-error ()
  (error "End of file"))
