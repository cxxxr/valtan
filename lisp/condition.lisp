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
    (when (subclassp (class-of condition) (car handler))
      (funcall (cdr handler) condition))))

(defun signal (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-condition)))
    (signal-1 condition)))

(defun invoke-debugger (condition)
  (system::error
   (ffi:cl->js (apply #'format nil
                      (simple-condition-format-control condition)
                      (simple-condition-format-arguments condition)))))

(defvar *in-error* nil)

(defun error (datum &rest arguments)
  (if (or *in-error*
          (not (fboundp 'make-instance)))
      (system::error (ffi:cl->js (apply #'format nil datum arguments)))
      (let ((*in-error* t))
        (let ((condition (coerce-to-condition datum arguments 'simple-error)))
          (signal-1 condition)
          (invoke-debugger condition)))))

(defmacro assert (test-form &optional place datum-form argument-form)
  (declare (ignore place datum-form argument-form))
  `(unless ,test-form
     (error ,(format nil "assertion error: ~S" test-form))))

(defun type-error (value type-name)
  (error "The value ~S is not of the expected type ~A" value type-name))

(defun eof-error ()
  (error "End of file"))

(defmacro handler-bind (bindings &body forms)
  `(let ((*handlers* *handlers*))
     ,@(mapcar (lambda (binding)
                 `(push (cons (find-class ',(car binding)) ,(cadr binding)) *handlers*))
               (reverse bindings))
     ,@forms))
