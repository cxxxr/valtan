(in-package :common-lisp)

(defvar *handlers* '())

(defun coerce-to-condition (datum arguments default-condition)
  (if (or (stringp datum) (functionp datum))
      (make-condition default-condition
                      :format-control datum
                      :format-arguments arguments)
      (apply #'make-condition datum arguments)))

(defun signal-1 (condition)
  (let ((class (class-of condition)))
    (dolist (handler *handlers*)
      (when (subclassp class (car handler))
        (funcall (cdr handler) condition)))))

(defun signal (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-condition)))
    (signal-1 condition)))

(defun invoke-debugger (condition)
  (system::error (ffi:cl->js (princ-to-string condition))))

(defvar *in-error* nil)

(defun error (datum &rest arguments)
  (if (not (fboundp 'make-instance))
      (system::error (ffi:cl->js (apply #'format nil (princ-to-string datum) arguments)))
      (let ((*in-error* t)
            (condition (coerce-to-condition datum arguments 'simple-error)))
        (signal-1 condition)
        (invoke-debugger condition))))

(defmacro assert (test-form &optional place datum-form argument-form)
  (declare (ignore place datum-form argument-form))
  `(unless ,test-form
     (error ,(format nil "assertion error: ~S" test-form))))

(defun simple-error (string &rest arguments)
  (apply #'error string arguments))

(defun program-error (string &rest arguments)
  (error 'program-error
         :format-control string
         :format-arguments arguments))

(defun undefined-function (name)
  (error 'undefined-function :name name))

(defun unbound-variable (name)
  (error 'unbound-variable :name name))

(defun arguments-error (name num-args)
  (error 'program-error
         :format-control "invalid number of arguments for ~A: ~A"
         :format-arguments (list name num-args)))

(defun type-error (datum expected-type)
  (error 'type-error :datum datum :expected-type expected-type))

(defun eof-error ()
  (error "End of file"))

(defmacro handler-bind (bindings &body forms)
  `(let ((*handlers* *handlers*))
     ,@(mapcar (lambda (binding)
                 `(push (cons (find-class ',(car binding)) ,(cadr binding)) *handlers*))
               (reverse bindings))
     ,@forms))

(defmacro handler-case (form &rest cases)
  (let ((error-clauses
          (remove-if (lambda (c) (eq (car c) :no-error)) cases))
        (no-error-clause
          (find-if (lambda (c) (eq (car c) :no-error)) cases))
        (g-condition (gensym))
        (g-name (gensym)))
    `(block ,g-name
       (handler-bind
           ,(mapcar (lambda (c)
                      `(,(car c)
                        ,(if (consp (cadr c))
                             `(lambda ,(cadr c)
                                (return-from ,g-name
                                  (progn ,@(cddr c))))
                             `(lambda (,g-condition)
                                (declare (ignore ,g-condition))
                                (return-from ,g-name
                                  (progn ,@(cdr c)))))))
                    error-clauses)
         ,(if no-error-clause
              (destructuring-bind (args . body)
                  (cdr no-error-clause)
                `(multiple-value-call (lambda ,args ,@body) ,form))
              form)))))
