(in-package :common-lisp)

(defvar *handler-clusters* '())

(defun coerce-to-condition (datum arguments default-condition)
  (cond ((or (stringp datum) (functionp datum))
         (make-condition default-condition
                         :format-control datum
                         :format-arguments arguments))
        ((and (system::structure-p datum)
              (eq (system::%structure-name datum)
                  'standard-instance)
              (subclassp (class-of datum) (find-class 'condition)))
         (when arguments
           (error 'type-error :datum arguments :expected-type nil))
         datum)
        (t
         (apply #'make-condition datum arguments))))

(defun signal-1 (condition)
  (let ((class (class-of condition))
        (*handler-clusters* *handler-clusters*))
    (do ()
        ((null *handler-clusters*))
      (dolist (handler (pop *handler-clusters*))
        (when (subclassp class (car handler))
          (funcall (cadr handler) condition))))))

(defun signal (datum &rest arguments)
  ;; TODO: *break-on-signals*
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
  `(let ((*handler-clusters*
           (cons (list ,@(mapcar (lambda (binding)
                                   (destructuring-bind (type handler) binding
                                     `(list (find-class ',type)
                                            ,handler)))
                                 bindings))
                 *handler-clusters*)))
     ,@forms))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gen-handler-case-1 (error-clauses form)
    (let ((g-form-name (gensym "FORM-"))
          (fun-names (mapcar (lambda (arg)
                               (gensym (format nil "FUN-~A-" (car arg))))
                             error-clauses))
          (tag-names (mapcar (lambda (arg)
                               (gensym (format nil "TAG-~A-" (car arg))))
                             error-clauses))
          (g-block-name (gensym "BLOCK-"))
          (g-temp (gensym "TEMP-")))
      `(flet ((,g-form-name () ,form)
              ,@(mapcar (lambda (c fun-name)
                          `(,fun-name ,@(cdr c)))
                  error-clauses
                  fun-names))
         (let ((,g-temp))
           (block ,g-block-name
             (tagbody
               (handler-bind
                   ,(mapcar (lambda (c tag-name)
                              (let ((arg (gensym)))
                                `(,(car c)
                                  (lambda (,arg)
                                    (setq ,g-temp ,arg)
                                    (go ,tag-name)))))
                            error-clauses
                            tag-names)
                 (return-from ,g-block-name (,g-form-name)))
               ,@(mapcan (lambda (tag-name fun-name error-clause)
                           `(,tag-name
                             (return-from ,g-block-name
                               (,fun-name ,@(if (null (cadr error-clause))
                                                nil
                                                `(,g-temp))))))
                  tag-names
                  fun-names
                  error-clauses))))))))

(defmacro handler-case (form &rest cases)
  (let ((error-clauses
          (remove-if (lambda (c) (eq (car c) :no-error)) cases))
        (no-error-clause
          (find-if (lambda (c) (eq (car c) :no-error)) cases)))
    (if no-error-clause
        (let ((g-error-return (gensym "ERROR-RETURN-"))
              (g-normal-return (gensym "NORMAL-RETURN-")))
          `(block ,g-error-return
             (multiple-value-call (lambda ,@(cdr no-error-clause))
               (block ,g-normal-return
                 (return-from ,g-error-return
                   (handler-case (return-from ,g-normal-return ,form)
                     ,@error-clauses))))))
        (gen-handler-case-1 error-clauses form))))

(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))
