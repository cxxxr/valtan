#+valtan
(in-package :common-lisp)
#-valtan
(in-package :valtan-core)

(defvar *handler-clusters* '())

(declaim (ftype function make-condition class-of find-class subclassp))
(defun coerce-to-condition (datum arguments default-condition)
  (cond ((or (cl:stringp datum) (functionp datum))
         (make-condition default-condition
                         :format-control datum
                         :format-arguments arguments))
        ((and (*:structure-p datum)
              (eq (*:%structure-name datum)
                  'standard-instance)
              (subclassp (class-of datum) (find-class 'base-condition)))
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
  (*:error (ffi:cl->js (princ-to-string condition))))

(defvar *in-error* nil)

(declaim (ftype function format))
(defun error (datum &rest arguments)
  (if (not (fboundp 'make-instance))
      (*:error (ffi:cl->js (apply #'format nil (princ-to-string datum) arguments)))
      (let ((*in-error* t)
            (condition (coerce-to-condition datum arguments 'simple-error)))
        (signal-1 condition)
        (invoke-debugger condition))))

(defmacro assert (test-form &optional place (datum nil datum-p) &rest arguments)
  (declare (ignore place))
  `(unless ,test-form
     (error ,(if datum-p datum "assertion error: ~S")
            ,@(if datum-p arguments))))

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
    (let ((g-form-name (cl:gensym))
          (fun-names
            (cl:mapcar (lambda (arg) (declare (ignore arg)) (cl:gensym))
                       error-clauses))
          (tag-names
            (cl:mapcar (lambda (arg) (declare (ignore arg)) (cl:gensym))
                       error-clauses))
          (g-block-name (cl:gensym))
          (g-temp (cl:gensym)))
      `(flet ((,g-form-name ()
                ,form)
              ,@(cl:mapcar (lambda (c fun-name) `(,fun-name ,@(cl:cdr c))) error-clauses fun-names))
         (let ((,g-temp))
           (block ,g-block-name
             (tagbody
               (cl:handler-bind ,(cl:mapcar
                                  (lambda (c tag-name)
                                    (let ((arg (cl:gensym)))
                                      `(,(cl:car c)
                                        (lambda (,arg) (setq ,g-temp ,arg) (go ,tag-name)))))
                                  error-clauses tag-names)
                 (return-from ,g-block-name (,g-form-name)))
               ,@(cl:mapcan
                   (lambda (tag-name fun-name error-clause)
                     `(,tag-name
                       (return-from ,g-block-name
                         (,fun-name
                          ,@(if (cl:null (cl:cadr error-clause))
                                nil
                                `(,g-temp))))))
                  tag-names fun-names error-clauses))))))))

(defmacro handler-case (form &rest cases)
  (let ((error-clauses
          (remove-if (lambda (c) (eq (car c) :no-error)) cases))
        (no-error-clause
          (find-if (lambda (c) (eq (car c) :no-error)) cases)))
    (if no-error-clause
        (let ((g-error-return (cl:gensym))
              (g-normal-return (cl:gensym)))
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

#+valtan
(defun print-backtrace (&key (stream *standard-output*))
  (write-string (ffi:js->cl ((ffi:ref "lisp" "getBacktrace"))) stream))
