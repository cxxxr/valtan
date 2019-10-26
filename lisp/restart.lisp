(in-package :common-lisp)

(defvar *restarts* '())

(defstruct restart
  name
  function
  interactive-function
  report-function
  test-function
  (associated-conditions '()))

(defun map-restarts (function condition)
  (dolist (restart *restarts*)
    (when (or (null condition)
              (null (restart-associated-conditions restart))
              (member condition (restart-associated-conditions restart)))
      (funcall function restart))))

(defun compute-restarts (&optional condition)
  (with-accumulate ()
    (map-restarts (lambda (restart)
                    (collect restart))
                  condition)))

(defun find-restart (identier &optional condition)
  (map-restarts (lambda (restart)
                  (when (eq identier (restart-name restart))
                    (return-from find-restart restart)))
                condition))

(defun invoke-restart (restart-name &rest values)
  (let ((restart (find-restart restart-name)))
    (unless restart
      (error 'control-error
             :format-control "No restart ~S is active"
             :format-arguments (list restart-name)))
    (apply (restart-function restart) values)))

(defmacro restart-bind (bindings &body forms)
  `(let ((*restarts* *restarts*))
     ,@(mapcar (lambda (binding)
                 (destructuring-bind
                       (name function &key interactive-function report-function test-function)
                     binding
                   `(push (make-restart :name ',name
                                        :function ,function
                                        :interactive-function ,interactive-function
                                        :report-function ,report-function
                                        :test-function ,test-function)
                          *restarts*)))
               (reverse bindings))
     ,@forms))

(defmacro restart-case (expression &body clauses)
  (labels ((ensure-interactive-function (interactive)
             `(function ,interactive))
           (ensure-report-function (report)
             (if (stringp report)
                 `(lambda (stream) (write-string ,report stream))
                 `(function ,report)))
           (ensure-test-function (test)
             (if (and (consp test)
                      (eq 'function (car test)))
                 test
                 `(function ,test)))
           (parse-clause-1 (clause)
             (case (car clause)
               (:interactive
                (values (list :interactive-function
                              (ensure-interactive-function (cadr clause)))
                        (cddr clause)))
               (:report
                (values (list :report-function
                              (ensure-report-function (cadr clause)))
                        (cddr clause)))
               (:test
                (values (list :test-function
                              (ensure-test-function (cadr clause)))
                        (cddr clause)))
               (otherwise
                (values nil
                        clause))))
           (parse-clause (clause options)
             (multiple-value-bind (option clause)
                 (parse-clause-1 clause)
               (cond ((null option)
                      (values clause options))
                     (t
                      (parse-clause clause
                                    (nconc option options)))))))
    (let ((outer-block-name (gensym)))
      `(block ,outer-block-name
         (restart-bind ,(mapcar (lambda (clause)
                                  (destructuring-bind (case-name lambda-list . clause) clause
                                    (multiple-value-bind (clause options)
                                        (parse-clause clause nil)
                                      (multiple-value-bind (forms declares)
                                          (compiler::parse-body clause nil)
                                        `(,case-name (lambda ,lambda-list
                                                       (declare ,@declares)
                                                       (return-from ,outer-block-name
                                                         (progn ,@forms)))
                                                     ,@options)))))
                                clauses)
           ,expression)))))

(defmacro with-simple-restart ((restart-name format-control &rest format-arguments) &body forms)
  (let ((g-stream (gensym)))
    `(restart-case (progn ,@forms)
       (,restart-name ()
         :report (lambda (,g-stream)
                   (format ,g-stream ,format-control ,@format-arguments)))
       (values nil t))))

(defmacro with-condition-restarts (condition-form restarts-form &body body)
  (let ((condition (gensym))
        (restarts (gensym))
        (restart (gensym)))
    `(let ((,condition ,condition-form)
           (,restarts ,restarts-form))
       (unwind-protect
            (progn
              (dolist (,restart ,restarts)
                (push ,condition (restart-associated-conditions ,restart)))
              ,@body)
         (dolist (,restart ,restarts)
           (pop (restart-associated-conditions ,restart)))))))

(defun cerror (continue-format-control datum &rest arguments)
  (with-simple-restart (continue "~A" (apply #'format nil continue-format-string arguments))
    (let ((condition (coerce-to-condition datum arguments 'simple-error)))
      (with-condition-restarts condition (list (find-restart 'continue))
        (signal-1 condition)
        (invoke-debugger condition))))
  nil)

(defun warn (datum &rest arguments)
  (let ((condition (coerce-to-condition datum arguments 'simple-warning)))
    (restart-case (signal condition)
      (muffle-warning ()
        :report "Skip warning."
        (return-from warn nil)))
    (format *error-output* "~&Warning: ~A~%" condition)
    nil))