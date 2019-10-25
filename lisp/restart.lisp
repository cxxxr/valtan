(in-package :common-lisp)

(defvar *restarts* '())

(defstruct restart
  name
  function
  interactive-function
  report-function
  test-function)

(defun find-restart (identier &optional condition)
  (find identier *restarts* :key #'restart-name))

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
                                        :interactive-function interactive-function
                                        :report-function report-function
                                        :test-function test-function)
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
                                clauses))
         ,expression))))
