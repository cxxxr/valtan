(in-package :cl-user)
(defvar *resumable-value* :global)
(defvar *resumable-cleanup* nil)
(defvar *resumable-checks* 0)
(setf *resumable-checks* 0)
(defun resumable-check (value)
  (assert value () "Resumable assertion ~D failed" (1+ *resumable-checks*))
  (incf *resumable-checks*))
(defun compile-resumable-test (form)
  (let ((compiler::*resumable-functions* t))
    (multiple-value-bind (function warnings failure) (compile nil form)
      (declare (ignore warnings))
      (assert (not failure))
      function)))
(defun make-resumable-test-task (function &rest args)
  (ffi:new (ffi:ref "lisp" "ResumableTask") function (system::list-to-raw-array args)))
(defun resumable-test-step (task &optional (value (ffi:ref "undefined")) (budget 10000 budgetp))
  ;; Tests of explicit yields should not depend on how many core functions
  ;; expose checkpoints. Explicit budgets still return after exactly one slice.
  (loop for step = ((ffi:ref task "resume") value budget)
        then ((ffi:ref task "resume") (ffi:ref "undefined") budget)
        repeat 1000
        when (or budgetp
                 (not (eq (ffi:ref step "value")
                          (ffi:ref "lisp" "resumableCheckpoint"))))
          do (return-from resumable-test-step step))
  (error "Resumable test exceeded its checkpoint limit"))
(defun resumable-test-done (step) (eq (ffi:ref step "done") (ffi:ref "true")))
(defun resumable-test-yield (step) (ffi:ref (ffi:ref step "value") "value"))
(defun resumable-test-values (task) (coerce (ffi:js->cl (ffi:ref task "values")) 'list))

;; Local calls, FUNCALL, recursion, closures, shared mutation and special values
;; all cross an actual suspension point. The caller's binding stays independent.
(let* ((shared (vector 0))
       (function
         (compile-resumable-test
          '(lambda (shared label)
             (let ((*resumable-value* label))
               (labels ((pause ()
                          (incf (aref shared 0))
                          (system::%resumable-yield *resumable-value*)
                          (values *resumable-value* (aref shared 0))))
                 (funcall #'pause))))))
       (one (make-resumable-test-task function shared :one))
       (two (make-resumable-test-task function shared :two)))
  (let ((*resumable-value* :caller))
    (resumable-check (eq (resumable-test-yield (resumable-test-step one)) :one))
    (resumable-check (eq (resumable-test-yield (resumable-test-step two)) :two))
    (resumable-check (= (aref shared 0) 2))
    (resumable-check (eq *resumable-value* :caller))
    (resumable-check (resumable-test-done (resumable-test-step two)))
    (resumable-check (resumable-test-done (resumable-test-step one)))
    (resumable-check (equal (resumable-test-values one) '(:one 2)))
    (resumable-check (equal (resumable-test-values two) '(:two 2)))
    (resumable-check (eq *resumable-value* :caller))))
(resumable-check (eq *resumable-value* :global))

(let* ((function (compile-resumable-test
                  '(lambda (*resumable-value* &optional
                             (value (system::%resumable-yield :default)))
                     (values *resumable-value* value))))
       (task (make-resumable-test-task function :parameter)))
  (resumable-check (eq (resumable-test-yield (resumable-test-step task)) :default))
  (resumable-check (eq *resumable-value* :global))
  (resumable-check (resumable-test-done (resumable-test-step task 99)))
  (resumable-check (equal (resumable-test-values task) '(:parameter 99))))

;; Nonlocal return traverses recursive generator calls, retaining all values.
(let* ((function (compile-resumable-test
                  '(lambda ()
                     (block done
                       (labels ((walk (n)
                                  (if (zerop n)
                                      (return-from done (values :done 42))
                                      (progn (system::%resumable-yield n) (walk (1- n))))))
                         (walk 3))))))
       (task (make-resumable-test-task function)))
  (dolist (n '(3 2 1))
    (resumable-check (= (resumable-test-yield (resumable-test-step task)) n)))
  (resumable-check (resumable-test-done (resumable-test-step task)))
  (resumable-check (equal (resumable-test-values task) '(:done 42))))

;; Cleanup can itself suspend without losing the protected multiple values.
(setf *resumable-cleanup* nil)
(let* ((function (compile-resumable-test
                  '(lambda ()
                     (unwind-protect (values :first :second)
                       (system::%resumable-yield :cleanup)
                       (setf *resumable-cleanup* t)))))
       (task (make-resumable-test-task function)))
  (resumable-check (eq (resumable-test-yield (resumable-test-step task)) :cleanup))
  (resumable-check (not *resumable-cleanup*))
  (resumable-check (resumable-test-done (resumable-test-step task)))
  (resumable-check *resumable-cleanup*)
  (resumable-check (equal (resumable-test-values task) '(:first :second))))

;; MULTIPLE-VALUE-CALL must dispatch through the resumable function as well.
(let* ((function (compile-resumable-test
                  '(lambda ()
                     (multiple-value-call
                         (lambda (a b c)
                           (system::%resumable-yield b)
                           (values a c))
                       (values 1 2) (values) (values 3)))))
       (task (make-resumable-test-task function)))
  (resumable-check (= (resumable-test-yield (resumable-test-step task)) 2))
  (resumable-check (resumable-test-done (resumable-test-step task)))
  (resumable-check (equal (resumable-test-values task) '(1 3))))

;; Backedges yield even if the loop never calls an explicitly yielding function.
(let* ((shared (vector 0))
       (function (compile-resumable-test
                  '(lambda (shared)
                     (dotimes (i 20) (incf (aref shared 0))) (values))))
       (task (make-resumable-test-task function shared))
       (step (resumable-test-step task (ffi:ref "undefined") 3))
       (intermediate nil))
  (resumable-check (not (resumable-test-done step)))
  (loop repeat 10000 until (resumable-test-done step)
        do (when (< 0 (aref shared 0) 20) (setf intermediate t))
           (setf step (resumable-test-step task (ffi:ref "undefined") 3)))
  (resumable-check intermediate)
  (resumable-check (resumable-test-done step))
  (resumable-check (= (aref shared 0) 20))
  (resumable-check (null (resumable-test-values task))))

;; Named calls keep their late function lookup across suspension/redefinition.
(setf (symbol-function 'resumable-named-callee)
      (compile-resumable-test '(lambda () (system::%resumable-yield :old) 1)))
(let ((task (make-resumable-test-task
             (compile-resumable-test
              '(lambda () (values (resumable-named-callee) (resumable-named-callee)))))))
  (resumable-check (eq (resumable-test-yield (resumable-test-step task)) :old))
  (setf (symbol-function 'resumable-named-callee)
        (compile-resumable-test '(lambda () (system::%resumable-yield :new) 2)))
  (resumable-check (eq (resumable-test-yield (resumable-test-step task)) :new))
  (resumable-check (resumable-test-done (resumable-test-step task)))
  (resumable-check (equal (resumable-test-values task) '(1 2))))

(let ((task (make-resumable-test-task
             (compile-resumable-test
              '(lambda ()
                 (handler-case (progn (system::%resumable-yield :before-error)
                                      (error "Handled resumable error"))
                   (error () (values :caught :condition))))))))
  (resumable-check (eq (resumable-test-yield (resumable-test-step task)) :before-error))
  (resumable-check (resumable-test-done (resumable-test-step task)))
  (resumable-check (equal (resumable-test-values task) '(:caught :condition))))

(let ((task (make-resumable-test-task
             (compile-resumable-test
              '(lambda ()
                 (catch 'done
                   (system::%resumable-yield :before-throw)
                   (throw 'done (values :caught :throw))))))))
  (resumable-check (eq (resumable-test-yield (resumable-test-step task)) :before-throw))
  (resumable-check (resumable-test-done (resumable-test-step task)))
  (resumable-check (equal (resumable-test-values task) '(:caught :throw))))

;; Normal synchronous callers remain usable when no explicit suspension occurs.
(resumable-check (= (funcall (compile-resumable-test '(lambda (x) (+ x 1))) 8) 9))
(ffi:set (ffi:ref "globalThis" "valtanResumableChecksPassed") *resumable-checks*)
(format t "~&RESUMABLE-CHECKS-PASS ~D~%" *resumable-checks*)
