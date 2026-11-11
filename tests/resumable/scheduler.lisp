(in-package :cl-user)
(defvar *scheduled-test-value* :global)

;; Export real Lisp closures for the asynchronous JS verifier. Each task writes
;; its own cell in one shared Lisp array; the host callback records observations.
;; Shared read-modify-write counters would require the future lock API.
(ffi:set (ffi:ref "globalThis" "valtanScheduledTestSetup")
         (lambda ()
           (let ((shared (make-array 64 :initial-element 0)) (cleaned 0))
             (ffi:object
              "run" (lambda (id record)
                      (let ((*scheduled-test-value* id))
                        (dotimes (round 3)
                          (incf (aref shared id))
                          (funcall record id round *scheduled-test-value*)
                          (system::%resumable-yield nil)
                          (assert (= *scheduled-test-value* id)))
                        (values id (aref shared id))))
              "cpu" (lambda (id record)
                      (let ((*scheduled-test-value* id))
                        ;; No explicit yield: only generated checkpoints allow
                        ;; another task to run while this loop is in progress.
                        (dotimes (round 8)
                          (incf (aref shared id))
                          (funcall record id round *scheduled-test-value*))
                        (values id (aref shared id))))
              "snapshot" (lambda () (ffi:cl->js shared))
              "global" (lambda () *scheduled-test-value*)
              "await" (lambda (promise)
                        (let ((*scheduled-test-value* :waiting))
                          (unwind-protect
                               (values (system::%resumable-await promise) :awaited)
                            (system::%resumable-await
                             ((ffi:ref "Promise" "resolve") :cleanup))
                            (assert (eq *scheduled-test-value* :waiting))
                            (incf cleaned))))
              "cleaned" (lambda () cleaned)))))
;; The module namespace is the same one captured by generated Lisp functions.
(ffi:set (ffi:ref "globalThis" "valtanSchedulerKernel") (ffi:ref "lisp"))
