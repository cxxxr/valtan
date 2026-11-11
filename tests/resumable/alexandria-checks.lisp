(in-package :cl-user)
;; Run after performance/resumable.lisp and resumable/checks.lisp, with the
;; real Quicklisp Alexandria loaded using :VALTAN-RESUMABLE target features.
(let ((task (make-resumable-test-task
             (compile nil '(lambda ()
                             (alexandria:mappend
                              (lambda (value)
                                (system::%resumable-yield value)
                                (list value value))
                              '(1 2)))))))
  (dual-check (= (resumable-test-yield (dual-step task)) 1))
  (dual-check (= (resumable-test-yield (dual-step task)) 2))
  (dual-check (resumable-test-done (dual-step task)))
  (dual-check (equal (resumable-test-values task) '((1 1 2 2)))))
(assert (= *dual-checks* 55))
(write-line "RESUMABLE-ALEXANDRIA-CHECKS-PASS 4")
