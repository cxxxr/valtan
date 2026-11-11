(in-package :cl-user)
(defvar *callback-checks* 0)
(setf *callback-checks* 0)
(defun callback-check (value)
  (assert value () "Native callback assertion ~D failed" (1+ *callback-checks*))
  (incf *callback-checks*))

;; Exercise registered native wrappers with compiled Lisp callbacks, preserving
;; dynamic bindings, closure mutation and the final single NIL value.
(let* ((package (make-package "RESUMABLE-CALLBACK-PACKAGE" :use nil))
       (one (intern "ONE" package))
       (two (intern "TWO" package)))
  (unwind-protect
       (dolist (name '(system:map-package-symbols system:map-package-external-symbols
                      system:map-package-all-symbols))
         (export (list one two) package)
         (let* ((function
                  (compile nil '(lambda (name package)
                                  (let ((*resumable-value* :package))
                                    (funcall name package
                                             (lambda (symbol)
                                               (system::%resumable-yield
                                                (list symbol *resumable-value*))))))))
                (task (make-resumable-test-task function name package)))
           (callback-check (equal (resumable-test-yield (dual-step task)) (list one :package)))
           (callback-check (eq *resumable-value* :global))
           (callback-check (equal (resumable-test-yield (dual-step task)) (list two :package)))
           (callback-check (resumable-test-done (dual-step task)))
           (callback-check (equal (resumable-test-values task) '(nil)))))
    (delete-package package)))

;; The native Map iterator stays live across suspension: removed future entries
;; are skipped and newly inserted entries are visited, just as with JS forEach.
(let* ((map (system:make-map))
       (function (compile nil '(lambda (map)
                                (system:map-foreach
                                 map (lambda (value key)
                                       (system::%resumable-yield (list key value)))))))
       (task (make-resumable-test-task function map)))
  (system:map-set map :one 10)
  (system:map-set map :two 20)
  (callback-check (equal (resumable-test-yield (dual-step task)) '(:one 10)))
  (system:map-remove map :two)
  (system:map-set map :three 30)
  (callback-check (equal (resumable-test-yield (dual-step task)) '(:three 30)))
  (callback-check (resumable-test-done (dual-step task)))
  (callback-check (equal (resumable-test-values task) '(nil))))

;; Weak maps expose the same entries protocol; keep the key alive explicitly.
(let* ((map (system:weak-op #j"map" #j"key" #j"eq"))
       (key (list :key))
       (function (compile nil '(lambda (map)
                                (system:map-foreach
                                 map (lambda (value key)
                                       (system::%resumable-yield (list key value)))))))
       (task (make-resumable-test-task function map)))
  (system:map-set map key :value)
  (let ((pair (resumable-test-yield (dual-step task))))
    (callback-check (eq (first pair) key))
    (callback-check (eq (second pair) :value)))
  (callback-check (resumable-test-done (dual-step task)))
  (callback-check (equal (resumable-test-values task) '(nil))))

(format t "~&NATIVE-CALLBACK-CHECKS-PASS ~D~%" *callback-checks*)
(ffi:set (ffi:ref "globalThis" "valtanCallbackChecksPassed") *callback-checks*)
